--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;

with RP.DMA;
with RP.Device;

with Net.Headers;
with Net.Protos.IPv4;

with Enet_Config;

with Enet_Picowi.DMA_Interrupt;

package body Enet_Picowi.Generic_Interface is

   pragma Compile_Time_Error
     (Enet_Config.Headroom < 22,
      "enet.Headroom should be at least 22 (bus prefix and SDPCM header)");

   pragma Compile_Time_Error
     ((Enet_Config.Headroom - SDPCM.Data_Header_Length) mod 4 /= 0,
      "enet.Headroom - 18 should be a multiple of 4 for word aligned frames");

   Rx_Offset : constant := SDPCM.Data_Header_Length;
   --  A received frame is placed Rx_Offset bytes before the Ethernet header,
   --  so the Ethernet header is in place if the frame header has the usual
   --  length.

   function Tx_Offset return Positive is
     (Picowi.PIO_SPI.gSPI.Word'Length + SDPCM.Data_Header_Length);
   --  A frame to send is placed Tx_Offset bytes before the Ethernet header,
   --  so the driver doesn't move the data to put the headers.

   function Buffer_Length (Offset : Positive) return Positive is
     (Positive (Net.Buffers.NET_BUF_SIZE) + Offset);
   --  Length of the buffer starting Offset bytes before the Ethernet header

   procedure Wait_Transfer;
   --  Wait for the end of asynchronous transfer, if any, and complete it

   type Uint16_Array is array (Positive range <>) of Net.Uint16;

   function Get_Ip_Sum (Raw : Uint16_Array) return Net.Uint16;

   procedure Set_Check_Sum (Packet : Net.Buffers.Buffer_Type)
     with Pre => not Net.Buffers.Is_Null (Packet);

   ----------------
   -- Get_Ip_Sum --
   ----------------

   function Get_Ip_Sum (Raw : Uint16_Array) return Net.Uint16 is
      use type Net.Uint16;
      use type Net.Uint32;
      Sum : Net.Uint32 := 0;
   begin
      for Item of Raw loop
         Sum := Sum + Net.Uint32 (Item);
      end loop;

      while Sum >= 16#1_0000# loop
         Sum := Sum - 16#1_0000# + 1;
      end loop;

      return not Net.Uint16 (Sum and 16#FFFF#);
   end Get_Ip_Sum;


   ------------------
   -- Buffer_Queue --
   ------------------

   protected body Buffer_Queue is

      ---------
      -- Get --
      ---------

      procedure Get (Packet : in out Net.Buffers.Buffer_Type) is
      begin
         if not Net.Buffers.Is_Empty (List) then
            Net.Buffers.Peek (List, Packet);
            Count := Count - 1;
         end if;
      end Get;

      ---------
      -- Put --
      ---------

      procedure Put
        (Packet : in out Net.Buffers.Buffer_Type;
         Done   : out Boolean) is
      begin
         Done := Count < Send_Queue_Length;

         if Done then
            Net.Buffers.Insert (List, Packet);
            Count := Count + 1;
         end if;
      end Put;

   end Buffer_Queue;

   ------------
   -- Create --
   ------------

   procedure Create (Self : in out Ifnet'Class) is
      Ok : Boolean ;
   begin
      Picowi.PIO_SPI.Configure_GPIO (Power_On => True);

      for J in 1 .. 4 loop
         RP.Device.Timer.Delay_Milliseconds (2);
         Picowi.PIO_SPI.gSPI.Detect_Chip (Ok);
         exit when Ok;
      end loop;

      pragma Assert (Ok);

      Picowi.PIO_SPI.gSPI.Switch_Endian (Ok);
      pragma Assert (Ok);

      RP.DMA.Enable_IRQ (Picowi.PIO_SPI.DMA_Channel, 0);
   end Create;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      Done : Boolean;
   begin
      Self.Send_Queue.Put (Packet, Done);

      if not Done then
         --  The queue is full, drop the packet like a NIC with full TX ring
         Net.Buffers.Release (Packet);
      end if;
   end Send;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      use type Net.Uint8;
      Output   : Net.Buffers.Buffer_Type;
      Action   : SDPCM_IO.Action;
      Can_Send : Boolean := False;
   begin
      loop
         if Can_Send then
            Can_Send := False;
            Self.Send_Queue.Get (Output);

            if Net.Buffers.Is_Null (Output) then
               RP.Device.Timer.Delay_Milliseconds (1);
            end if;
         end if;

         if Net.Buffers.Is_Null (Output) then
            declare
               Raw : SDPCM.Buffer_Byte_Array (1 .. Buffer_Length (Rx_Offset))
                 with Import,
                   Address => Packet.Get_Buffer_Address (Rx_Offset);
            begin
               SDPCM_IO.Poll (Self.State, Raw, Action => Action);
            end;
         else
            Set_Check_Sum (Output);

            declare
               Size : constant Positive := Positive (Output.Get_Length);
               Raw  : SDPCM.Buffer_Byte_Array (1 .. Buffer_Length (Tx_Offset))
                 with Import,
                   Address => Output.Get_Buffer_Address
                                (Net.Uint16 (Tx_Offset));
            begin
               --  The headers go to the reserved space before the data
               SDPCM_IO.Poll
                 (Self.State,
                  Raw,
                  Send   => (From => Tx_Offset + 1, To => Tx_Offset + Size),
                  Action => Action);
            end;
         end if;

         case Action.Kind is
            when SDPCM_IO.Idle =>
               Can_Send := True;

               if Self.Mac (1) = 0 then
                  Self.Mac := Net.Ether_Addr (SDPCM_IO.Get_MAC (Self.State));
               end if;
            when SDPCM_IO.Continue =>
               null;
            when SDPCM_IO.Sleep =>
               RP.Device.Timer.Delay_Milliseconds (Action.Milliseconds);
            when SDPCM_IO.Complete_IO =>
               Wait_Transfer;
               --  Release the sent packet, if any.
               Net.Buffers.Release (Output);
            when SDPCM_IO.Process_Packet =>
               declare
                  From : constant Positive := Action.Span.From;
                  To   : constant Positive := Action.Span.To;
                  Raw  : SDPCM.Buffer_Byte_Array
                    (1 .. Buffer_Length (Rx_Offset))
                      with Import,
                        Address => Packet.Get_Buffer_Address (Rx_Offset);
               begin
                  if From /= Rx_Offset + 1 then
                     --  Unusual header length, move the Ethernet frame
                     Raw (Rx_Offset + 1 .. Rx_Offset + To - From + 1) :=
                       Raw (From .. To);
                  end if;

                  Packet.Set_Length (Net.Uint16 (To - From + 1));

                  exit;
               end;
         end case;
      end loop;
   end Receive;

   -------------------
   -- Set_Check_Sum --
   -------------------

   procedure Set_Check_Sum (Packet : Net.Buffers.Buffer_Type) is
      use type Net.Uint16;
      Addr   : constant System.Address := Packet.Get_Data_Address;
      Ether  : Net.Headers.Ether_Header renames
        Packet.Ethernet.all;
      Ip     : Net.Headers.IP_Header renames Packet.IP.all;
      Ip_Raw : Uint16_Array (1 .. 10)
        with Import, Address => Ip'Address;
      Size   : Natural := Natural (Packet.Get_Length);
   begin
      if Ether.Ether_Type =
        Net.Headers.To_Network (Net.Protos.ETHERTYPE_IP)
      then
         --  IPv4 header checksum offload
         Ip.Ip_Sum := 0;
         Ip.Ip_Sum := Get_Ip_Sum (Ip_Raw);

         case Ip.Ip_P is
            when Net.Protos.IPv4.P_ICMP =>
               declare
                  Size     : constant Positive := Positive
                    (Packet.Get_Data_Size (Net.Buffers.IP_PACKET))
                      / 2;

                  ICMP     : Net.Headers.ICMP_Header renames
                    Packet.ICMP.all;

                  ICMP_Raw : Uint16_Array (1 .. Size)
                    with Import, Address => ICMP'Address;
               begin
                  Packet.ICMP.Icmp_Checksum := 0;
                  Packet.ICMP.Icmp_Checksum := Get_Ip_Sum (ICMP_Raw);
               end;

            when Net.Protos.IPv4.P_UDP =>
               Packet.UDP.Uh_Sum := 0;

            when others =>
               null;
         end case;
      end if;
   end Set_Check_Sum;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer is
   begin
      if Picowi.PIO_SPI.gSPI.Is_Transfer_Pending then
         Enet_Picowi.DMA_Interrupt.Wait;
         Picowi.PIO_SPI.gSPI.Complete_Transfer;
      end if;
   end Wait_Transfer;

end Enet_Picowi.Generic_Interface;
