--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;
with RP.Device;
with Net.Headers;
with Net.Protos.IPv4;

package body Enet_Picowi.Generic_Interface is

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
         end if;
      end Get;

      ---------
      -- Put --
      ---------

      procedure Put (Packet : in out Net.Buffers.Buffer_Type) is
      begin
         Net.Buffers.Insert (List, Packet);
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
   end Create;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type) is
   begin
      Self.Send_Queue.Put (Packet);
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
               Addr : constant System.Address := Packet.Get_Data_Address;
               From : Positive := 1;
               To   : Natural := 0;
               Raw  : SDPCM.Buffer_Byte_Array
                 (1 .. Net.Buffers.Data_Type'Length)
                   with Import, Address => Addr;
            begin
               SDPCM_IO.Process
                 (Self.State,
                  Raw,
                  From,
                  To,
                  Action => Action);
            end;
         else
            declare
               Addr : constant System.Address := Output.Get_Data_Address;
               Size : Natural := Natural (Output.Get_Length);
            begin
               Set_Check_Sum (Output);

               declare
                  From : Positive := 1;
                  Raw  : SDPCM.Buffer_Byte_Array
                    (1 .. Net.Buffers.Data_Type'Length)
                      with Import, Address => Addr;
               begin
                  SDPCM_IO.Process
                    (Self.State,
                     Raw,
                     From,
                     Size,
                     Action => Action);
                  --  TBD: Complete IO then release Output buffer
               end;
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
               null;  --  We use synchronous I/O in this demo. Do nothing
               Net.Buffers.Release (Output);
            when SDPCM_IO.Process_Packet =>
               declare
                  From : constant Positive := Action.From;
                  To   : constant Positive := Action.To;
                  Addr : constant System.Address := Packet.Get_Data_Address;
                  Raw  : SDPCM.Buffer_Byte_Array
                    (1 .. Net.Buffers.Data_Type'Length)
                      with Import, Address => Addr;
               begin
                  Raw (1 .. To - From + 1) := Raw (From .. To);
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

end Enet_Picowi.Generic_Interface;
