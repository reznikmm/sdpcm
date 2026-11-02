--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Synchronous_Task_Control;

with A0B.Callbacks.Generic_Parameterless;

with Net.Headers;
with Net.Protos.IPv4;

with STM32.GPIO;
with STM32.Polling.SDIO;
with STM32.SDIO.DMA_SDIO;

package body Enet_BCM43362.Generic_Interface is

   use type Interfaces.Unsigned_32;
   use type STM32.SDIO.Error_Kind;

   package SDIO renames STM32.Polling.SDIO;
   --  Synchronous commands: CMD52 and short CMD53

   package DMA is new STM32.SDIO.DMA_SDIO (DMA_Priority);
   --  Asynchronous CMD53 for WLAN frames. Never used together with SDIO:
   --  Generic_IO doesn't call the bus until the transfer is completed.

   procedure On_Transfer_Done;
   --  DMA transfer completion callback

   package Transfer_Done_Callbacks is
     new A0B.Callbacks.Generic_Parameterless (On_Transfer_Done);

   Transfer_Done : Ada.Synchronous_Task_Control.Suspension_Object;
   --  Set by On_Transfer_Done, the receiver task waits on it

   procedure Wait_Transfer;
   --  Wait for the end of asynchronous transfer, if any, and complete it

   CCCR_IO_Abort       : constant := 16#06#;
   IO_Abort_Reset      : constant := 16#08#;  --  RES bit

   Backplane_Window    : constant := 16#1000A#;
   --  Three bytes: bits 15 .. 8, 23 .. 16, 31 .. 24 of backplane address
   Backplane_Word_Area : constant := 16#8000#;
   --  Offset of 32-bit access area in the backplane window
   Chipcommon_Base     : constant := 16#1800_0000#;
   --  Chip id register is the first register of the chipcommon core
   Chip_Id_BCM43362    : constant := 16#A962#;

   Data_Timeout        : constant := 2_400_000;  --  about 100 ms at 24 MHz

   type Uint16_Array is array (Positive range <>) of Net.Uint16;

   function Get_Ip_Sum (Raw : Uint16_Array) return Net.Uint16;
   --  Internet checksum of Raw

   procedure Set_Check_Sum (Packet : Net.Buffers.Buffer_Type)
     with Pre => not Net.Buffers.Is_Null (Packet);
   --  ENet stack expects the checksum offload of the MAC, so calculate IPv4
   --  and ICMP checksums here

   procedure Delay_Milliseconds (Milliseconds : Natural);

   procedure Reset_Chip;
   --  Pulse the reset pin of the WiFi chip

   procedure Reset_IO;
   --  Write RES bit in CCCR I/O abort register. It resets the I/O part of
   --  the card (bus width, enabled functions) if the card has not been
   --  powered off since the previous run. A card after power-on doesn't
   --  respond, that's fine.

   procedure Probe_SDIO_Card (Success : out Boolean);
   --  CMD0 + CMD5 (IO_SEND_OP_COND), CMD3 (SEND_RELATIVE_ADDR), CMD7
   --  (SELECT_CARD). Return Success = False on failure.

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

   procedure Create (Self : in out Ifnet'Class; Success : out Boolean) is
      pragma Unreferenced (Self);
      Chip_Id : Interfaces.Unsigned_32;
   begin
      Reset_Chip;

      SDIO.Configure
        (Clock_Pin   => Clock_Pin,
         Command_Pin => Command_Pin,
         Data_Pins   => Data_Pins,
         Speed       => 400_000);

      Reset_IO;
      Probe_SDIO_Card (Success);

      if Success then
         SDIO_Bus.Initialize (Success);
      end if;

      if Success then
         SDIO_Bus.Switch_To_Four_Bit_Bus (Success);
      end if;

      if Success then
         SDIO.Set_Bus (Speed => Bus_Speed, Width => 4);

         SDIO_Bus.Write_Backplane_Register
           (Backplane_Window, 3, Chipcommon_Base / 256);
         SDIO_Bus.Read_Backplane_Register (Backplane_Word_Area, 4, Chip_Id);

         Success := (Chip_Id and 16#FFFF#) = Chip_Id_BCM43362
           and not SDIO_Bus.Has_Failed;
      end if;
   end Create;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Milliseconds : Natural) is
   begin
      delay until
        Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Milliseconds);
   end Delay_Milliseconds;

   ----------------
   -- Get_Ip_Sum --
   ----------------

   function Get_Ip_Sum (Raw : Uint16_Array) return Net.Uint16 is
      use type Net.Uint16;
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

   ---------------------
   -- Probe_SDIO_Card --
   ---------------------

   procedure Probe_SDIO_Card (Success : out Boolean) is
      Error : STM32.SDIO.Error_Kind := STM32.SDIO.Command_Timeout;
      Relative_Card_Address : Interfaces.Unsigned_32;
   begin
      for Attempt in 1 .. 100 loop
         SDIO.Send_Command
           ((Command => 0, Argument => 0, Response => STM32.SDIO.No_Response),
            Error);

         SDIO.Send_Command
           ((Command  => 5,
             Argument => 0,
             Response => STM32.SDIO.Short_Response_Without_CRC),
            Error);

         exit when Error = STM32.SDIO.No_Error;
         Delay_Milliseconds (1);
      end loop;

      Success := Error = STM32.SDIO.No_Error;

      if Success then
         --  CMD3 (SEND_RELATIVE_ADDR), R6
         SDIO.Send_Command
           ((Command => 3, Argument => 0, others => <>), Error);

         Success := Error = STM32.SDIO.No_Error;
      end if;

      if Success then
         Relative_Card_Address := SDIO.Short_Response / 2 ** 16;

         --  CMD7 (SELECT_CARD), R1b
         SDIO.Send_Command
           ((Command  => 7,
             Argument => Relative_Card_Address * 2 ** 16,
             others   => <>),
            Error);

         Success := Error = STM32.SDIO.No_Error;
      end if;
   end Probe_SDIO_Card;

   ----------------------
   -- On_Transfer_Done --
   ----------------------

   procedure On_Transfer_Done is
   begin
      Ada.Synchronous_Task_Control.Set_True (Transfer_Done);
   end On_Transfer_Done;

   -----------------------
   -- Read_Write_Direct --
   -----------------------

   procedure Read_Write_Direct
     (Argument : Interfaces.Unsigned_32;
      Response : out Interfaces.Unsigned_32;
      Success  : out Boolean)
   is
      Error : STM32.SDIO.Error_Kind;
   begin
      pragma Assert (not DMA.Is_Busy);

      SDIO.Send_Command
        ((Command  => 52,
          Argument => Argument,
          Response => STM32.SDIO.Short_Response),
         Error);

      Response := SDIO.Short_Response;
      Success := Error = STM32.SDIO.No_Error;
   end Read_Write_Direct;

   -------------------------
   -- Read_Write_Extended --
   -------------------------

   procedure Read_Write_Extended
     (Argument   : Interfaces.Unsigned_32;
      Block_Size : Positive;
      Length     : Positive;
      Data       : System.Address;
      Response   : out Interfaces.Unsigned_32;
      Success    : out Boolean)
   is
      Error : STM32.SDIO.Error_Kind;
   begin
      pragma Assert (not DMA.Is_Busy);

      SDIO.Send_Command
        (Item   =>
           (Command  => 53,
            Argument => Argument,
            Response => STM32.SDIO.Short_Response),
         Data   =>
           (Direction  =>
                (if (Argument and 2 ** 31) /= 0 then STM32.SDIO.Host_To_Card
                 else STM32.SDIO.Card_To_Host),
            Block_Size => STM32.SDIO.Block_Length (Block_Size),
            Length     => Length,
            Timeout    => Data_Timeout),
         Buffer => Data,
         Error  => Error);

      Response := SDIO.Short_Response;
      Success := Error = STM32.SDIO.No_Error;
   end Read_Write_Extended;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      Output   : Net.Buffers.Buffer_Type;
      Action   : SDPCM_IO.Action;
      Can_Send : Boolean := False;
   begin
      loop
         if Can_Send then
            Can_Send := False;
            Self.Send_Queue.Get (Output);

            if Net.Buffers.Is_Null (Output) then
               Delay_Milliseconds (1);
            end if;
         end if;

         if Net.Buffers.Is_Null (Output) then
            declare
               Raw : SDPCM.Buffer_Byte_Array
                 (1 .. Net.Buffers.Data_Type'Length)
                   with Import, Address => Packet.Get_Data_Address;
            begin
               SDPCM_IO.Poll (Self.State, Raw, Action => Action);
            end;
         else
            Set_Check_Sum (Output);

            declare
               Size : constant Positive := Positive (Output.Get_Length);
               Raw  : SDPCM.Buffer_Byte_Array
                 (1 .. Net.Buffers.Data_Type'Length)
                   with Import, Address => Output.Get_Data_Address;
            begin
               SDPCM_IO.Poll
                 (Self.State,
                  Raw,
                  Send   => (From => 1, To => Size),
                  Action => Action);
            end;
         end if;

         case Action.Kind is
            when SDPCM_IO.Idle =>
               Can_Send := True;

               if not Self.Has_MAC then
                  Self.Mac := Net.Ether_Addr (SDPCM_IO.Get_MAC (Self.State));
                  Self.Has_MAC := True;
               end if;

            when SDPCM_IO.Continue =>
               null;

            when SDPCM_IO.Sleep =>
               Delay_Milliseconds (Action.Milliseconds);

            when SDPCM_IO.Complete_IO =>
               Wait_Transfer;
               Net.Buffers.Release (Output);

            when SDPCM_IO.Process_Packet =>
               declare
                  From : constant Positive := Action.Span.From;
                  To   : constant Positive := Action.Span.To;
                  Raw  : SDPCM.Buffer_Byte_Array
                    (1 .. Net.Buffers.Data_Type'Length)
                      with Import, Address => Packet.Get_Data_Address;
               begin
                  Raw (1 .. To - From + 1) := Raw (From .. To);
                  Packet.Set_Length (Net.Uint16 (To - From + 1));

                  exit;
               end;
         end case;
      end loop;
   end Receive;

   ----------------
   -- Reset_Chip --
   ----------------

   procedure Reset_Chip is
   begin
      STM32.GPIO.Configure_Output (Reset_Pin);
      STM32.GPIO.Set_Output (Reset_Pin, 0);
      Delay_Milliseconds (1);
      STM32.GPIO.Set_Output (Reset_Pin, 1);
      Delay_Milliseconds (1);
   end Reset_Chip;

   --------------
   -- Reset_IO --
   --------------

   procedure Reset_IO is
      Ignore_Response : Interfaces.Unsigned_32;
      Ignore_Success  : Boolean;
   begin
      Read_Write_Direct
        (Argument => 2 ** 31 + CCCR_IO_Abort * 2 ** 9 + IO_Abort_Reset,
         Response => Ignore_Response,
         Success  => Ignore_Success);

      Delay_Milliseconds (20);  --  The chip needs time to react
   end Reset_IO;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type) is
   begin
      Self.Send_Queue.Put (Packet);
   end Send;

   -------------------
   -- Set_Check_Sum --
   -------------------

   procedure Set_Check_Sum (Packet : Net.Buffers.Buffer_Type) is
      use type Net.Uint16;
      Ether  : Net.Headers.Ether_Header renames Packet.Ethernet.all;
      Ip     : Net.Headers.IP_Header renames Packet.IP.all;
      Ip_Raw : Uint16_Array (1 .. 10)
        with Import, Address => Ip'Address;
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
                  ICMP.Icmp_Checksum := 0;
                  ICMP.Icmp_Checksum := Get_Ip_Sum (ICMP_Raw);
               end;

            when Net.Protos.IPv4.P_UDP =>
               Packet.UDP.Uh_Sum := 0;

            when others =>
               null;
         end case;
      end if;
   end Set_Check_Sum;

   -------------------------------
   -- Start_Read_Write_Extended --
   -------------------------------

   procedure Start_Read_Write_Extended
     (Argument   : Interfaces.Unsigned_32;
      Block_Size : Positive;
      Length     : Positive;
      Data       : System.Address) is
   begin
      Ada.Synchronous_Task_Control.Set_False (Transfer_Done);

      DMA.Start_Command
        (Item   =>
           (Command  => 53,
            Argument => Argument,
            Response => STM32.SDIO.Short_Response),
         Data   =>
           (Direction  =>
                (if (Argument and 2 ** 31) /= 0 then STM32.SDIO.Host_To_Card
                 else STM32.SDIO.Card_To_Host),
            Block_Size => STM32.SDIO.Block_Length (Block_Size),
            Length     => Length,
            Timeout    => Data_Timeout),
         Buffer => Data,
         Done   => Transfer_Done_Callbacks.Create_Callback);
   end Start_Read_Write_Extended;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Milliseconds : Positive) is
   begin
      Delay_Milliseconds (Milliseconds);
   end Sleep;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer is
   begin
      if SDIO_Bus.Is_Transfer_Pending then
         Ada.Synchronous_Task_Control.Suspend_Until_True (Transfer_Done);
         SDIO_Bus.Complete_Transfer (DMA.Short_Response, not DMA.Has_Error);
      end if;
   end Wait_Transfer;

end Enet_BCM43362.Generic_Interface;
