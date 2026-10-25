--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;
with System;

with SDPCM.Generic_SDIO;

with STM32.GPIO;
with STM32.Polling.SDIO;
with STM32.Polling.USART_2;
with STM32.SDIO;

procedure EMW3165 is
   use type Interfaces.Unsigned_32;
   use type STM32.SDIO.Error_Kind;

   package UART renames STM32.Polling.USART_2;
   package SDIO renames STM32.Polling.SDIO;

   subtype Function_Number is Interfaces.Unsigned_32 range 0 .. 7;
   subtype Register_Address is Interfaces.Unsigned_32 range 0 .. 16#1_FFFF#;

   Clock_Pin   : constant STM32.Pin := (STM32.PB, 15);
   Command_Pin : constant STM32.Pin := (STM32.PA, 6);

   Data_Pins   : constant STM32.Pin_Array :=
     ((STM32.PB, 7), (STM32.PA, 8), (STM32.PA, 9), (STM32.PB, 5));

   Reset_Pin   : constant STM32.Pin := (STM32.PB, 14);
   --  Reset pin of the WiFi module (BCM43362)

   WiFi_IRQ_Pin : constant STM32.Pin := (STM32.PA, 0);

   UART_TX : constant STM32.Pin := (STM32.PA, 2);
   UART_RX : constant STM32.Pin := (STM32.PA, 3);

   R5_Error_Flags : constant := 16#CB00#;
   --  COM_CRC_ERROR, ILLEGAL_COMMAND, ERROR, FUNCTION_NUMBER, OUT_OF_RANGE

   CCCR_Revision         : constant := 16#00#;
   CCCR_Bus_Control      : constant := 16#07#;
   CCCR_Card_Capability  : constant := 16#08#;
   CCCR_CIS_Pointer      : constant := 16#09#;
   CCCR_Block_Size_Low   : constant := 16#10#;
   CCCR_Block_Size_High  : constant := 16#11#;

   CCCR_IO_Enable        : constant := 16#02#;
   CCCR_IO_Ready         : constant := 16#03#;
   CCCR_IO_Abort         : constant := 16#06#;
   FBR1_Block_Size_Low   : constant := 16#110#;
   FBR1_Block_Size_High  : constant := 16#111#;

   Backplane_Function    : constant := 1;
   Backplane_Window      : constant := 16#1000A#;
   --  Three bytes: bits 15 .. 8, 23 .. 16, 31 .. 24 of backplane address
   Backplane_Word_Access : constant := 16#8000#;
   --  Offset of 32-bit access area in the backplane window

   Chipcommon_Base       : constant := 16#1800_0000#;
   --  Chip id register is the first register of the chipcommon core

   procedure WiFi_Reset;
   procedure Reset_IO;
   --  Write RES bit in CCCR I/O abort register. It resets the I/O part of
   --  the card (bus width, enabled functions) if the card has not been
   --  powered off since the previous run. A card after power-on doesn't
   --  respond, that's fine.

   procedure Write_Byte
     (Address : Register_Address;
      Value   : Interfaces.Unsigned_8;
      Error   : out STM32.SDIO.Error_Kind;
      Number  : Function_Number := 0);
   --  Write a byte of function Number with CMD52

   function IO_Read_Write_Direct
     (Write    : Boolean;
      Number   : Function_Number;
      Address  : Register_Address;
      Data     : Interfaces.Unsigned_8 := 0) return STM32.SDIO.Command;
   --  Encode CMD52

   procedure Probe_SDIO_Card;
   --  Reset and probe the card: CMD0 + CMD5 (IO_SEND_OP_COND)

   procedure Read_Write_Direct
     (Argument : Interfaces.Unsigned_32;
      Response : out Interfaces.Unsigned_32;
      Success  : out Boolean);

   procedure Read_Write_Extended
     (Argument   : Interfaces.Unsigned_32;
      Block_Size : Positive;
      Length     : Positive;
      Data       : System.Address;
      Response   : out Interfaces.Unsigned_32;
      Success    : out Boolean);

   procedure Sleep (Milliseconds : Positive);

   package Bus is new SDPCM.Generic_SDIO
     (Read_Write_Direct, Read_Write_Extended, Sleep);

   function IO_Read_Write_Direct
     (Write    : Boolean;
      Number   : Function_Number;
      Address  : Register_Address;
      Data     : Interfaces.Unsigned_8 := 0) return STM32.SDIO.Command is
   begin
      return
        (Command  => 52,
         Argument =>
           (if Write then 2 ** 31 else 0)
             + Number * 2 ** 28
             + Address * 2 ** 9
             + Interfaces.Unsigned_32 (Data),
         Response => STM32.SDIO.Short_Response);
   end IO_Read_Write_Direct;

   procedure Probe_SDIO_Card is
      Error : STM32.SDIO.Error_Kind;
      RCA   : Interfaces.Unsigned_32;
   begin
      --  Reset and probe the card: CMD0 + CMD5 (IO_SEND_OP_COND)
      for Attempt in 1 .. 100 loop
         STM32.Polling.SDIO.Send_Command
           ((Command => 0, Argument => 0, Response => STM32.SDIO.No_Response),
            Error);

         STM32.Polling.SDIO.Send_Command
           ((Command  => 5,
             Argument => 0,
             Response => STM32.SDIO.Short_Response_Without_CRC),
            Error);

         exit when Error = STM32.SDIO.No_Error;
         delay 0.001;
      end loop;

      pragma Assert (Error = STM32.SDIO.No_Error);

      --  CMD3 (SEND_RELATIVE_ADDR), R6
      STM32.Polling.SDIO.Send_Command
        ((Command => 3, Argument => 0, others => <>), Error);
      pragma Assert (Error = STM32.SDIO.No_Error);
      RCA := STM32.Polling.SDIO.Short_Response / 2 ** 16;

      --  CMD7 (SELECT_CARD), R1b
      STM32.Polling.SDIO.Send_Command
        ((Command => 7, Argument => RCA * 2 ** 16, others => <>), Error);
      pragma Assert (Error = STM32.SDIO.No_Error);
   end Probe_SDIO_Card;

   procedure Read_Write_Direct
     (Argument : Interfaces.Unsigned_32;
      Response : out Interfaces.Unsigned_32;
      Success  : out Boolean)
   is
      Error : STM32.SDIO.Error_Kind;
   begin
      SDIO.Send_Command
        ((Command  => 52,
          Argument => Argument,
          Response => STM32.SDIO.Short_Response),
         Error);
      Response := STM32.Polling.SDIO.Short_Response;
      Success := Error = STM32.SDIO.No_Error;
   end Read_Write_Direct;

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
            Timeout    => 2_400_000),  --  about 100 ms at 24 MHz
         Buffer => Data,
         Error  => Error);
      Response := STM32.Polling.SDIO.Short_Response;
      Success := Error = STM32.SDIO.No_Error;
   end Read_Write_Extended;

   procedure Reset_IO is
      Ignore : STM32.SDIO.Error_Kind;
   begin
      Write_Byte (CCCR_IO_Abort, 16#08#, Ignore);
      delay 0.02;  --  The chip needs time to react
   end Reset_IO;

   procedure Sleep (Milliseconds : Positive) is
   begin
      delay Duration (Milliseconds) / 1000.0;
   end Sleep;

   procedure WiFi_Reset is
   begin
      STM32.GPIO.Configure_Output (Reset_Pin);
      STM32.GPIO.Set_Output (Reset_Pin, 0);
      delay 0.001;
      STM32.GPIO.Set_Output (Reset_Pin, 1);
      delay 0.001;
   end WiFi_Reset;

   procedure Write_Byte
     (Address : Register_Address;
      Value   : Interfaces.Unsigned_8;
      Error   : out STM32.SDIO.Error_Kind;
      Number  : Function_Number := 0)
   is
      Response : Interfaces.Unsigned_32;
   begin
      STM32.Polling.SDIO.Send_Command
        (IO_Read_Write_Direct (True, Number, Address, Value), Error);

      Response := STM32.Polling.SDIO.Short_Response;

      if Error = STM32.SDIO.No_Error
        and then (Response and R5_Error_Flags) /= 0
      then
         raise Program_Error with "R5 flags: " & Response'Image;
      end if;
   end Write_Byte;

   Ok : Boolean;
begin
   UART.Configure
     (TX   => UART_TX,
      RX   => UART_RX,
      Rate => 115_200);

   UART.Put ("EMW3165 demo" & ASCII.CR & ASCII.LF);

   WiFi_Reset;

   STM32.GPIO.Configure_Interrupt (WiFi_IRQ_Pin);

   SDIO.Configure
     (Clock_Pin   => Clock_Pin,
      Command_Pin => Command_Pin,
      Data_Pins   => Data_Pins,
      Speed       => 400_000);

   Reset_IO;

   Probe_SDIO_Card;

   Bus.Initialize (Ok);
   pragma Assert (Ok);

   Bus.Switch_To_Four_Bit_Bus (Ok);
   pragma Assert (Ok);

   SDIO.Set_Bus (Speed => 400_000, Width => 4);

   Bus.Write_Backplane_Register (16#1000A#, 3, Chipcommon_Base / 256);

   declare
      Value : Interfaces.Unsigned_32;
   begin
      Bus.Read_Backplane_Register (16#8000#, 4, Value);
      pragma Assert (Value = 16#1591_A962#);
   end;

   loop
      delay 1.0;
   end loop;
end EMW3165;
