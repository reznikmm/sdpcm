--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body SDPCM.Generic_SPI is
   use type Interfaces.Unsigned_32;

   package gSPI_Register is
      --  gSPI register addresses from the public datasheet.

      Bus_Control : constant := 16#00#;
      --  Endian, word length and other bus control settings.

      Interrupt   : constant := 16#04#;
      --  Interrupt register (2 bytes).

      Status      : constant := 16#08#;
      --  Same as status bit definitions (4 bytes).

      Read_Test   : constant := 16#14#;
      --  This register contains a predefined pattern, which the
      --  host can read to determine if the gSPI interface is
      --  working properly.
   end gSPI_Register;

   type gSPI_Command is record
      Length  : Natural range 0 .. 2**11 - 1;  --  0 means 2048 bytes
      Address : Natural range 0 .. 2**17 - 1;
      Func    : Bus_Function;
      Incr    : Boolean;
      Write   : Boolean;
   end record;
   --  gSPI Command Structure as defined in the public datasheet.

   for gSPI_Command use record
      Length  at 0 range 0 .. 10;
      Address at 0 range 11 .. 27;
      Func    at 0 range 28 .. 29;
      Incr    at 0 range 30 .. 30;
      Write   at 0 range 31 .. 31;
   end record;

   procedure Read
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Buffer_Byte_Array);

   procedure Read_Register
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Interfaces.Unsigned_32)
     with Pre =>
       (case Bus_Function is
        when SDPCM.Bus => Address in gSPI_Register.Bus_Control
         | gSPI_Register.Interrupt
         | gSPI_Register.Read_Test,
        when Backplane => True,
        when WLAN => Address = 0);

   procedure Write_SPI_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : Interfaces.Unsigned_32)
     with Pre =>
       Address in gSPI_Register.Bus_Control | gSPI_Register.Interrupt;

   function Swap (Value : gSPI_Command) return Word;

   function To_Bytes is new Ada.Unchecked_Conversion
     (gSPI_Command, Word);

   -----------------------------
   -- Available_Packet_Length --
   -----------------------------

   function Available_Packet_Length return Interfaces.Unsigned_32 is
      PKT_AVAIL : constant := 16#100#;
      LEN_SHIFT : constant := 9;
      LEN_MASK  : constant := 16#7FF#;

      Value : Interfaces.Unsigned_32;

   begin
      Read_Register
        (Bus_Function => SDPCM.Bus,
         Address      => gSPI_Register.Status,
         Length       => 4,
         Value        => Value);

      return
        (if (Value and PKT_AVAIL) = 0 then 0
         else Interfaces.Shift_Right (Value, LEN_SHIFT) and LEN_MASK);
   end Available_Packet_Length;

   -----------------
   -- Clear_Error --
   -----------------

   procedure Clear_Error is
      Value : Interfaces.Unsigned_32;
   begin
      Read_Register
        (Bus_Function => SDPCM.Bus,
         Address      => gSPI_Register.Interrupt,
         Length       => 2,
         Value        => Value);

      if (Value and 1) /= 0 then
         Write_SPI_Register
           (Address => gSPI_Register.Interrupt,
            Length  => 2,
            Value   => Value);
      end if;
   end Clear_Error;

   -----------------
   -- Detect_Chip --
   -----------------

   procedure Detect_Chip (Success : out Boolean) is

      function Read_Test_Register_Swapped return Interfaces.Unsigned_32;

      ------------------------------
      -- Read_Test_Register_Swapped --
      ------------------------------

      function Read_Test_Register_Swapped return Interfaces.Unsigned_32 is
         Raw    : constant Word := Swap
           ((Length  => 4,
             Address => gSPI_Register.Read_Test,
             Func    => SDPCM.Bus,
             Incr    => True,
             Write   => False));

         Result : Interfaces.Unsigned_32 := 0;
         Bytes  : Buffer_Byte_Array (1 .. 4)
           with Import, Address => Result'Address;
      begin
         Chip_Select (On => True);
         Write (Raw);
         Read (Bytes);
         Chip_Select (On => False);
         return Result;
      end Read_Test_Register_Swapped;

      Value : Interfaces.Unsigned_32;
   begin
      Value := Read_Test_Register_Swapped;
      Success := Value = 16#EDFE_ADBE#;
   end Detect_Chip;

   --------------
   -- Is_Ready --
   --------------

   function Is_Ready return Boolean is
      F2_RX_READY : constant := 16#20#;

      Value : Interfaces.Unsigned_32;

   begin
      Read_Register
        (Bus_Function => SDPCM.Bus,
         Address      => gSPI_Register.Status,
         Length       => 1,
         Value        => Value);

      return (Value and F2_RX_READY) /= 0;
   end Is_Ready;

   ----------
   -- Read --
   ----------

   procedure Read
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Buffer_Byte_Array)
   is
      Prefix : constant gSPI_Command :=
        (Length  => Length,
         Address => Natural (Address and 16#1FFFF#),
         Func    => Bus_Function,
         Incr    => True,
         Write   => False);

      Raw : constant Word := To_Bytes (Prefix);
   begin
      Chip_Select (On => True);
      Write (Raw);
      Read (Value);
      Chip_Select (On => False);
   end Read;

   -----------------------------
   -- Read_Backplane_Register --
   -----------------------------

   procedure Read_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : out Interfaces.Unsigned_32) is
   begin
      Read_Register (Backplane, Address, Length, Value);
   end Read_Backplane_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Interfaces.Unsigned_32)
   is
      function From_Bytes is new Ada.Unchecked_Conversion
        (Word, Interfaces.Unsigned_32);

      Data : Buffer_Byte_Array (1 .. 8);  --  Gap and value
   begin
      if Bus_Function = Backplane then
         --  Backplane read requires 4 bytes gap before the value
         Read (Bus_Function, Address, Length, Data);
         Value := From_Bytes (Word (Data (5 .. 8)));
      else
         Read (Bus_Function, Address, Length, Data (1 .. 4));
         Value := From_Bytes (Data (1 .. 4));
      end if;
   end Read_Register;

   ---------------
   -- Read_WLAN --
   ---------------

   procedure Read_WLAN (Value : out Buffer_Byte_Array) is
   begin
      Read
        (Bus_Function => SDPCM.WLAN,
         Address      => 0,
         Length       => Value'Length,
         Value        => Value);
   end Read_WLAN;

   ----------
   -- Swap --
   ----------

   function Swap (Value : gSPI_Command) return Word is

      procedure Swap_Byte (Left, Right : in out Interfaces.Unsigned_8);

      procedure Swap_Byte (Left, Right : in out Interfaces.Unsigned_8) is
         Temp : constant Interfaces.Unsigned_8 := Left;
      begin
         Left := Right;
         Right := Temp;
      end Swap_Byte;

      Result : gSPI_Command := Value;

      Raw    : Word
        with Import, Address => Result'Address;

   begin
      Swap_Byte (Raw (1), Raw (2));
      Swap_Byte (Raw (3), Raw (4));

      return Raw;
   end Swap;

   -------------------
   -- Switch_Endian --
   -------------------

   procedure Switch_Endian (Success : out Boolean) is

      procedure Write_Bus_Control_Register_Swapped (Bytes : Word);

      ----------------------------------------
      -- Write_Bus_Control_Register_Swapped --
      ----------------------------------------

      procedure Write_Bus_Control_Register_Swapped (Bytes : Word) is

         Raw : constant Buffer_Byte_Array (1 .. 8) := Swap
           ((Length  => 4,
             Address => gSPI_Register.Bus_Control,
             Func    => SDPCM.Bus,
             Incr    => True,
             Write   => True)) & Bytes;

      begin
         Chip_Select (On => True);
         Write (Raw);
         Chip_Select (On => False);
      end Write_Bus_Control_Register_Swapped;

      Value : Interfaces.Unsigned_32;
   begin
      Write_Bus_Control_Register_Swapped ((04, 16#b3#, 0, 2));
      Read_Register (SDPCM.Bus, gSPI_Register.Read_Test, 4, Value);
      Success := Value = 16#FEED_BEAD#;
   end Switch_Endian;

   ---------------------
   -- Write_Backplane --
   ---------------------

   procedure Write_Backplane
     (Address : Interfaces.Unsigned_32;
      Value   : Buffer_Byte_Array)
   is
      pragma Unreferenced (Address);  --  Address is embedded in Value
   begin
      Chip_Select (On => True);
      Write (Value);
      Chip_Select (On => False);
   end Write_Backplane;

   ----------------
   -- Write_WLAN --
   ----------------

   procedure Write_WLAN (Value : Buffer_Byte_Array) is
   begin
      Chip_Select (On => True);
      Write (Value);
      Chip_Select (On => False);
   end Write_WLAN;

   ------------------------------
   -- Write_Backplane_Register --
   ------------------------------

   procedure Write_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : Interfaces.Unsigned_32)
   is
      function To_Bytes is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_32, Word);

      Output : constant Buffer_Byte_Array (1 .. 8) :=
        (Write_Prefix (Backplane, Address, Length) & To_Bytes (Value));
   begin
      Write_Backplane (Address, Output);
   end Write_Backplane_Register;

   ------------------
   -- Write_Prefix --
   ------------------

   function Write_Prefix
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return Word is
     (Word
       (To_Bytes
         ((Length  => Length,
           Address => Natural (Address),
           Func    => Bus_Function,
           Incr    => True,
           Write   => True))));

   ------------------------
   -- Write_SPI_Register --
   ------------------------

   procedure Write_SPI_Register
     (Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : Interfaces.Unsigned_32)
   is
      function To_Bytes is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_32, Word);

      Output : constant Buffer_Byte_Array (1 .. 8) :=
        (Write_Prefix (SDPCM.Bus, Address, Length) & To_Bytes (Value));
   begin
      Chip_Select (On => True);
      Write (Output);
      Chip_Select (On => False);
   end Write_SPI_Register;

end SDPCM.Generic_SPI;
