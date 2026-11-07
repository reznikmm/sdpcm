--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with System.Storage_Elements;

package body SDPCM.Generic_SDIO is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_32;
   use type System.Storage_Elements.Storage_Offset;

   package CCCR is
      --  Card Common Control Registers (function 0)
      IO_Enable            : constant := 16#02#;
      IO_Ready             : constant := 16#03#;
      Interrupt_Enable     : constant := 16#04#;
      IO_Abort             : constant := 16#06#;
      Bus_Interface        : constant := 16#07#;
      Block_Size_Low       : constant := 16#10#;
      Function_1_Block_Low : constant := 16#110#;
      Function_2_Block_Low : constant := 16#210#;
      Function_2_Block_High : constant := 16#211#;
   end CCCR;

   package Function_1_Register is
      --  SDIO core registers in function 1 address space
      Frame_Control  : constant := 16#1000D#;
      Chip_Clock_CSR : constant := 16#1000E#;
      Pull_Up        : constant := 16#1000F#;
   end Function_1_Register;

   Function_1_Bit : constant := 2;  --  IO_Enable, IO_Ready, interrupts
   Function_2_Bit : constant := 4;
   Master_Interrupt_Enable : constant := 1;

   Four_Bit_Bus   : constant := 2;
   Bus_Width_Mask : constant := 3;

   Force_ALP           : constant := 16#01#;
   ALP_Available_Request : constant := 16#08#;
   Force_Hardware_Clock_Request_Off : constant := 16#20#;
   ALP_Available       : constant := 16#40#;

   Read_Frame_Terminate : constant := 1;

   R5_Error_Flags : constant := 16#CB00#;
   --  COM_CRC_ERROR, ILLEGAL_COMMAND, ERROR, FUNCTION_NUMBER, OUT_OF_RANGE

   Maximum_Block_Count : constant := 511;

   Minimum_Chunk : constant := 16;
   --  The smallest byte mode transfer for asynchronous CMD53 (four words,
   --  a DMA burst)

   type Initialization_Step is
     (Enable_Backplane,
      Set_Block_Size,
      Wait_Backplane_Ready,
      Wait_ALP_Clock);
   --  Steps of Initialize which could wait

   Init_Step    : Initialization_Step := Enable_Backplane;
   Init_Attempt : Natural := 0;
   --  The current step of Initialize and its attempt number

   Failed      : Boolean := False;
   Tag_Pending : Boolean := False;
   Tag         : Interfaces.Unsigned_32 := 0;

   type Transfer_Kind is (None, Writing, Reading);

   Pending     : Transfer_Kind := None;
   --  Kind of the asynchronous transfer in progress

   Tail_Data    : System.Address;
   Tail_Length  : Natural := 0;
   Tail_Address : Interfaces.Unsigned_32 := 0;
   --  The rest of the frame (less than a block) to be read by
   --  Complete_Transfer after the asynchronous block read, or the rest of
   --  the backplane data to be written at Tail_Address after the
   --  asynchronous block write

   function Power_Of_Two
     (Value   : Positive;
      Minimum : Positive) return Positive;
   --  The smallest power of two not less than Value and Minimum

   procedure Write_Tail
     with Pre => Tail_Length > 0;
   --  Write Tail_Length bytes from Tail_Data to the backplane at
   --  Tail_Address synchronously, without padding

   procedure Read_Tail
     with Pre => Tail_Length > 0;
   --  Read Tail_Length bytes of the frame to Tail_Data with one byte mode
   --  CMD53. The transfer length is rounded up to a power of two, extra
   --  bytes are dropped (the device pads the frame).

   subtype Function_Number is Interfaces.Unsigned_32 range 0 .. 7;

   function Number (Value : Bus_Function) return Function_Number is
     (Bus_Function'Pos (Value));

   function Direct_Argument
     (Write    : Boolean;
      Number   : Function_Number;
      Address  : Interfaces.Unsigned_32;
      Data     : Interfaces.Unsigned_8 := 0) return Interfaces.Unsigned_32 is
       ((if Write then 2 ** 31 else 0)
        + Number * 2 ** 28
        + (Address and 16#1_FFFF#) * 2 ** 9
        + Interfaces.Unsigned_32 (Data));
   --  CMD52 argument

   function Extended_Argument
     (Write      : Boolean;
      Number     : Function_Number;
      Block_Mode : Boolean;
      Address    : Interfaces.Unsigned_32;
      Count      : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
       ((if Write then 2 ** 31 else 0)
        + Number * 2 ** 28
        + (if Block_Mode then 2 ** 27 else 0)
        + 2 ** 26  --  OP code: incrementing address
        + (Address and 16#1_FFFF#) * 2 ** 9
        + (Count and 16#1FF#));
   --  CMD53 argument

   procedure Read_Byte
     (Number  : Function_Number;
      Address : Interfaces.Unsigned_32;
      Value   : out Interfaces.Unsigned_8;
      Success : out Boolean);

   procedure Write_Byte
     (Number  : Function_Number;
      Address : Interfaces.Unsigned_32;
      Value   : Interfaces.Unsigned_8;
      Success : out Boolean);

   procedure Extended
     (Write      : Boolean;
      Number     : Function_Number;
      Address    : Interfaces.Unsigned_32;
      Count      : Positive;
      Data       : System.Address;
      Success    : out Boolean)
     with Pre => Count in 4 | 8 | 16 | 32 | 64;
   --  Send byte mode CMD53 with Count bytes synchronously

   procedure Transfer_Exactly
     (Write   : Boolean;
      Number  : Function_Number;
      Address : Interfaces.Unsigned_32;
      Data    : System.Address;
      Length  : Natural;
      Success : out Boolean)
     with Pre => Length < Block_Size;
   --  Transfer exactly Length bytes (less than a block) synchronously:
   --  power of two byte mode chunks, then single bytes (CMD52 for function
   --  1, a padded word for function 2).

   procedure Abort_Read;
   --  Abort function 2 read and drop the rest of the frame

   procedure Mark (Success : Boolean);
   --  Update Failed flag

   ----------------
   -- Abort_Read --
   ----------------

   procedure Abort_Read is
      Ignore : Boolean;
   begin
      Write_Byte (0, CCCR.IO_Abort, Interfaces.Unsigned_8 (Number (WLAN)),
                  Ignore);
      Write_Byte
        (1, Function_1_Register.Frame_Control, Read_Frame_Terminate, Ignore);
      Tag_Pending := False;
   end Abort_Read;

   -----------------------------
   -- Available_Packet_Length --
   -----------------------------

   function Available_Packet_Length return Interfaces.Unsigned_32 is
      Success : Boolean;
      Length  : Interfaces.Unsigned_32;
      Check   : Interfaces.Unsigned_32;
   begin
      if not Tag_Pending then
         --  The tag could be read at any time, it is zero if there is no
         --  frame available.
         Tag := 0;

         Extended
           (Write      => False,
            Number     => Number (WLAN),
            Address    => 0,
            Count      => 4,
            Data       => Tag'Address,
            Success    => Success);

         if not Success then
            Abort_Read;
            return 0;
         end if;
      end if;

      Length := Tag and 16#FFFF#;
      Check := Interfaces.Shift_Right (Tag, 16);

      if Length = 0 or (Length xor Check) /= 16#FFFF# or Length < 4 then
         Tag_Pending := False;

         return 0;
      else
         Tag_Pending := True;

         return Length;
      end if;
   end Available_Packet_Length;

   -----------------
   -- Clear_Error --
   -----------------

   procedure Clear_Error is
   begin
      Failed := False;
   end Clear_Error;

   --------------
   -- Extended --
   --------------

   procedure Extended
     (Write      : Boolean;
      Number     : Function_Number;
      Address    : Interfaces.Unsigned_32;
      Count      : Positive;
      Data       : System.Address;
      Success    : out Boolean)
   is
      Response : Interfaces.Unsigned_32;
   begin
      IO_Read_Write_Extended
        (Argument   =>
           Extended_Argument
             (Write, Number, False, Address,
              Interfaces.Unsigned_32 (Count)),
         Block_Size => Count,
         Length     => Count,
         Data       => Data,
         Response   => Response,
         Success    => Success);

      Success := Success and (Response and R5_Error_Flags) = 0;
      Mark (Success);
   end Extended;

   ----------------
   -- Has_Failed --
   ----------------

   -----------------------
   -- Complete_Transfer --
   -----------------------

   procedure Complete_Transfer
     (Response : Interfaces.Unsigned_32;
      Success  : Boolean)
   is
      Ok : constant Boolean :=
        Success and (Response and R5_Error_Flags) = 0;
   begin
      Mark (Ok);

      case Pending is
         when Reading =>
            Pending := None;

            if not Ok then
               Abort_Read;
               Tail_Length := 0;
            elsif Tail_Length > 0 then
               Read_Tail;
            end if;

         when Writing =>
            Pending := None;

            if not Ok then
               Tail_Length := 0;
            elsif Tail_Length > 0 then
               Write_Tail;
            end if;

         when None =>
            null;
      end case;
   end Complete_Transfer;

   function Has_Failed return Boolean is (Failed);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Success : out Boolean;
      Sleep   : in out Natural)
   is
      procedure Go_To (Step : Initialization_Step);
      --  Switch to the next Step

      procedure Retry (Limit : Positive);
      --  Ask to sleep 1 ms and repeat the current step, or fail if Limit
      --  attempts have been done already

      procedure Finish (Result : Boolean);
      --  Report Result and reset the state for the next Initialize

      -----------
      -- Go_To --
      -----------

      procedure Go_To (Step : Initialization_Step) is
      begin
         Init_Step := Step;
         Init_Attempt := 0;
      end Go_To;

      ------------
      -- Finish --
      ------------

      procedure Finish (Result : Boolean) is
      begin
         Success := Result;
         Sleep := 0;
         Go_To (Enable_Backplane);
      end Finish;

      -----------
      -- Retry --
      -----------

      procedure Retry (Limit : Positive) is
      begin
         if Init_Attempt < Limit then
            Init_Attempt := Init_Attempt + 1;
            Success := False;
            Sleep := 1;
         else
            Finish (False);
         end if;
      end Retry;

      Value : Interfaces.Unsigned_8 := 0;
   begin
      if Sleep = 0 then
         Init_Step := Enable_Backplane;
      end if;

      loop
         case Init_Step is
            when Enable_Backplane =>
               --  Enable function 1 (backplane)
               Write_Byte (0, CCCR.IO_Enable, Function_1_Bit, Success);
               Read_Byte (0, CCCR.IO_Enable, Value, Success);

               if Success and Value = Function_1_Bit then
                  Go_To (Set_Block_Size);
               else
                  Retry (500);
                  exit;
               end if;

            when Set_Block_Size =>
               --  Set the block size, wait till the backplane is ready
               Write_Byte (0, CCCR.Block_Size_Low, Block_Size, Success);
               Read_Byte (0, CCCR.Block_Size_Low, Value, Success);

               if not Success or Value /= Block_Size then
                  Retry (500);
                  exit;
               end if;

               Write_Byte (0, CCCR.Function_1_Block_Low, Block_Size, Success);

               if Success then
                  Write_Byte
                    (0, CCCR.Function_2_Block_Low, Block_Size, Success);
               end if;

               if Success then
                  Write_Byte (0, CCCR.Function_2_Block_High, 0, Success);
               end if;

               if Success then
                  Write_Byte
                    (0,
                     CCCR.Interrupt_Enable,
                     Master_Interrupt_Enable + Function_1_Bit
                       + Function_2_Bit,
                     Success);
               end if;

               if Success then
                  Go_To (Wait_Backplane_Ready);
               else
                  Finish (False);
                  exit;
               end if;

            when Wait_Backplane_Ready =>
               Read_Byte (0, CCCR.IO_Ready, Value, Success);

               if not Success then
                  Finish (False);
                  exit;
               elsif (Value and Function_1_Bit) = 0 then
                  Retry (500);
                  exit;
               end if;

               --  Request ALP clock
               Write_Byte
                 (1,
                  Function_1_Register.Chip_Clock_CSR,
                  Force_Hardware_Clock_Request_Off + ALP_Available_Request
                    + Force_ALP,
                  Success);

               if Success then
                  Go_To (Wait_ALP_Clock);
               else
                  Finish (False);
                  exit;
               end if;

            when Wait_ALP_Clock =>
               Read_Byte
                 (1, Function_1_Register.Chip_Clock_CSR, Value, Success);

               if not Success then
                  Finish (False);
                  exit;
               elsif (Value and ALP_Available) = 0 then
                  Retry (100);
                  exit;
               end if;

               Write_Byte (1, Function_1_Register.Chip_Clock_CSR, 0, Success);

               --  Disable the extra SDIO pull-ups
               if Success then
                  Write_Byte (1, Function_1_Register.Pull_Up, 0, Success);
               end if;

               --  Enable functions 1 and 2
               if Success then
                  Write_Byte
                    (0, CCCR.IO_Enable, Function_1_Bit + Function_2_Bit,
                     Success);
               end if;

               --  Enable function 2 interrupt only
               if Success then
                  Write_Byte
                    (0,
                     CCCR.Interrupt_Enable,
                     Master_Interrupt_Enable + Function_2_Bit,
                     Success);
               end if;

               Finish (Success);
               exit;
         end case;
      end loop;
   end Initialize;

   --------------
   -- Is_Ready --
   --------------

   function Is_Transfer_Pending return Boolean is (Pending /= None);

   function Is_Ready return Boolean is
      Value   : Interfaces.Unsigned_8;
      Success : Boolean;
   begin
      Read_Byte (0, CCCR.IO_Ready, Value, Success);

      return Success and then (Value and Function_2_Bit) /= 0;
   end Is_Ready;

   ----------
   -- Mark --
   ----------

   procedure Mark (Success : Boolean) is
   begin
      Failed := Failed or not Success;
   end Mark;

   -----------------------------
   -- Read_Backplane_Register --
   -----------------------------

   procedure Read_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : out Interfaces.Unsigned_32)
   is
      Ignore  : Boolean;  --  Has_Failed reports errors
      Byte    : Interfaces.Unsigned_8;
   begin
      Value := 0;

      if Length = 4 and Address < 16#1_0000# then
         --  A register in the backplane window
         Extended
           (Write      => False,
            Number     => Number (Backplane),
            Address    => Address,
            Count      => 4,
            Data       => Value'Address,
            Success    => Ignore);
      else
         --  SDIO core registers (16#1000A# .. 16#1001F#) are byte wide
         for J in 0 .. Length - 1 loop
            Read_Byte
              (Number (Backplane),
               Address + Interfaces.Unsigned_32 (J),
               Byte,
               Ignore);

            Value := Value or Interfaces.Shift_Left
              (Interfaces.Unsigned_32 (Byte), 8 * J);
         end loop;
      end if;
   end Read_Backplane_Register;

   ---------------
   -- Read_Byte --
   ---------------

   procedure Read_Byte
     (Number  : Function_Number;
      Address : Interfaces.Unsigned_32;
      Value   : out Interfaces.Unsigned_8;
      Success : out Boolean)
   is
      Response : Interfaces.Unsigned_32;
   begin
      IO_Read_Write_Direct
        (Direct_Argument (False, Number, Address), Response, Success);

      Value := Interfaces.Unsigned_8 (Response and 16#FF#);
      Success := Success and (Response and R5_Error_Flags) = 0;
      Mark (Success);
   end Read_Byte;

   ------------------
   -- Power_Of_Two --
   ------------------

   function Power_Of_Two
     (Value   : Positive;
      Minimum : Positive) return Positive
   is
      Result : Positive := Minimum;
   begin
      while Result < Value loop
         Result := Result * 2;
      end loop;

      return Result;
   end Power_Of_Two;

   ---------------
   -- Read_Tail --
   ---------------

   procedure Read_Tail is
      Rest    : Buffer_Byte_Array (1 .. Tail_Length)
        with Import, Address => Tail_Data;
      Chunk   : constant Positive := Power_Of_Two (Tail_Length, 4);
      Word    : Buffer_Byte_Array (1 .. Block_Size) := (others => 0);
      Success : Boolean;
   begin
      Extended
        (Write      => False,
         Number     => Number (WLAN),
         Address    => 0,
         Count      => Chunk,
         Data       => Word'Address,
         Success    => Success);

      if Success then
         Rest := Word (1 .. Tail_Length);
      else
         Abort_Read;
      end if;

      Tail_Length := 0;
   end Read_Tail;

   ---------------
   -- Read_WLAN --
   ---------------

   procedure Read_WLAN (Value : out Buffer_Byte_Array) is
      Tag_Bytes : Buffer_Byte_Array (1 .. 4)
        with Import, Address => Tag'Address;

      First  : Positive := Value'First;
      Blocks : Natural;
   begin
      if Tag_Pending and Value'Length >= 4 then
         Value (First .. First + 3) := Tag_Bytes;
         First := First + 4;
         Tag_Pending := False;
      end if;

      if First > Value'Last then
         return;
      end if;

      Blocks := Natural'Min
        ((Value'Last - First + 1) / Block_Size, Maximum_Block_Count);

      Tail_Length := Value'Last - First + 1 - Blocks * Block_Size;

      if Tail_Length > 0 then
         Tail_Data := Value (First + Blocks * Block_Size)'Address;
      end if;

      if Blocks > 0 then
         Pending := Reading;

         Start_IO_Read_Write_Extended
           (Argument   =>
              Extended_Argument
                (Write      => False,
                 Number     => Number (WLAN),
                 Block_Mode => True,
                 Address    => 0,
                 Count      => Interfaces.Unsigned_32 (Blocks)),
            Block_Size => Block_Size,
            Length     => Blocks * Block_Size,
            Data       => Value (First)'Address);
      elsif Tail_Length > 0 then
         Read_Tail;
      end if;
   end Read_WLAN;

   ----------------------------
   -- Switch_To_Four_Bit_Bus --
   ----------------------------

   procedure Switch_To_Four_Bit_Bus (Success : out Boolean) is
      Value : Interfaces.Unsigned_8;
   begin
      Read_Byte (0, CCCR.Bus_Interface, Value, Success);

      if Success then
         Write_Byte
           (0,
            CCCR.Bus_Interface,
            (Value and not Bus_Width_Mask) or Four_Bit_Bus,
            Success);
      end if;
   end Switch_To_Four_Bit_Bus;

   ----------------------
   -- Transfer_Exactly --
   ----------------------

   procedure Transfer_Exactly
     (Write   : Boolean;
      Number  : Function_Number;
      Address : Interfaces.Unsigned_32;
      Data    : System.Address;
      Length  : Natural;
      Success : out Boolean)
   is
      use System.Storage_Elements;

      Is_FIFO : constant Boolean := Number = SDPCM.Generic_SDIO.Number (WLAN);
      Offset  : Natural := 0;
      Chunk   : Positive := Block_Size / 2;

      function Next_Address return Interfaces.Unsigned_32 is
        (if Is_FIFO then Address
         else Address + Interfaces.Unsigned_32 (Offset));

      function Next_Data return System.Address is
        (Data + Storage_Offset (Offset));
   begin
      Success := True;

      --  Power of two chunks in byte mode: 32, 16, 8, 4
      while Success and Length - Offset >= 4 loop
         while Chunk > Length - Offset loop
            Chunk := Chunk / 2;
         end loop;

         Extended
           (Write      => Write,
            Number     => Number,
            Address    => Next_Address,
            Count      => Chunk,
            Data       => Next_Data,
            Success    => Success);

         Offset := Offset + Chunk;
      end loop;

      --  Last 1 .. 3 bytes
      if Success and Offset < Length then
         declare
            Rest : Buffer_Byte_Array (1 .. Length - Offset)
              with Import, Address => Next_Data;
         begin
            if Is_FIFO then
               declare
                  Word : Buffer_Byte_Array (1 .. 4) := (others => 0);
               begin
                  if Write then
                     Word (Rest'Range) := Rest;
                  end if;

                  Extended
                    (Write      => Write,
                     Number     => Number,
                     Address    => Address,
                     Count      => 4,
                     Data       => Word'Address,
                     Success    => Success);

                  if not Write then
                     Rest := Word (Rest'Range);
                  end if;
               end;
            else
               for J in Rest'Range loop
                  exit when not Success;

                  if Write then
                     Write_Byte (Number, Next_Address, Rest (J), Success);
                  else
                     Read_Byte (Number, Next_Address, Rest (J), Success);
                  end if;

                  Offset := Offset + 1;
               end loop;
            end if;
         end;
      end if;
   end Transfer_Exactly;

   ---------------------
   -- Write_Backplane --
   ---------------------

   procedure Write_Backplane
     (Address : Interfaces.Unsigned_32;
      Value   : Buffer_Byte_Array)
   is
      Blocks : constant Natural :=
        Natural'Min (Value'Length / Block_Size, Maximum_Block_Count);
      Ignore : Boolean;  --  Has_Failed reports errors
   begin
      Tail_Length := Value'Length - Blocks * Block_Size;

      if Blocks = 0 then
         --  Exact length: no padding to keep RAM after the image intact
         Transfer_Exactly
           (Write   => True,
            Number  => Number (Backplane),
            Address => Address,
            Data    => Value'Address,
            Length  => Value'Length,
            Success => Ignore);

         Tail_Length := 0;
      else
         if Tail_Length > 0 then
            Tail_Data := Value (Value'First + Blocks * Block_Size)'Address;
            Tail_Address :=
              Address + Interfaces.Unsigned_32 (Blocks * Block_Size);
         end if;

         Pending := Writing;

         Start_IO_Read_Write_Extended
           (Argument   =>
              Extended_Argument
                (Write      => True,
                 Number     => Number (Backplane),
                 Block_Mode => True,
                 Address    => Address,
                 Count      => Interfaces.Unsigned_32 (Blocks)),
            Block_Size => Block_Size,
            Length     => Blocks * Block_Size,
            Data       => Value'Address);
      end if;
   end Write_Backplane;

   ----------------
   -- Write_Tail --
   ----------------

   procedure Write_Tail is
      Ignore : Boolean;  --  Has_Failed reports errors
   begin
      --  Exact length: no padding to keep RAM after the image intact
      Transfer_Exactly
        (Write   => True,
         Number  => Number (Backplane),
         Address => Tail_Address,
         Data    => Tail_Data,
         Length  => Tail_Length,
         Success => Ignore);

      Tail_Length := 0;
   end Write_Tail;

   ------------------------------
   -- Write_Backplane_Register --
   ------------------------------

   procedure Write_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : Interfaces.Unsigned_32)
   is
      Ignore : Boolean;  --  Has_Failed reports errors
      Data   : Interfaces.Unsigned_32 := Value;
   begin
      if Length = 4 and Address < 16#1_0000# then
         --  A register in the backplane window
         Extended
           (Write      => True,
            Number     => Number (Backplane),
            Address    => Address,
            Count      => 4,
            Data       => Data'Address,
            Success    => Ignore);
      else
         --  SDIO core registers (16#1000A# .. 16#1001F#) are byte wide
         for J in 0 .. Length - 1 loop
            Write_Byte
              (Number (Backplane),
               Address + Interfaces.Unsigned_32 (J),
               Interfaces.Unsigned_8 (Data and 16#FF#),
               Ignore);

            Data := Interfaces.Shift_Right (Data, 8);
         end loop;
      end if;
   end Write_Backplane_Register;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte
     (Number  : Function_Number;
      Address : Interfaces.Unsigned_32;
      Value   : Interfaces.Unsigned_8;
      Success : out Boolean)
   is
      Response : Interfaces.Unsigned_32;
   begin
      IO_Read_Write_Direct
        (Direct_Argument (True, Number, Address, Value), Response, Success);

      Success := Success and (Response and R5_Error_Flags) = 0;
      Mark (Success);
   end Write_Byte;

   ----------------
   -- Write_WLAN --
   ----------------

   procedure Write_WLAN (Value : Buffer_Byte_Array) is
      Length : constant Positive := Value'Length;
      Blocks : constant Positive := (Length + Block_Size - 1) / Block_Size;
      Chunk  : constant Positive := Power_Of_Two (Length, Minimum_Chunk);
   begin
      --  One CMD53 per frame, padded up to the block size or to a power of
      --  two in byte mode (the STM32 data path works with blocks only).
      Tail_Length := 0;  --  Frames are padded, no tail
      Pending := Writing;

      if Length >= Block_Size then
         Start_IO_Read_Write_Extended
           (Argument   =>
              Extended_Argument
                (Write      => True,
                 Number     => Number (WLAN),
                 Block_Mode => True,
                 Address    => 0,
                 Count      => Interfaces.Unsigned_32 (Blocks)),
            Block_Size => Block_Size,
            Length     => Blocks * Block_Size,
            Data       => Value'Address);
      else
         Start_IO_Read_Write_Extended
           (Argument   =>
              Extended_Argument
                (Write      => True,
                 Number     => Number (WLAN),
                 Block_Mode => False,
                 Address    => 0,
                 Count      => Interfaces.Unsigned_32 (Chunk)),
            Block_Size => Chunk,
            Length     => Chunk,
            Data       => Value'Address);
      end if;
   end Write_WLAN;

end SDPCM.Generic_SDIO;
