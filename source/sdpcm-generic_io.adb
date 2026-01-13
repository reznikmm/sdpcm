--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

package body SDPCM.Generic_IO is
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   package Backplane_Register is
      Win_Addr : constant := 16#1000a#;  --  Window addr
      Chip_Clock_CSR : constant := 16#1000e#;  --  Chip clock ctrl
      Pull_Up        : constant := 16#1000f#;
      Sleep_CSR      : constant := 16#1001f#;
   end Backplane_Register;

   subtype Frame_Tag is Interfaces.Unsigned_32;

   function Make_Tag (Length : Interfaces.Unsigned_16) return Frame_Tag is
     (Interfaces.Shift_Left (Interfaces.Unsigned_32 (not Length), 16) +
      Interfaces.Unsigned_32 (Length));

   package Executor is

      type Step_Kind is
        (Write_Register,
         Write_Register_Variable,
         Read_Register,
         Read_Register_Until,
         Upload_Firmware,
         Wait_Any_Event,
         Clear_Error,
         Sleep);

      type Step (Kind : Step_Kind := Sleep) is record
         case Kind is
            when Read_Register
               | Read_Register_Until
               | Write_Register
               | Write_Register_Variable
            =>
               Address : Interfaces.Unsigned_32;
               Length  : Positive;

               case Kind is
                  when Read_Register | Write_Register_Variable =>
                     null;

                  when Read_Register_Until =>
                     Mask : Interfaces.Unsigned_32;
                     Trys : Natural;

                  when Write_Register =>
                     Value : Interfaces.Unsigned_32;

                  when Upload_Firmware
                     | Wait_Any_Event
                     | Clear_Error
                     | Sleep
                   =>
                     null;
               end case;

            when Upload_Firmware =>
               Firmware : Resource_Kind;

            when Clear_Error =>
               null;

            when Sleep | Wait_Any_Event =>
               Milliseconds : Natural;
         end case;
      end record;

      type Step_Array is array (Positive range <>) of Step;

      procedure Execute
        (Step         : Executor.Step;
         Index        : in out Positive;
         Offset       : in out Natural;
         Buffer       : in out Byte_Array;
         Success      : in out Boolean;
         Custom_Value : Interfaces.Unsigned_32 := 0);

      AI_IOCTRL_OSET    : constant := 16#408#;
      AI_RESETCTRL_OSET : constant := 16#800#;
      RAM_Core_Base : constant Interfaces.Unsigned_32 := 16#4000#;
      ARM_Core_Base : constant Interfaces.Unsigned_32 := 16#3000#;

      Start : constant Step_Array :=
        [  --  1 =>
           (Kind         => Read_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Length       => 1),
         --  Check Active Low Power (ALP) clock
         --  2 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Value        => 16#08#,
            Length       => 1),
         --  3 =>
           (Kind         => Read_Register_Until,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Mask         => 16#40#,
            Length       => 1,
            Trys         => 10),
         --  4 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Value        => 0,
            Length       => 1),
         --  Reset (RAM)
         --  5 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#1810_0000# / 256,
            Length       => 3),
         --  6 =>
           (Kind         => Read_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  7 =>
           (Kind         => Write_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Value        => 3,
            Length       => 1),
         --  8 =>
           (Kind         => Read_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  9 =>
           (Kind         => Write_Register,
            Address      => RAM_Core_Base + AI_RESETCTRL_OSET,
            Value        => 0,
            Length       => 1),
         --  10 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  11 =>
           (Kind         => Write_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Value        => 1,
            Length       => 1),
         --  12 =>
           (Kind         => Read_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  13 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  end of reset
           --  14 =>
           (Kind         => Executor.Read_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Length       => 1),
           --  Write 0x18004010 and 0x18004044
           --  15 =>
           (Kind         => Executor.Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#180000#,
            Length       => 3),
           --  16 =>
           (Kind         => Executor.Write_Register,
            Address      => 16#04010#,
            Value        => 3,
            Length       => 4),
           --  17 =>
           (Kind         => Executor.Write_Register,
            Address      => 16#04044#,
            Value        => 0,
            Length       => 4),
           --  18 =>
           (Kind         => Executor.Upload_Firmware,
            Firmware     => Firmware),
           --  19 =>
           (Kind         => Executor.Sleep,
            Milliseconds => 5),
           --  20 =>
           (Kind         => Executor.Upload_Firmware,
            Firmware     => NVRAM),
           --  21 =>
           (Kind         => Executor.Write_Register_Variable,
            Address      => 16#FFFC#,  --  NVRAM size register
            Length       => 4),
         --  Reset (ARM)
         --  22 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#1810_0000# / 256,
            Length       => 3),
         --  23 =>
           (Kind         => Read_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  24 =>
           (Kind         => Write_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Value        => 3,
            Length       => 1),
         --  25 =>
           (Kind         => Read_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  26 =>
           (Kind         => Write_Register,
            Address      => ARM_Core_Base + AI_RESETCTRL_OSET,
            Value        => 0,
            Length       => 1),
         --  27 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  28 =>
           (Kind         => Write_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Value        => 1,
            Length       => 1),
         --  29 =>
           (Kind         => Read_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  30 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  end of reset
         --  31 =>
           (Kind         => Executor.Read_Register_Until,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Mask         => 16#80#,
            Length       => 1,
            Trys         => 50),
         --  32 =>
           (Kind => Executor.Wait_Any_Event,
            Milliseconds => 100)];
   end Executor;

   package body Executor is

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Step         : Executor.Step;
         Index        : in out Positive;
         Offset       : in out Natural;
         Buffer       : in out Byte_Array;
         Success      : in out Boolean;
         Custom_Value : Interfaces.Unsigned_32 := 0)
      is
         procedure Increment_Step;

         Block_Size  : constant := 64;
         Window_Size : constant := 16#8000#;

         subtype Block_Range is Positive range
           Bus.Write_Prefix_Length + 1 .. Bus.Write_Prefix_Length + Block_Size;

         --------------------
         -- Increment_Step --
         --------------------

         procedure Increment_Step is
         begin
            Index := Index + 1;
            Offset := 0;
         end Increment_Step;

         Value       : Interfaces.Unsigned_32;
         Last        : Natural;
      begin
         case Step.Kind is
            when Write_Register =>
               Bus.Write_Backplane_Register
                 (Step.Address, Step.Length, Step.Value);

            when Write_Register_Variable =>
               Bus.Write_Backplane_Register
                 (Step.Address, Step.Length, Custom_Value);

            when Read_Register =>
               Bus.Read_Backplane_Register (Step.Address, Step.Length, Value);

            when Read_Register_Until =>
               Bus.Read_Backplane_Register (Step.Address, Step.Length, Value);

            when Clear_Error =>
               Bus.Clear_Error;

            when Upload_Firmware =>
               Read_Resource
                 (Step.Firmware,
                  Offset,
                  Buffer (Block_Range),
                  Last);

               --  Calculate write Address
               Value := (if Step.Firmware = Firmware then 0 else 16#7_FCFC#) +
                 Interfaces.Unsigned_32 (Offset);

               if Last in Block_Range then
                  Buffer (1 .. Bus.Write_Prefix_Length) := Bus.Write_Prefix
                    (Bus_Function => SDPCM.Backplane,
                     Address      => Value mod Window_Size,
                     Length       => Last - Bus.Write_Prefix_Length);

                  if Offset = 0 or
                    (Value - 1) / Window_Size /= Value / Window_Size
                  then
                     Bus.Write_Backplane_Register
                       (Address      => Backplane_Register.Win_Addr,
                        Value        => Value / 256,
                        Length       => 3);
                  end if;

                  Bus.Write_Backplane
                    (Address => Value,
                     Value   => Buffer (1 .. Last));
               end if;

            when Wait_Any_Event =>
               Value := Boolean'Pos (Bus.Has_Event);

            when others =>
               null;
         end case;

         case Step.Kind is
            when Read_Register_Until =>
               if (Value and Step.Mask) /= 0 then
                  Increment_Step;
               elsif Offset < Step.Trys then
                  Offset := Offset + 1;
               else
                  Success := False;
               end if;

            when Upload_Firmware =>
               if Last < Block_Range'Last then
                  Increment_Step;
               else
                  Offset := Offset + Block_Size;
               end if;

            when Wait_Any_Event =>
               if Value = 1 then
                  Increment_Step;
               elsif Offset < Step.Milliseconds then
                  Offset := Offset + 1;
               else
                  Success := False;
               end if;

            when others =>
               Increment_Step;
         end case;
      end Execute;

   end Executor;

   -------------
   -- Process --
   -------------

   procedure Process
     (State  : in out SDPCM.Generic_IO.State;
      Buffer : in out Byte_Array;
      Length : Natural;
      Action : out SDPCM.Generic_IO.Action)
   is
      function To_Action
        (Step : Executor.Step) return SDPCM.Generic_IO.Action is
          (case Step.Kind is
              when Executor.Sleep =>
                (Sleep, Milliseconds => Step.Milliseconds),
              when Executor.Read_Register_Until
                | Executor.Wait_Any_Event =>
                  (if State.Offset = 0 then (Kind => Continue)
                   else (Sleep, Milliseconds => 1)),
              when others =>
                (Kind => Continue));

      Ok : Boolean := True;
   begin
      case State.Joining.Kind is
         when Boot_Up =>
            Executor.Execute
              (Step         => Executor.Start (State.Step),
               Index        => State.Step,
               Offset       => State.Offset,
               Buffer       => Buffer,
               Success      => Ok,
               Custom_Value => Make_Tag (16#300# / 4));

            if State.Step not in Executor.Start'Range then
               raise Program_Error;  --  List is completed
            else
               Action := To_Action (Executor.Start (State.Step));
            end if;
         when others =>
            raise Program_Error;
      end case;

      if not Ok then
         State.Joining := (Kind => Crashed);
      end if;
   end Process;

end SDPCM.Generic_IO;
