--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

package body SDPCM.Generic_IO is

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
               Firmware : Positive;

            when Wait_Any_Event | Clear_Error =>
               null;

            when Sleep =>
               Milliseconds : Natural;
         end case;
      end record;

      type Step_Array is array (Positive range <>) of Step;

      procedure Execute
        (Steps        : Step_Array;
         Index        : in out Positive;
         Success      : in out Boolean;
         Custom_Value : Interfaces.Unsigned_32 := 0);

   end Executor;

   package body Executor is

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Steps        : Step_Array;
         Index        : in out Positive;
         Success      : in out Boolean;
         Custom_Value : Interfaces.Unsigned_32 := 0)
      is
         Step renames Steps (Index);
      begin
         case Step.Kind is
            when Write_Register =>
               Bus.Write_Backplane_Register
                 (Step.Address, Step.Length, Step.Value);

            when Write_Register_Variable =>
               Bus.Write_Backplane_Register
                 (Step.Address, Step.Length, Custom_Value);

            when others =>
               null;
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
      Action : out SDPCM.Generic_IO.Action) is
   begin
      case State.Joining.Kind is
         when Boot_Up =>
            null;
         when others =>
            null;
      end case;
   end Process;

end SDPCM.Generic_IO;
