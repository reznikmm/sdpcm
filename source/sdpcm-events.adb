--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package body SDPCM.Events is

      -------------
      -- To_Mask --
      -------------

      function To_Mask (List : Event_Array) return Event_Mask is
      begin
         return Mask : Event_Mask := (others => False) do
            for Event of List loop
               Mask (Event) := True;
            end loop;
         end return;
      end To_Mask;

      -----------------------
      -- To_Raw_Event_Mask --
      -----------------------

      function To_Raw_Event_Mask (Mask : Event_Mask) return Raw_Event_Mask is
         Raw : Raw_Event_Mask := (others => 0);

         Copy : Event_Mask
           with Import, Address => Raw (5)'Address;
      begin
         Copy := Mask;

         return Raw;
      end To_Raw_Event_Mask;

end SDPCM.Events;
