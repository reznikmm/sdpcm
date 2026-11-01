--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  Debug output to USART2 (PA2/PA3, 115200). Safe to call from several tasks.

package Console is

   procedure Initialize;

   procedure Put_Line (Text : String);

end Console;
