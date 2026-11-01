--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with STM32.Polling.USART_2;

package body Console is

   package UART renames STM32.Polling.USART_2;

   protected Lock is
      procedure Put_Line (Text : String);
   end Lock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      UART.Configure
        (TX   => (STM32.PA, 2),
         RX   => (STM32.PA, 3),
         Rate => 115_200);
   end Initialize;

   ----------
   -- Lock --
   ----------

   protected body Lock is

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (Text : String) is
      begin
         UART.Put (Text & ASCII.CR & ASCII.LF);
      end Put_Line;

   end Lock;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Lock.Put_Line (Text);
   end Put_Line;

end Console;
