--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Pico;

with RP.Device;
with RP.Clock;

with Picowi.PIO_SPI;

procedure Blink is
   Ok : Boolean;
begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;

   Picowi.PIO_SPI.Configure_GPIO (Power_On => True);

   for J in 1 .. 4 loop
      RP.Device.Timer.Delay_Milliseconds (2);
      Picowi.PIO_SPI.gSPI.Detect_Chip (Ok);
      exit when Ok;
   end loop;

   pragma Assert (Ok);

   Picowi.PIO_SPI.gSPI.Switch_Endian (Ok);
   pragma Assert (Ok);

   loop
      RP.Device.Timer.Delay_Milliseconds (1);
   end loop;
end Blink;
