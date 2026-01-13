--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with HAL;

with Pico;
with RP.Device;
with RP.GPIO;
with Picowi.Generic_PIO_SPI;

package body Picowi.PIO_SPI is

   GP29 : RP.GPIO.GPIO_Point := (Pin => 29);

   package SPI is new Picowi.Generic_PIO_SPI
     (WL_ON  => Pico.GP23,
      WL_D   => Pico.GP24,
      WL_CS  => Pico.GP25,
      WL_CLK => GP29,
      P      => RP.Device.PIO_0,
      SM     => 0);

   procedure Chip_Select (On : Boolean) renames SPI.Chip_Select;
   procedure Power_On renames SPI.Power_On;
   procedure Configure_PIO renames SPI.Configure_PIO;

   --------------------
   -- Configure_GPIO --
   --------------------

   procedure Configure_GPIO (Power_On : Boolean) is
   begin
      SPI.Configure_GPIO;

      if Power_On then
         RP.Device.Timer.Delay_Milliseconds (100);
         SPI.Power_On;
         RP.Device.Timer.Delay_Milliseconds (50);
         SPI.Configure_PIO;
      end if;
   end Configure_GPIO;

   ----------
   -- Read --
   ----------

   procedure Read (Data : out SDPCM.Buffer_Byte_Array) is
      Raw : HAL.UInt8_Array (Data'Range)
        with Import, Address => Data'Address;
   begin
      SPI.Read_SPI (Raw);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Data : SDPCM.Buffer_Byte_Array) is
      Raw : HAL.UInt8_Array (Data'Range)
        with Import, Address => Data'Address;
   begin
      SPI.Write_SPI (Raw);
   end Write;

end Picowi.PIO_SPI;
