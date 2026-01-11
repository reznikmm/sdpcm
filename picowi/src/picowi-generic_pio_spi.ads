--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with HAL;

with RP.GPIO;
with RP.PIO;

generic
   WL_CLK : in out RP.GPIO.GPIO_Point;
   WL_D   : in out RP.GPIO.GPIO_Point;
   WL_CS  : in out RP.GPIO.GPIO_Point;
   WL_ON  : in out RP.GPIO.GPIO_Point;

   P  : in out RP.PIO.PIO_Device;
   SM : RP.PIO.PIO_SM;
package Picowi.Generic_PIO_SPI is
   pragma Preelaborate;

   procedure Configure_GPIO;
   --  Configure GPIO pins and power OFF the WLAN chip

   procedure Power_On;
   --  You can Power On only after a delay since Power_Off/Configure_GPIO.

   procedure Configure_PIO;
   --  You can configure PIO only after a few milliseconds since Power_On

   procedure Chip_Select (On : Boolean);
   procedure Write_SPI (Data : HAL.UInt8_Array);
   procedure Read_SPI (Data : out HAL.UInt8_Array);

end Picowi.Generic_PIO_SPI;
