--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with SDPCM.Generic_SPI;

package Picowi.PIO_SPI is

   procedure Configure_GPIO (Power_On : Boolean);
   procedure Power_On;
   procedure Configure_PIO;  --  After Power_On and a delay

   procedure Chip_Select (On : Boolean);
   procedure Read (Data : out SDPCM.Byte_Array);
   procedure Write (Data : SDPCM.Byte_Array);

   package gSPI is new SDPCM.Generic_SPI
     (Chip_Select => Chip_Select,
      Read        => Read,
      Write       => Write);

end Picowi.PIO_SPI;
