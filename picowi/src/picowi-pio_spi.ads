--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with RP.DMA;

with SDPCM.Generic_SPI;

package Picowi.PIO_SPI is

   procedure Configure_GPIO (Power_On : Boolean);
   procedure Power_On;
   procedure Configure_PIO;  --  After Power_On and a delay

   DMA_Channel : constant RP.DMA.DMA_Channel_Id := 11;
   --  DMA channel used for WLAN frames. Enable its IRQ to get notified
   --  about the end of a transfer, or poll Is_DMA_Busy.

   procedure Chip_Select (On : Boolean);
   procedure Read (Data : out SDPCM.Buffer_Byte_Array);
   procedure Write (Data : SDPCM.Buffer_Byte_Array);

   procedure Start_Read (Data : out SDPCM.Buffer_Byte_Array);
   procedure Start_Write (Data : SDPCM.Buffer_Byte_Array);
   procedure Finish_Transfer;

   function Is_DMA_Busy return Boolean;
   --  DMA transfer is in progress

   package gSPI is new SDPCM.Generic_SPI
     (Chip_Select     => Chip_Select,
      Read            => Read,
      Write           => Write,
      Start_Read      => Start_Read,
      Start_Write     => Start_Write,
      Finish_Transfer => Finish_Transfer);

end Picowi.PIO_SPI;
