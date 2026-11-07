--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with HAL;

with RP.DMA;
with RP.GPIO;
with RP.PIO;

generic
   WL_CLK : in out RP.GPIO.GPIO_Point;
   WL_D   : in out RP.GPIO.GPIO_Point;
   WL_CS  : in out RP.GPIO.GPIO_Point;
   WL_ON  : in out RP.GPIO.GPIO_Point;

   P  : in out RP.PIO.PIO_Device;
   SM : RP.PIO.PIO_SM;

   DMA_Channel : RP.DMA.DMA_Channel_Id;
   --  DMA channel for asynchronous transfers

   TX_Trigger  : RP.DMA.DMA_Request_Trigger;
   RX_Trigger  : RP.DMA.DMA_Request_Trigger;
   --  DMA requests of TX/RX FIFO of the state machine SM of P
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

   procedure Start_Write_SPI (Data : HAL.UInt8_Array);
   --  Start writing Data by DMA. Call Finish_SPI after DMA completion.

   procedure Start_Read_SPI (Data : out HAL.UInt8_Array);
   --  Start reading Data by DMA. Call Finish_SPI after DMA completion.

   function Is_DMA_Busy return Boolean;
   --  DMA transfer is in progress

   procedure Finish_SPI;
   --  Finish the transfer started by Start_Write_SPI/Start_Read_SPI. Wait
   --  until the last bits are on the wire for a write transfer.

end Picowi.Generic_PIO_SPI;
