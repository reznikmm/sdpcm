--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  DMA interrupt handler for Picowi.PIO_SPI.DMA_Channel. It is a separate
--  library package, because GNAT doesn't allow Attach_Handler in a generic.

package Enet_Picowi.DMA_Interrupt is

   procedure Wait;
   --  Wait for the end of a DMA transfer. Only one task should call it.

end Enet_Picowi.DMA_Interrupt;
