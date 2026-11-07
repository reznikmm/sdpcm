--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Interrupts.Names;
with Ada.Synchronous_Task_Control;
with System;

with RP.DMA;

with Picowi.PIO_SPI;

package body Enet_Picowi.DMA_Interrupt is

   Transfer_Done : Ada.Synchronous_Task_Control.Suspension_Object;
   --  Set by the interrupt handler, the receiver task waits on it

   protected Device
     with Interrupt_Priority => System.Interrupt_Priority'First
   is
   private
      procedure Handler;
      pragma Attach_Handler
        (Handler, Ada.Interrupts.Names.DMA_IRQ_0_Interrupt_CPU_1);
   end Device;

   ------------
   -- Device --
   ------------

   protected body Device is

      -------------
      -- Handler --
      -------------

      procedure Handler is
      begin
         RP.DMA.Ack_IRQ (Picowi.PIO_SPI.DMA_Channel, 0);
         Ada.Synchronous_Task_Control.Set_True (Transfer_Done);
      end Handler;

   end Device;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (Transfer_Done);
   end Wait;

end Enet_Picowi.DMA_Interrupt;
