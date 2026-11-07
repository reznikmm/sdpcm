--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  ENet network interface on top of SDPCM driver for BCM43362 connected to
--  the STM32F4 SDIO host controller. Register access uses the polling SDIO
--  variant, WLAN frames are transferred by DMA (DMA2 Stream 3).
--
--  The package owns the SDIO peripheral and DMA2 Stream 3, so instantiate
--  it only once.
--  Call Create, then start a Net.Generic_Receiver task for the interface.
--  The receiver task runs the driver state machine inside Receive.

with Ada.Real_Time;
with Interfaces;
with System;

with Net.Buffers;
with Net.Interfaces;

with SDPCM.Generic_IO;
with SDPCM.Generic_SDIO;

with STM32;

generic
   Clock_Pin   : STM32.Pin := (STM32.PB, 15);
   --  SDIO_CK pin: (PC, 12) or (PB, 15) on STM32F411

   Command_Pin : STM32.Pin := (STM32.PA, 6);
   --  SDIO_CMD pin: (PD, 2) or (PA, 6) on STM32F411

   Data_Pins   : STM32.Pin_Array :=
     ((STM32.PB, 7), (STM32.PA, 8), (STM32.PA, 9), (STM32.PB, 5));
   --  SDIO_D0 .. SDIO_D3 pins in this order

   Reset_Pin   : STM32.Pin := (STM32.PB, 14);
   --  WL_REG_ON/reset pin of the WiFi chip

   DMA_Priority : System.Interrupt_Priority := System.Interrupt_Priority'First;
   --  Priority of SDIO and DMA interrupt handlers

   Bus_Speed   : Positive := 24_000_000;
   --  SDIO_CK frequency (Hz) after card identification. Register access and
   --  firmware upload use polling without hardware flow control, so keep it
   --  moderate.

   with package Resources is new SDPCM.Generic_Resources (<>);
   --  Firmware and NVRAM (BCM43362 has no CLM blob)

   with package Network is new SDPCM.Generic_Network (<>);
   --  Generic SSID/Password access interface

   Send_Queue_Length : Positive := 4;
   --  Maximum number of packets waiting for sending. Send drops a packet
   --  when the queue is full, so the network stack never runs out of
   --  buffers for receiving.

package Enet_BCM43362.Generic_Interface is

   type Ifnet (Priority : System.Priority := System.Default_Priority) is
     new Net.Interfaces.Ifnet_Type with private;

   procedure Create (Self : in out Ifnet'Class; Success : out Boolean);
   --  Reset the chip, identify the SDIO card, switch to 4-bit bus and check
   --  the chip id. Return Success = False on failure. The firmware is loaded
   --  later by the Receive procedure.

   overriding procedure Send
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type);
   --  Queue the packet. It is sent by the receiver task.

   overriding procedure Receive
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type);
   --  Drive the SDPCM state machine and send queued packets until a data
   --  packet is received.

   function Is_Starting (Self : Ifnet) return Boolean;
   --  The driver is still loading the firmware and configuring the chip

   function Is_Joined (Self : Ifnet) return Boolean;
   --  The interface is joined to the WiFi network

   function Is_Failed (Self : Ifnet) return Boolean;
   --  Join has failed, the driver will retry later

private

   --  SDIO host interface implementation
   --------------------------------------

   procedure Read_Write_Direct
     (Argument : Interfaces.Unsigned_32;
      Response : out Interfaces.Unsigned_32;
      Success  : out Boolean);
   --  Send CMD52 (IO_RW_DIRECT)

   procedure Read_Write_Extended
     (Argument   : Interfaces.Unsigned_32;
      Block_Size : Positive;
      Length     : Positive;
      Data       : System.Address;
      Response   : out Interfaces.Unsigned_32;
      Success    : out Boolean);
   --  Send CMD53 (IO_RW_EXTENDED) with a data transfer

   procedure Start_Read_Write_Extended
     (Argument   : Interfaces.Unsigned_32;
      Block_Size : Positive;
      Length     : Positive;
      Data       : System.Address);
   --  Start CMD53 (IO_RW_EXTENDED) with a DMA transfer

   package SDIO_Bus is new SDPCM.Generic_SDIO
     (IO_Read_Write_Direct         => Read_Write_Direct,
      IO_Read_Write_Extended       => Read_Write_Extended,
      Start_IO_Read_Write_Extended => Start_Read_Write_Extended);

   package Bus is new SDPCM.Generic_Bus
     (Read_Backplane_Register  => SDIO_Bus.Read_Backplane_Register,
      Write_Backplane_Register => SDIO_Bus.Write_Backplane_Register,
      Write_Prefix_Length      => SDIO_Bus.Write_Prefix_Length,
      Write_Prefix             => SDIO_Bus.Write_Prefix,
      Start_Writing_WLAN       => SDIO_Bus.Write_WLAN,
      Start_Reading_WLAN       => SDIO_Bus.Read_WLAN,
      Start_Writing_Backplane  => SDIO_Bus.Write_Backplane,
      Is_Ready                 => SDIO_Bus.Is_Ready,
      Available_Packet_Length  => SDIO_Bus.Available_Packet_Length,
      Clear_Error              => SDIO_Bus.Clear_Error);

   --  Timeout interface implementation
   ------------------------------------

   use type Ada.Real_Time.Time;

   function New_Timeout (Second : Natural) return Ada.Real_Time.Time is
     (Ada.Real_Time.Clock + Ada.Real_Time.Seconds (Second));

   function Is_Expired (Value : Ada.Real_Time.Time) return Boolean is
     (Value < Ada.Real_Time.Clock);

   package Timeouts is new SDPCM.Generic_Timeouts
     (Timeout     => Ada.Real_Time.Time,
      New_Timeout => New_Timeout,
      Is_Expired  => Is_Expired);

   package SDPCM_IO is new SDPCM.Generic_IO
     (Bus           => Bus,
      Timeouts      => Timeouts,
      Resources     => Resources,
      Network       => Network,
      Chip          => SDPCM.BCM43362);

   protected type Buffer_Queue
     (Priority : System.Priority := System.Default_Priority)
   is
      procedure Put
        (Packet : in out Net.Buffers.Buffer_Type;
         Done   : out Boolean);
      --  Append Packet to the queue unless the queue is full

      procedure Get (Packet : in out Net.Buffers.Buffer_Type);
      --  Take the first packet, if any
   private
      pragma Priority (Priority);

      List  : Net.Buffers.Buffer_List;
      Count : Natural := 0;
   end Buffer_Queue;

   type Ifnet (Priority : System.Priority := System.Default_Priority) is
     new Net.Interfaces.Ifnet_Type with
   record
      Send_Queue : Buffer_Queue (Priority);
      State      : SDPCM_IO.State;
      Has_MAC    : Boolean := False;
   end record;

   function Is_Starting (Self : Ifnet) return Boolean is
     (SDPCM_IO.Is_Starting (Self.State));

   function Is_Joined (Self : Ifnet) return Boolean is
     (SDPCM_IO.Is_Joinded (Self.State));

   function Is_Failed (Self : Ifnet) return Boolean is
     (SDPCM_IO.Is_Failed (Self.State));

end Enet_BCM43362.Generic_Interface;
