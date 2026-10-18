--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  SDIO bus layer for Broadcom WiFi chips (BCM43362 and similar).
--
--  The package implements the bus interface expected by SDPCM.Generic_Bus
--  on top of two SDIO commands provided by a host controller driver:
--  CMD52 (IO_RW_DIRECT) and CMD53 (IO_RW_EXTENDED). It doesn't depend on
--  any particular MCU.
--
--  The host is responsible for the card identification (CMD0, CMD5, CMD3,
--  CMD7) before calling Initialize. After Switch_To_Four_Bit_Bus the host
--  should switch its own bus width too.
--
--  All operations are synchronous. Start_Writing_WLAN/Start_Reading_WLAN
--  of SDPCM.Generic_Bus are complete when the procedures return.

with Interfaces;
with System;

generic
   with procedure IO_Read_Write_Direct
     (Argument : Interfaces.Unsigned_32;
      Response : out Interfaces.Unsigned_32;
      Success  : out Boolean);
   --  Send CMD52 (IO_RW_DIRECT) and return its R5 response.

   with procedure IO_Read_Write_Extended
     (Argument   : Interfaces.Unsigned_32;
      Block_Size : Positive;
      Length     : Positive;
      Data       : System.Address;
      Response   : out Interfaces.Unsigned_32;
      Success    : out Boolean);
   --  Send CMD53 (IO_RW_EXTENDED) and transfer Length bytes from/to Data.
   --  Bit 31 of Argument defines the direction (1 means writing to the
   --  card). Block_Size is a power of two in 4 .. 64 and Length is a
   --  multiple of Block_Size. Data is word aligned. In byte mode (bit 27 is
   --  0) Block_Size = Length.

   with procedure Sleep (Milliseconds : Positive);
   --  Wait for given time. Used only by Initialize.

package SDPCM.Generic_SDIO is
   pragma Preelaborate;

   Block_Size : constant := 64;
   --  Block size for functions 0, 1 and 2

   procedure Initialize (Success : out Boolean);
   --  Bring the backplane up: enable function 1, set block sizes, enable
   --  interrupts, wait for the ALP clock, disable extra pull-ups and enable
   --  function 2. Call it after the card has been selected with CMD7.

   procedure Switch_To_Four_Bit_Bus (Success : out Boolean);
   --  Configure the card for 4-bit data bus. The host should change its bus
   --  width after this call.

   function Has_Failed return Boolean;
   --  Some bus operation failed since the last Clear_Error call

   --  Bus interface implementation (see SDPCM.Generic_Bus)

   procedure Read_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : out Interfaces.Unsigned_32);

   procedure Write_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : Interfaces.Unsigned_32);

   Write_Prefix_Length : constant := 0;
   --  SDIO commands carry the address, no data prefix is needed

   pragma Warnings (Off, "is not referenced");
   function Write_Prefix
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return Buffer_Byte_Array is
        (1 .. 0 => <>);
   pragma Warnings (Off, "is not referenced");
   --  Return an empty array. We need this dummy function to instantiate
   --  Generic_BusGeneric_Bus generic.

   procedure Write_Backplane
     (Address : Interfaces.Unsigned_32;
      Value   : Buffer_Byte_Array);
   --  Write Value to the backplane window at Address

   procedure Write_WLAN (Value : Buffer_Byte_Array);
   --  Send a frame to function 2. The transfer is padded up to the block
   --  size (or to a power of two for short frames), so up to 63 bytes after
   --  Value'Last are read and sent too. The device ignores them, because
   --  the frame length is in the SDPCM header.

   procedure Read_WLAN (Value : out Buffer_Byte_Array);
   --  Read a frame from function 2. Value'Length should be the length
   --  returned by Available_Packet_Length.

   function Available_Packet_Length return Interfaces.Unsigned_32;
   --  Read the 4-byte frame tag (length and its complement) from function 2
   --  and return the frame length, or 0 if there is no frame. The tag is
   --  kept and returned as the first 4 bytes by the next Read_WLAN call.

   function Is_Ready return Boolean;
   --  Function 2 is ready (CCCR I/O Ready register)

   procedure Clear_Error;
   --  Reset Has_Failed flag

end SDPCM.Generic_SDIO;
