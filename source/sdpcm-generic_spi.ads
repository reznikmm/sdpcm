--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  gSPI bus layer for CYW43439 and similar chips.
--
--  Register access is synchronous (Read, Write). Frame transfers
--  (Write_WLAN, Read_WLAN) and firmware upload (Write_Backplane) start an
--  asynchronous transfer with Start_Read/Start_Write, for instance a DMA
--  transfer. When
--  Is_Transfer_Pending is True, the host should wait for the end of the
--  transfer and call Complete_Transfer before any other call.

generic
   with procedure Chip_Select (On : Boolean);
   --  Activate/deactivate the chip select line

   with procedure Read (Data : out Buffer_Byte_Array);
   --  Read Data'Length bytes synchronously

   with procedure Write (Data : Buffer_Byte_Array);
   --  Write Data synchronously

   with procedure Start_Read (Data : out Buffer_Byte_Array);
   --  Start reading Data'Length bytes. Data should be available until
   --  Complete_Transfer.

   with procedure Start_Write (Data : Buffer_Byte_Array);
   --  Start writing Data. Data should be available until Complete_Transfer.

   with procedure Finish_Transfer;
   --  Called by Complete_Transfer when the asynchronous transfer is done,
   --  before the chip select is deactivated (for instance, wait until the
   --  last bits are on the wire).

package SDPCM.Generic_SPI is
   pragma Preelaborate;

   procedure Detect_Chip (Success : out Boolean);

   procedure Switch_Endian (Success : out Boolean);

   --  Bus interface implementation

   procedure Read_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : out Interfaces.Unsigned_32);

   procedure Write_Backplane_Register
     (Address : Interfaces.Unsigned_32;
      Length  : Positive;
      Value   : Interfaces.Unsigned_32);

   subtype Word is Buffer_Byte_Array (1 .. 4);

   function Write_Prefix
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return Word
     with Inline;

   procedure Start_Writing_Backplane
     (Address : Interfaces.Unsigned_32;
      Value   : Buffer_Byte_Array);
   --  Start writing to the backplane (firmware upload). Value is prefixed
   --  with Write_Prefix, so Address is not used.

   procedure Write_WLAN (Value : Buffer_Byte_Array);
   --  Start writing a frame (prefixed with Write_Prefix)

   procedure Read_WLAN (Value : out Buffer_Byte_Array);
   --  Start reading a frame of Value'Length bytes

   function Is_Transfer_Pending return Boolean;
   --  An asynchronous transfer started by Write_WLAN, Read_WLAN or
   --  Write_Backplane should be completed by Complete_Transfer

   procedure Complete_Transfer
     with Pre => Is_Transfer_Pending, Post => not Is_Transfer_Pending;
   --  Finish the asynchronous transfer and deactivate the chip select

   function Available_Packet_Length return Interfaces.Unsigned_32;
   --  Check if WiFi chip is ready to send data to driver/host

   function Is_Ready return Boolean;
   --  Check if WiFi chip is ready to receive data from driver/host

   procedure Clear_Error;

end SDPCM.Generic_SPI;
