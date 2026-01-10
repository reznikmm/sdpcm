--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

generic
   with procedure Chip_Select (On : Boolean);
   with procedure Read (Data : out Byte_Array);
   with procedure Write (Data : Byte_Array);
package SDPCM.Generic_SPI is
   pragma Pure;

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

   subtype Word is Byte_Array (1 .. 4);

   function Write_Prefix
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return Word
     with Inline;

   procedure Start_Writing_WLAN (Value : Byte_Array) is null;

   procedure Start_Reading_WLAN (Value : out Byte_Array) is null;

   procedure Write_Backplane
     (Address : Interfaces.Unsigned_32;
      Value   : Byte_Array);

   function Has_Event return Boolean;

   function Available_Packet_Length return Interfaces.Unsigned_32;

   function Is_Ready_To_Send return Boolean;

   procedure Clear_Error;

   package Bus is new SDPCM.Generic_Bus
     (Read_Backplane_Register  => Read_Backplane_Register,
      Write_Backplane_Register => Write_Backplane_Register,
      Write_Prefix_Length      => Word'Length,
      Write_Prefix             => Write_Prefix,
      Start_Reading_WLAN       => Start_Reading_WLAN,
      Start_Writing_WLAN       => Start_Writing_WLAN,
      Write_Backplane          => Write_Backplane,
      Has_Event                => Has_Event,
      Available_Packet_Length  => Available_Packet_Length,
      Is_Ready_To_Send         => Is_Ready_To_Send,
      Clear_Error              => Clear_Error);

end SDPCM.Generic_SPI;