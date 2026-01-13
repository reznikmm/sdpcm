--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

generic
   with procedure Chip_Select (On : Boolean);
   with procedure Read (Data : out Buffer_Byte_Array);
   with procedure Write (Data : Buffer_Byte_Array);
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

   subtype Word is Buffer_Byte_Array (1 .. 4);

   function Write_Prefix
     (Bus_Function : SDPCM.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return Word
     with Inline;

   procedure Write_Backplane
     (Address : Interfaces.Unsigned_32;
      Value   : Buffer_Byte_Array);

   function Has_Event return Boolean;

   function Available_Packet_Length return Interfaces.Unsigned_32;

   function Is_Ready_To_Send return Boolean;

   procedure Clear_Error;

end SDPCM.Generic_SPI;
