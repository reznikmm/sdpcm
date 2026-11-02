--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  Resources located in the memory mapped area (for instance, in the MCU
--  flash): firmware, then NVRAM, then CLM blob without gaps.

with System;

generic
   Address         : System.Address;
   --  Address of the firmware, NVRAM and CLM blob follow it

   Firmware_Length : Positive;
   NVRAM_Length    : Positive;
   CLM_Blob_Length : Natural;
package SDPCM.Generic_Mapped_Resources is

   procedure Read
     (Kind   : Resource_Kind;
      Offset : Natural;
      Data   : out Byte_Array;
      Last   : out Natural);
   --  Copy a part of the resource into Data

   function Length (Kind : Resource_Kind) return Natural is
     (case Kind is
         when Firmware => Firmware_Length,
         when NVRAM    => NVRAM_Length,
         when CLM_Blob => CLM_Blob_Length);

   package Resources is new SDPCM.Generic_Resources (Read, Length);
   --  Instance to pass to SDPCM.Generic_IO

end SDPCM.Generic_Mapped_Resources;
