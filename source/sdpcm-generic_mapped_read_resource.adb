--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

procedure SDPCM.Generic_Mapped_Read_Resource
  (Kind   : Resource_Kind;
   Offset : Natural;
   Data   : out Byte_Array;
   Last   : out Natural)
is
   Total : constant Positive :=
     Firmware_Length + NVRAM_Length + CLM_Blob_Length;

   Raw : SDPCM.Byte_Array (1 .. Total)
     with Import, Address => Address;

   From : constant Positive := Offset +
     (case Kind is
         when Firmware => 1,
         when NVRAM    => Firmware_Length + 1,
         when CLM_Blob => Firmware_Length + NVRAM_Length + 1);

   To : constant Positive :=
     (case Kind is
         when Firmware => Firmware_Length,
         when NVRAM    => Firmware_Length + NVRAM_Length,
         when CLM_Blob => Total);

   Length : constant Natural := Natural'Min (To - From + 1, Data'Length);

begin
   Last := Data'First + Length - 1;
   Data (Data'First .. Last) := Raw (From .. From + Length - 1);
end SDPCM.Generic_Mapped_Read_Resource;
