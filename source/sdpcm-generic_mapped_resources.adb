--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package body SDPCM.Generic_Mapped_Resources is

   ----------
   -- Read --
   ----------

   procedure Read
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

      To : constant Natural :=
        (case Kind is
            when Firmware => Firmware_Length,
            when NVRAM    => Firmware_Length + NVRAM_Length,
            when CLM_Blob => Total);

      Count : constant Natural :=
        Integer'Max (0, Integer'Min (To - From + 1, Data'Length));
   begin
      Last := Data'First + Count - 1;
      Data (Data'First .. Last) := Raw (From .. From + Count - 1);
   end Read;

end SDPCM.Generic_Mapped_Resources;
