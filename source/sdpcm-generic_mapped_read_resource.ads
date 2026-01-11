--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with System;

generic
   Address         : System.Address;
   Firmware_Length : Positive;
   NVRAM_Length    : Positive;
   CLM_Blob_Length : Positive;
procedure SDPCM.Generic_Mapped_Read_Resource
  (Kind   : Resource_Kind;
   Offset : Natural;
   Data   : out Byte_Array;
   Last   : out Natural);
