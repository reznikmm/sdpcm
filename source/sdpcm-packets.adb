--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Unchecked_Conversion;

package body SDPCM.Packets is

   ------------------
   -- Decode_Input --
   ------------------

   procedure Decode_Input
     (Input  : Buffer_Byte_Array;
      Result : out Packet)
   is
      subtype IOCTL_Header_Raw is
        Buffer_Byte_Array (1 .. IOCTL.IOCTL_Header'Size / 8);

      function To_IOCTL_Header is new Ada.Unchecked_Conversion
        (IOCTL_Header_Raw, IOCTL.IOCTL_Header);

      subtype BDC_Header_Raw is
        Buffer_Byte_Array (1 .. BDC_Header'Size / 8);

      function To_BDC_Header is new Ada.Unchecked_Conversion
        (BDC_Header_Raw, BDC_Header);

      SDPCM : SDPCM_Header
        with Import, Address => Input'Address;
   begin
      if not Is_Valid (SDPCM.Tag) or else
        (SDPCM.Channel = Control and then
         Input'Last < SDPCM.Hdr_Len + IOCTL.IOCTL_Header'Size / 8)
        or else
          (SDPCM.Channel in Event | Data and then
           Input'Last < SDPCM.Hdr_Len + BDC_Header'Size / 8)
      then
         Result := (Channel => SDPCM_Channel'Last);
      elsif SDPCM.Channel = Control then
         declare
            Skip : constant Natural := SDPCM.Hdr_Len;

            Header : constant IOCTL.IOCTL_Header := To_IOCTL_Header
              (Input (Skip + 1 .. Skip + IOCTL.IOCTL_Header'Size / 8));

            Header_Length : constant Positive :=
              Skip + IOCTL.IOCTL_Header'Size / 8;

         begin
            Result := (Control, Header, Header_Length + 1);
         end;
      elsif SDPCM.Channel in Event .. Data then
         declare
            Skip : constant Natural := SDPCM.Hdr_Len;

            BDC : constant BDC_Header := To_BDC_Header
              (Input (Skip + 1 .. Skip + BDC_Header'Size / 8));

            Header_Length : constant Positive :=
              Skip + BDC'Size / 8 + 4 * BDC.Offset;

         begin
            if Input'Last >= Header_Length + 1 then
               Result :=
                 (Channel => Event_Or_Data (SDPCM.Channel),
                  Offset  => Header_Length + 1);
            end if;
         end;
      end if;
   end Decode_Input;

end SDPCM.Packets;
