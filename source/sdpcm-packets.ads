--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;

with SDPCM.IOCTL;

package SDPCM.Packets is
   pragma Preelaborate;

   subtype Frame_Tag is Interfaces.Unsigned_32;

   function Make_Tag (Length : Interfaces.Unsigned_16) return Frame_Tag;
   function Is_Valid (Tag : Frame_Tag) return Boolean;

   type BDC_Header is record
      Flags    : Interfaces.Unsigned_8;
      Priority : Interfaces.Unsigned_8;
      Flags2   : Interfaces.Unsigned_8;
      Offset   : Natural range 0 .. 255;
   end record;

   for BDC_Header use record
      Flags    at 0 range 0 .. 7;
      Priority at 1 range 0 .. 7;
      Flags2   at 2 range 0 .. 7;
      Offset   at 3 range 0 .. 7;
   end record;

   type SDPCM_Channel is new Interfaces.Unsigned_8;

   Control : constant SDPCM_Channel := 0;
   Event   : constant SDPCM_Channel := 1;
   Data    : constant SDPCM_Channel := 2;

   subtype Event_Or_Data is SDPCM_Channel range Event .. Data;

   type SDPCM_Header is record
      Tag      : Frame_Tag;
      Sequence : Interfaces.Unsigned_8;
      Channel  : SDPCM_Channel;
      Next_Len : Interfaces.Unsigned_8;
      Hdr_Len  : Natural range 0 .. 255;  --  SDPCM header plus any padding
      Flow     : Interfaces.Unsigned_8;
      Credit   : Interfaces.Unsigned_8;
      Reserved : Interfaces.Unsigned_16;
   end record;

   for SDPCM_Header use record
      Tag      at 0 range 0 .. 31;
      Sequence at 4 range 0 .. 7;
      Channel  at 5 range 0 .. 7;
      Next_Len at 6 range 0 .. 7;
      Hdr_Len  at 7 range 0 .. 7;
      Flow     at 8 range 0 .. 7;
      Credit   at 9 range 0 .. 7;
      Reserved at 10 range 0 .. 15;
   end record;

   type Packet (Channel : SDPCM_Channel := 0) is record
      case Channel is
         when Control =>
            IOCTL_Header : IOCTL.IOCTL_Header;
            IOCTL_Offset : Natural;
         when Event | Data =>
            Offset       : Natural;
         when others =>
            null;
      end case;
   end record;

   procedure Decode_Input
     (Input  : Buffer_Byte_Array;
      Result : out Packet);

private
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   function Make_Tag (Length : Interfaces.Unsigned_16) return Frame_Tag is
     (Interfaces.Shift_Left (Interfaces.Unsigned_32 (not Length), 16) +
      Interfaces.Unsigned_32 (Length));

   function Is_Valid (Tag : Frame_Tag) return Boolean is
     (((Interfaces.Shift_Right (Tag, 16) xor Tag) and 16#FFFF#) = 16#FFFF#);

end SDPCM.Packets;
