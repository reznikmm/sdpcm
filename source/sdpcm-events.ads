--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;

package SDPCM.Events is
   pragma Pure;

   type Event is new Interfaces.Unsigned_8;

   JOIN          : constant Event := 1;
   ASSOC         : constant Event := 7;
   REASSOC       : constant Event := 9;
   ASSOC_REQ_IE  : constant Event := 87;
   ASSOC_RESP_IE : constant Event := 88;
   SET_SSID      : constant Event := 0;
   LINK          : constant Event := 16;
   AUTH          : constant Event := 3;
   PSK_SUP       : constant Event := 46;
   EAPOL_MSG     : constant Event := 25;
   DISASSOC_IND  : constant Event := 12;

   Last_Event : constant Event := 208;

   type Event_Mask is array (Event range 0 .. Last_Event) of Boolean
     with Pack, Alignment => 1;

   type Event_Array is array (Positive range <>) of Event;

   Join_Events : constant Event_Array :=
     (JOIN,
      ASSOC,
      REASSOC,
      ASSOC_REQ_IE,
      ASSOC_RESP_IE,
      SET_SSID,
      LINK,
      AUTH,
      PSK_SUP,
      EAPOL_MSG,
      DISASSOC_IND);

   function To_Mask (List : Event_Array) return Event_Mask;

   subtype Raw_Event_Mask is Byte_Array (1 .. Event_Mask'Size / 8 + 4);

   function To_Raw_Event_Mask (Mask : Event_Mask) return Raw_Event_Mask;

   type ETHER_HDR is record
      Dest_Addr : Byte_Array (1 .. 6);
      Srce_Addr : Byte_Array (1 .. 6);
      Tipe      : Interfaces.Unsigned_16;
   end record
     with Pack;

   type BCMETH_HDR is record
      Subtipe     : Interfaces.Unsigned_16;
      Len         : Interfaces.Unsigned_16;
      Ver         : Interfaces.Unsigned_8;
      Oui         : Byte_Array (1 .. 3);
      Usr_Subtype : Interfaces.Unsigned_16;
   end record
     with Pack;

   type EVENT_HDR is record
      Ver        : Interfaces.Unsigned_16;
      Flags      : Interfaces.Unsigned_16;
      Event_Type : Interfaces.Unsigned_32;
      Status     : Interfaces.Unsigned_32;
      Reason     : Interfaces.Unsigned_32;
      Auth_Type  : Interfaces.Unsigned_32;
      Datalen    : Interfaces.Unsigned_32;
      Addr       : Byte_Array (1 .. 6);
      Ifname     : Byte_Array (1 .. 16);
      Ifidx      : Interfaces.Unsigned_8;
      Bsscfgidx  : Interfaces.Unsigned_8;
   end record
     with Pack;

   type Event_Record is record
      Ether  : ETHER_HDR;
      Bcmeth : BCMETH_HDR;
      Eventh : EVENT_HDR;
   end record
     with Pack;

end SDPCM.Events;
