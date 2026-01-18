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

end SDPCM.Events;
