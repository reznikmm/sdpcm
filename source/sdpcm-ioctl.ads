--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

with SDPCM.Events;

package SDPCM.IOCTL is
   pragma Preelaborate;

   type Command is new Interfaces.Unsigned_32;

   UP           : constant Command := 2;
   DOWN         : constant Command := 3;
   SET_INFRA    : constant Command := 20;
   SET_AUTH     : constant Command := 22;
   SET_SSID     : constant Command := 26;
   SET_ANTDIV   : constant Command := 64;
   SET_GMODE    : constant Command := 110;
   SET_WSEC     : constant Command := 134;
   SET_BAND     : constant Command := 142;
   SET_WPA_AUTH : constant Command := 165;
   GET_VAR      : constant Command := 262;
   SET_VAR      : constant Command := 263;
   SET_WSEC_PMK : constant Command := 268;

   generic
      with function Data_Offset return Positive;

      with procedure Set_In_Place
        (Buffer  : in out Buffer_Byte_Array;
         Command : IOCTL.Command;
         Write   : Boolean);

   procedure Set
     (Buffer  : in out Buffer_Byte_Array;
      Command : IOCTL.Command;
      Name    : Byte_Array;
      Data    : Byte_Array;
      Write   : Boolean);

   generic
      with package Bus is new SDPCM.Generic_Bus (<>);
   procedure Set_In_Place
     (Buffer  : in out Buffer_Byte_Array;
      Command : IOCTL.Command;
      Write   : Boolean);

   function Data_Offset (Write_Prefix_Length : Natural) return Positive;

   type IOCTL_Header is record
      Command    : IOCTL.Command;
      Out_Length : Interfaces.Unsigned_16;
      In_Length  : Interfaces.Unsigned_16;
      Flags      : Interfaces.Unsigned_32;
      Status     : Interfaces.Unsigned_32;
   end record;

   for IOCTL_Header use record
      Command    at 0 range 0 .. 31;
      Out_Length at 4 range 0 .. 15;
      In_Length  at 6 range 0 .. 15;
      Flags      at 8 range 0 .. 31;
      Status     at 12 range 0 .. 31;
   end record;

   type IO_Variable is
     (None,
      country,
      bus_txglom,
      apsta,
      ampdu_ba_wsize,
      ampdu_mpdu,
      ampdu_rx_factor,
      bsscfg_event_msgs,
      mcast_list,
      pm2_sleep_ret,
      bcn_li_bcn,
      bcn_li_dtim,
      assoc_listen,
      bsscfg_sup_wpa,
      bsscfg_sup_wpa2_eapver,
      bsscfg_sup_wpa_tmo,
      cur_etheraddr);

   subtype Output_Variable is IO_Variable
   range None .. bsscfg_sup_wpa_tmo;

   function To_Name (Name : IO_Variable) return String is
     (case Name is
         when None                   => "",
         when country                => "country",
         when cur_etheraddr          => "cur_etheraddr",
         when bus_txglom             => "bus:txglom",
         when apsta                  => "apsta",
         when ampdu_ba_wsize         => "ampdu_ba_wsize",
         when ampdu_mpdu             => "ampdu_mpdu",
         when ampdu_rx_factor        => "ampdu_rx_factor",
         when bsscfg_event_msgs      => "bsscfg:event_msgs",
         when mcast_list             => "mcast_list",
         when pm2_sleep_ret          => "pm2_sleep_ret",
         when bcn_li_bcn             => "bcn_li_bcn",
         when bcn_li_dtim            => "bcn_li_dtim",
         when assoc_listen           => "assoc_listen",
         when bsscfg_sup_wpa         => "bsscfg:sup_wpa",
         when bsscfg_sup_wpa2_eapver => "bsscfg:sup_wpa2_eapver",
         when bsscfg_sup_wpa_tmo     => "bsscfg:sup_wpa_tmo")
        with Static;

   function To_Raw_Name (Name : IO_Variable) return Byte_Array is
     (if Name = None then []
      else [for X of To_Name (Name) => Character'Pos (X)] & 0);

   XX_Country : constant Byte_Array (1 .. 20) :=
     [16#58#, 16#58#, 16#00#, 16#00#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#58#, 16#58#, others => 16#00#];
   --  "XX\x00\x00\xFF\xFF\xFF\xFFXX"

   Multicast_List : constant Byte_Array (1 .. 6 * 10) :=
     [1, 0, 0, 0,
      16#01#, 16#00#, 16#5E#, 16#00#, 16#00#, 16#FB#,
      others => 0];

   SUP_WPA : constant Byte_Array (1 .. 8) :=
     [16#00#, 16#00#, 16#00#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#];

   SUP_WPA2_EAPVER : constant Byte_Array (1 .. 8) :=
     [16#00#, 16#00#, 16#00#, 16#00#, 16#FF#, 16#FF#, 16#FF#, 16#FF#];

   SUP_WPA_TMO : constant Byte_Array (1 .. 8) :=
     [16#00#, 16#00#, 16#00#, 16#00#, 16#C4#, 16#09#, 16#00#, 16#00#];

   function Raw_Value
     (Name    : Output_Variable;
      Command : IOCTL.Command) return Byte_Array is
     (case Command is
         when IOCTL.UP => [],
         when others =>
        (case Name is
            when country => XX_Country,
            when bsscfg_event_msgs =>
               Events.To_Raw_Event_Mask (Events.To_Mask (Events.Join_Events)),
            when mcast_list => Multicast_List,
            when bsscfg_sup_wpa => SUP_WPA,
            when bsscfg_sup_wpa2_eapver => SUP_WPA2_EAPVER,
            when bsscfg_sup_wpa_tmo => SUP_WPA_TMO,
            when others => raise Program_Error));

   function Encode
     (Value : String; X : Interfaces.Unsigned_8) return Byte_Array;

end SDPCM.IOCTL;
