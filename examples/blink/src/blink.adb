--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;
with Interfaces;
with RP.Timer;
with System;

with Pico;

with RP.Device;
with RP.Clock;

with Picowi.PIO_SPI;

with SDPCM.Generic_Mapped_Read_Resource;
with SDPCM.Generic_IO;

procedure Blink is

   --  Bus interface implementaion
   -------------------------------

   package SPI_Bus is new SDPCM.Generic_Bus
     (Read_Backplane_Register  => Picowi.PIO_SPI.gSPI.Read_Backplane_Register,
      Write_Backplane_Register => Picowi.PIO_SPI.gSPI.Write_Backplane_Register,
      Write_Prefix_Length      => Picowi.PIO_SPI.gSPI.Word'Length,
      Write_Prefix             => Picowi.PIO_SPI.gSPI.Write_Prefix,
      Start_Writing_WLAN       => Picowi.PIO_SPI.gSPI.Write_WLAN,
      Start_Reading_WLAN       => Picowi.PIO_SPI.gSPI.Read_WLAN,
      Write_Backplane          => Picowi.PIO_SPI.gSPI.Write_Backplane,
      Is_Ready                 => Picowi.PIO_SPI.gSPI.Is_Ready,
      Available_Packet_Length  => Picowi.PIO_SPI.gSPI.Available_Packet_Length,
      Clear_Error              => Picowi.PIO_SPI.gSPI.Clear_Error);

   --  Timeout interface implementation
   ------------------------------------
   use type RP.Timer.Time;

   function New_Timeout (Second : Natural) return RP.Timer.Time is
     (RP.Timer.Clock + RP.Timer.Ticks_Per_Second * RP.Timer.Time (Second));

   function Is_Expired (Value : RP.Timer.Time) return Boolean is
     (Value < RP.Timer.Clock);

   package Timeouts is new SDPCM.Generic_Timeouts
     (Timeout     => RP.Timer.Time,
      New_Timeout => New_Timeout,
      Is_Expired  => Is_Expired);

   --  Resource interface implementation
   -------------------------------------

   procedure Read_Resource is new SDPCM.Generic_Mapped_Read_Resource
     (Address         => System'To_Address (16#101c0000#),
      Firmware_Length => 224256,
      NVRAM_Length    => 16#300#,
      CLM_Blob_Length => 988);

   --  Network interface implementation
   ------------------------------------
   function SSID return String is ("guest");

   function Password return String is ("guest123");

   package Network is new SDPCM.Generic_Network
     (SSID, Password, Security => SDPCM.WPA2_AES);

   package SDPCM_IO is new SDPCM.Generic_IO
     (Bus           => SPI_Bus,
      Timeouts      => Timeouts,
      Read_Resource => Read_Resource,
      Network       => Network);

   Ok : Boolean;

   Buffer : SDPCM.Buffer_Byte_Array (1 .. 1600);
   State  : SDPCM_IO.State;
   Prev   : RP.Timer.Time;
begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;

   Picowi.PIO_SPI.Configure_GPIO (Power_On => True);

   for J in 1 .. 4 loop
      RP.Device.Timer.Delay_Milliseconds (2);
      Picowi.PIO_SPI.gSPI.Detect_Chip (Ok);
      exit when Ok;
   end loop;

   pragma Assert (Ok);

   Picowi.PIO_SPI.gSPI.Switch_Endian (Ok);
   pragma Assert (Ok);

   Prev := RP.Timer.Clock;

   loop
      declare
         use all type SDPCM_IO.Action_Kind;
         Action : SDPCM_IO.Action;
      begin
         SDPCM_IO.Process
           (State,
            Buffer,
            From => 1,
            To => 0,
            Action => Action);

         if Action.Kind = Process_Packet then
            Ada.Text_IO.Put_Line
              ("Got " & Integer'Image (Action.To - Action.From + 1));
         elsif RP.Timer.Clock - Prev > RP.Timer.Ticks_Per_Second then
            Prev := RP.Timer.Clock;

            SDPCM_IO.Set_GPIO
              (Interfaces.Unsigned_32
                 (Prev / RP.Timer.Ticks_Per_Second mod 2));
            --  Turn LED ON or OFF
         end if;

         case Action.Kind is
            when SDPCM_IO.Idle =>
               RP.Device.Timer.Delay_Milliseconds (1);
            when SDPCM_IO.Continue =>
               null;
            when SDPCM_IO.Sleep =>
               RP.Device.Timer.Delay_Milliseconds (Action.Milliseconds);
            when SDPCM_IO.Complete_IO =>
               null;  --  We use synchronous I/O in this demo. Do nothing
            when SDPCM_IO.Process_Packet =>
               null;
         end case;
      end;
   end loop;
end Blink;
