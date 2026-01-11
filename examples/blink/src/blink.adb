--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

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

   procedure Start_Writing_WLAN (Value : SDPCM.Byte_Array) is null;
   procedure Start_Reading_WLAN (Value : out SDPCM.Byte_Array) is null;

   package SPI_Bus is new SDPCM.Generic_Bus
     (Read_Backplane_Register  => Picowi.PIO_SPI.gSPI.Read_Backplane_Register,
      Write_Backplane_Register => Picowi.PIO_SPI.gSPI.Write_Backplane_Register,
      Write_Prefix_Length      => Picowi.PIO_SPI.gSPI.Word'Length,
      Write_Prefix             => Picowi.PIO_SPI.gSPI.Write_Prefix,
      Start_Writing_WLAN       => Start_Writing_WLAN,
      Start_Reading_WLAN       => Start_Reading_WLAN,
      Write_Backplane          => Picowi.PIO_SPI.gSPI.Write_Backplane,
      Has_Event                => Picowi.PIO_SPI.gSPI.Has_Event,
      Available_Packet_Length  => Picowi.PIO_SPI.gSPI.Available_Packet_Length,
      Is_Ready_To_Send         => Picowi.PIO_SPI.gSPI.Is_Ready_To_Send,
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

   Buffer : SDPCM.Byte_Array (1 .. 1600);
   State  : SDPCM_IO.State;
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

   loop
      declare
         Action : SDPCM_IO.Action;
      begin
         SDPCM_IO.Process
           (State,
            Buffer,
            Length => 0,
            Action => Action);

         case Action.Kind is
            when SDPCM_IO.Idle =>
               RP.Device.Timer.Delay_Milliseconds (1);
            when SDPCM_IO.Continue =>
               null;
            when SDPCM_IO.Sleep =>
               RP.Device.Timer.Delay_Milliseconds (Action.Milliseconds);
            when SDPCM_IO.Complete_IO =>
               raise Program_Error;
            when SDPCM_IO.Process_Packet =>
               null;
         end case;
      end;
   end loop;
end Blink;
