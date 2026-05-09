--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;
with Interfaces;
with Ada.Real_Time;
with System;

with Picowi.GPIO_SPI;

with SDPCM.Generic_Mapped_Read_Resource;
with SDPCM.Generic_IO;

procedure Blink is

   --  Bus interface implementaion
   -------------------------------

   package GPIO_SPI renames Picowi.GPIO_SPI;

   package SPI_Bus is new SDPCM.Generic_Bus
     (Read_Backplane_Register  => GPIO_SPI.gSPI.Read_Backplane_Register,
      Write_Backplane_Register => GPIO_SPI.gSPI.Write_Backplane_Register,
      Write_Prefix_Length      => GPIO_SPI.gSPI.Word'Length,
      Write_Prefix             => GPIO_SPI.gSPI.Write_Prefix,
      Start_Writing_WLAN       => GPIO_SPI.gSPI.Write_WLAN,
      Start_Reading_WLAN       => GPIO_SPI.gSPI.Read_WLAN,
      Write_Backplane          => GPIO_SPI.gSPI.Write_Backplane,
      Is_Ready                 => GPIO_SPI.gSPI.Is_Ready,
      Available_Packet_Length  => GPIO_SPI.gSPI.Available_Packet_Length,
      Clear_Error              => GPIO_SPI.gSPI.Clear_Error);

   --  Timeout interface implementation
   ------------------------------------
   use type Ada.Real_Time.Time;

   function New_Timeout (Second : Natural) return Ada.Real_Time.Time is
     (Ada.Real_Time.Clock + Ada.Real_Time.Seconds (Second));

   function Is_Expired (Value : Ada.Real_Time.Time) return Boolean is
     (Value < Ada.Real_Time.Clock);

   package Timeouts is new SDPCM.Generic_Timeouts
     (Timeout     => Ada.Real_Time.Time,
      New_Timeout => New_Timeout,
      Is_Expired  => Is_Expired);

   --  Resource interface implementation
   -------------------------------------

   procedure Read_Resource is new SDPCM.Generic_Mapped_Read_Resource
     (Address         => System'To_Address (16#101c0000#),
      Firmware_Length => 224192,
      NVRAM_Length    => 16#300#,
      CLM_Blob_Length => 984);

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

   procedure Delay_Milliseconds (Count : Positive);

   procedure Delay_Milliseconds (Count : Positive) is
   begin
      delay 0.001 * Count;
   end Delay_Milliseconds;

   Ok : Boolean;

   Buffer : SDPCM.Buffer_Byte_Array (1 .. 1600);
   State  : SDPCM_IO.State;
   Prev   : Ada.Real_Time.Time;
   On     : Interfaces.Unsigned_32 := 0;
begin

   GPIO_SPI.Configure_GPIO (Power_On => True);

   for J in 1 .. 4 loop
      Delay_Milliseconds (2);
      GPIO_SPI.gSPI.Detect_Chip (Ok);
      exit when Ok;
   end loop;

   pragma Assert (Ok);

   GPIO_SPI.gSPI.Switch_Endian (Ok);
   pragma Assert (Ok);

   Prev := Ada.Real_Time.Clock;

   loop
      declare
         use type Interfaces.Unsigned_32;
         use all type SDPCM_IO.Action_Kind;
         Action : SDPCM_IO.Action;
      begin
         SDPCM_IO.Poll
           (State,
            Buffer,
            Action => Action);

         if Action.Kind = Process_Packet then
            Ada.Text_IO.Put_Line
              ("Got " & Integer'Image (Action.Span.To - Action.Span.From + 1));
         elsif Ada.Real_Time.Clock > Prev + Ada.Real_Time.Seconds (1) then
            Prev := Ada.Real_Time.Clock;

            SDPCM_IO.Set_GPIO (On);
            --  Turn LED ON or OFF
            On := On xor 1;
         end if;

         case Action.Kind is
            when SDPCM_IO.Idle =>
               Delay_Milliseconds (1);
            when SDPCM_IO.Continue =>
               null;
            when SDPCM_IO.Sleep =>
               Delay_Milliseconds (Action.Milliseconds);
            when SDPCM_IO.Complete_IO =>
               null;  --  We use synchronous I/O in this demo. Do nothing
            when SDPCM_IO.Process_Packet =>
               null;
         end case;
      end;
   end loop;
end Blink;
