--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Net.Buffers;
with Net.Interfaces;
with Picowi.PIO_SPI;
with RP.Timer;
with SDPCM.Generic_IO;
with System;

generic
   with procedure Read_Resource
     (Kind   : SDPCM.Resource_Kind;
      Offset : Natural;
      Data   : out SDPCM.Byte_Array;
      Last   : out Natural);
   --  Generic read firmware procedure

   with package Network is new SDPCM.Generic_Network (<>);
   --  Generic SSID/Password access interface

package Enet_Picowi.Generic_Interface is

   type Ifnet (Priority : System.Priority := System.Default_Priority) is
     new Net.Interfaces.Ifnet_Type with private;

   procedure Create (Self : in out Ifnet'Class);

   procedure Send
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type);

   procedure Receive
     (Self   : in out Ifnet;
      Packet : in out Net.Buffers.Buffer_Type);

   function Is_Joined (Self : Ifnet) return Boolean;

private

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

   package SDPCM_IO is new SDPCM.Generic_IO
     (Bus           => SPI_Bus,
      Timeouts      => Timeouts,
      Read_Resource => Read_Resource,
      Network       => Network);

   protected type Buffer_Queue
     (Priority : System.Priority := System.Default_Priority)
   is
      procedure Put (Packet : in out Net.Buffers.Buffer_Type);
      procedure Get (Packet : in out Net.Buffers.Buffer_Type);
   private
      pragma Priority (Priority);

      List : Net.Buffers.Buffer_List;
   end Buffer_Queue;

   type Ifnet (Priority : System.Priority := System.Default_Priority) is
     new Net.Interfaces.Ifnet_Type with
   record
      Send_Queue : Buffer_Queue;
      State      : SDPCM_IO.State;
   end record;

   function Is_Joined (Self : Ifnet) return Boolean is
     (SDPCM_IO.Is_Joinded (Self.State));

end Enet_Picowi.Generic_Interface;
