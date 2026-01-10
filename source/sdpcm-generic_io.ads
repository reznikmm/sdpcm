--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

generic
   with package Bus is new SDPCM.Generic_Bus (<>);
   with package Timeouts is new SDPCM.Generic_Timeouts (<>);
   with package Resources is new SDPCM.Generic_Resources (<>);
   with package Network is new SDPCM.Generic_Network (<>);
package SDPCM.Generic_IO is
   pragma Pure;

   type State is limited private
     with Preelaborable_Initialization;
   --  State of the generic IO driver.

   type Action_Kind is
     (Idle,
      Continue,
      Sleep,
      Complete_IO,
      Process_Packet);
   --  Kinds of actions returned by Process procedure.
   --
   --  * @enum Idle - no immediate action required
   --  * @enum Continue - call Process again immediately
   --  * @enum Sleep - suspend calling Process for given timeout
   --  * @enum Complete_IO - complete pending asynchronous IO operation
   --  * @enum Process_Packet - process received packet from the device

   type Action (Kind : Action_Kind := Continue) is record
      case Kind is
         when Idle | Continue | Complete_IO =>
            null;
         when Sleep =>
            Milliseconds : Positive;
            --  Timeout to wait before next Process call.
         when Process_Packet =>
            From, To : Positive;
            --  Bounds of the received packet.
      end case;
   end record;
   --  Action returned by Process procedure.

   procedure Process
     (State  : in out SDPCM.Generic_IO.State;
      Buffer : in out Byte_Array;
      Length : Natural;
      Action : out SDPCM.Generic_IO.Action);
   --  Process IO events and change State accordingly. This procedure is not
   --  expected to block. User should't change Buffer content if asynchronous
   --  IO is in progress (Action.Kind = Complete_IO).
   --
   --  To send data packet to the device, user should write data to Buffer
   --  and call Process with Length set to the length of the data to send.
   --  This could be done only if Action.Kind = Idle.

private

   type Joining_State_Kind is
    (Boot_Up, Idle, Joining, Joined, Failed);

   type Joining_State (Kind : Joining_State_Kind := Boot_Up) is record
      case Kind is
         when Boot_Up =>
            Boot_Step : Positive := 1;
         when Idle | Joined =>
            null;
         when Joining | Failed =>
            Timeout : Timeouts.Timeout;
            --  Timeout for joining/failed states.
      end case;
   end record;

   type State is limited record
      Joining : Joining_State := (Kind => Boot_Up, Boot_Step => 1);
   end record;

end SDPCM.Generic_IO;