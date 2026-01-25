--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  Generic non-blocking WiFi driver core.
--
--  This package implements a cooperative state machine that drives
--  WiFi chip initialization, control traffic and data exchange.
--  It does not perform any blocking waits and does not allocate memory.
--
--  The user is responsible for:
--   * providing a reusable packet buffer
--   * calling Poll periodically
--   * handling scheduling based on returned Action
--   * providing bus-specific asynchronous I/O completion
--
--  The driver never owns memory. All packet data is exchanged through
--  the Buffer passed to Poll.
--
--  This package is intended to be a low-level building block for higher
--  level network stacks, not a complete socket-level API.

generic
   with package Bus is new SDPCM.Generic_Bus (<>);

   with package Timeouts is new SDPCM.Generic_Timeouts (<>);

   with procedure Read_Resource
     (Kind   : Resource_Kind;
      Offset : Natural;
      Data   : out Byte_Array;
      Last   : out Natural);
   --  Generic Resources Interface. Read resource of given kind.

   with package Network is new SDPCM.Generic_Network (<>);

package SDPCM.Generic_IO is
   pragma Preelaborate;

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

   type Buffer_Span is record
      From : Positive;
      To   : Natural;
   end record;
   --  Represents a subrange inside buffer (see Poll).
   --
   --  A span is considered empty when To < From. Function Empty returns
   --  such a value and is used to indicate "no packet".

   function Empty return Buffer_Span is (From => 1, To => 0);

   type Action (Kind : Action_Kind := Continue) is record
      case Kind is
         when Idle | Continue =>
            null;
         when Complete_IO =>
            --  The driver has started an asynchronous bus operation and cannot
            --  make progress until it completes. This is intentionally exposed
            --  to allow integration with systems that use interrupt- or
            --  DMA-driven bus layers without blocking inside the driver.
            null;
         when Sleep =>
            Milliseconds : Positive;
            --  Timeout to wait before next Process call.
         when Process_Packet =>
            Span : Buffer_Span;
            --  A full packet has been received from the device and stored into
            --  Buffer. Span designates the packet boundaries.
            --  The data remains valid until the next call to Poll.
      end case;
   end record;
   --  Result of a Poll step.
   --
   --  Action_Kind combines scheduling hints with driver events.
   --  Some values indicate when Poll should be called again, while others
   --  signal data or I/O state changes.

   procedure Poll
     (State  : in out Generic_IO.State;
      Buffer : in out Buffer_Byte_Array;
      Send   : Buffer_Span := Empty;
      Action : out Generic_IO.Action);
   --  Advance the internal driver state machine.
   --
   --  This procedure performs at most one small, non-blocking step of the
   --  driver. It may initiate asynchronous bus I/O operations but will not
   --  wait for them to complete.
   --
   --  PARAMETERS
   --
   --  @param State
   --  Driver state. Must be preserved between calls.
   --
   --  @param Buffer
   --  Shared packet buffer used for both transmit and receive operations.
   --  The driver may read from or write to this buffer depending on Action.
   --
   --  @param Send
   --  If Send /= Empty, the span designates a packet to be transmitted.
   --  The packet must remain unchanged until the driver reports that the
   --  operation has completed (see Action.Kind = Complete_IO).
   --
   --  ACTION SEMANTICS
   --
   --  * Idle
   --    No immediate work is required. Poll may be called later. Sending
   --    is allowed on the next Poll call.
   --
   --  * Continue
   --    Driver has more internal work to do. Poll should be called again
   --    as soon as possible.
   --
   --  * Sleep
   --    Driver is waiting for a timeout. Poll should not be called for
   --    at least Action.Milliseconds.
   --
   --  * Complete_IO
   --    An asynchronous bus operation is in progress. The user must wait
   --    until the bus layer signals completion before calling Poll again.
   --    The Buffer content must not be modified during this time.
   --
   --  * Process_Packet
   --    A packet has been received into Buffer. Action.Span designates
   --    the valid packet data. The user may process or copy it before the
   --    next Poll call.
   --
   --  BUFFER OWNERSHIP RULES
   --
   --  * When Action.Kind = Complete_IO, the buffer is owned by the driver.
   --  * When Action.Kind = Process_Packet, the buffer contains RX data.
   --  * Otherwise the buffer is owned by the user.
   --
   --  This procedure is re-entrant only at the call level: it must not be
   --  called again until the previous asynchronous operation (if any) has
   --  completed.

   function Is_Starting (State : SDPCM.Generic_IO.State) return Boolean;
   --  True while the driver is still performing chip boot.

   function Is_Joinded (State : SDPCM.Generic_IO.State) return Boolean;
   --  True when the device has successfully joined the configured network.

   function Is_Failed (State : SDPCM.Generic_IO.State) return Boolean;
   --  True if initialization or join has permanently failed.

   subtype Ether_Address is Byte_Array (1 .. 6);

   function Get_MAC (State : SDPCM.Generic_IO.State) return Ether_Address
     with Pre => not (Is_Starting (State) or Is_Failed (State));

   procedure Set_GPIO (Value : Interfaces.Unsigned_32);
   --  Could be use to control LED connected to WiFi chip

private

   type Joining_State_Kind is
    (Boot_Up, Idle, Joining, Joined, Failed, Crashed);

   type Joining_State (Kind : Joining_State_Kind := Boot_Up) is record
      case Kind is
         when Boot_Up =>
            null;
         when Idle | Joined | Crashed =>
            null;
         when Joining | Failed =>
            Timeout : Timeouts.Timeout;
            --  Timeout for joining/failed states.
      end case;
   end record;

   type Link_State (Failed : Boolean := True) is record
      case Failed is
         when False =>
            Up   : Boolean;
            Auth : Boolean;
         when True =>
            null;
      end case;
   end record;

   type State is limited record
      Joining : Joining_State := (Kind => Boot_Up);
      Step    : Positive := 1;
      Offset  : Natural := 0;
      Command : Interfaces.Unsigned_32 := 0;
      Reading : Natural := 0;
      MAC     : Ether_Address := (others => 0);
      Link    : Link_State;
   end record;
   --  Internal driver state.
   --  This record intentionally contains protocol progress, firmware loading
   --  offsets, command sequencing and link state. It is opaque to users and
   --  must only be modified by Poll.

   function Get_MAC (State   : SDPCM.Generic_IO.State) return Ether_Address is
      (State.MAC);

   function Is_Joinded (State : SDPCM.Generic_IO.State) return Boolean is
     (State.Joining.Kind = Joined);

   function Is_Starting (State : SDPCM.Generic_IO.State) return Boolean is
     (State.Joining.Kind = Boot_Up);

   function Is_Failed (State : SDPCM.Generic_IO.State) return Boolean is
     (State.Joining.Kind = Failed);

end SDPCM.Generic_IO;
