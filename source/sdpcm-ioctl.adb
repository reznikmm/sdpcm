--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with SDPCM.Packets;

package body SDPCM.IOCTL is
   TX_Sequence : Interfaces.Unsigned_8 := 0;
   TX_Request  : Interfaces.Unsigned_16 := 0;

   -----------------
   -- Data_Offset --
   -----------------

   function Data_Offset (Write_Prefix_Length : Natural) return Positive is
      type Header is record
         S : SDPCM.Packets.SDPCM_Header;
         I : IOCTL_Header;
         D : Interfaces.Unsigned_8;
      end record
        with Pack;

      Dummy : constant Header :=
        (S => (others => <>),
         I => (others => <>),
         D => 0);

   begin
      return 1 + Dummy.D'Position + Write_Prefix_Length;
   end Data_Offset;

   ------------
   -- Encode --
   ------------

   function Encode
     (Value : String; X : Interfaces.Unsigned_8) return Byte_Array is
   begin
      return Raw : Byte_Array (1 .. Value'Length + 4) do
         Raw (1) := Value'Length;
         Raw (2) := 0;
         Raw (3) := X;
         Raw (4) := 0;

         for J in Value'Range loop
            Raw (4 + J) := Character'Pos (Value (J));
         end loop;
      end return;
   end Encode;

   ---------
   -- Set --
   ---------

   procedure Set
     (Buffer  : in out Buffer_Byte_Array;
      Command : IOCTL.Command;
      Name    : Byte_Array;
      Data    : Byte_Array;
      Write   : Boolean)
   is
      First : constant Natural := Data_Offset;
      Last  : constant Natural := First +
        (Name'Length + Data'Length + 3) / 4 * 4 - 1;
   begin
      Buffer (First .. First + Name'Length - 1) := Buffer_Byte_Array (Name);

      Buffer (First + Name'Length .. First + Name'Length + Data'Length - 1)
        := Buffer_Byte_Array (Data);

      for J in First + Name'Length + Data'Length .. Last loop
         Buffer (J) := 0;
      end loop;

      Set_In_Place (Buffer (1 .. Last), Command, Write);
   end Set;

   -----------------
   -- Send_Buffer --
   -----------------

   procedure Send_Buffer
     (Buffer : in out Buffer_Byte_Array;
      From   : Positive;
      To     : Positive)
   is

      type Output_Data is record
         Prefix : Buffer_Byte_Array (1 .. Bus.Write_Prefix_Length);
         SDPCM  : Packets.SDPCM_Header;
         Pad    : Interfaces.Unsigned_16;
         BDC    : Packets.BDC_Header;
         Data   : Buffer_Byte_Array (1 .. 1600);
      end record
        with Pack;

      Output : Output_Data
        with Import, Address => Buffer'Address;

   begin
      if From /= 1 + Output.Data'Position then
         declare
            First : Positive := 1 + Output.Data'Position;
            Last  : Positive := First + To - From;
         begin
            Buffer (First .. Last) := Buffer (From .. To);
            Last := (Last + 3) / 4 * 4;

            Send_Buffer (Buffer, First, Last);
            return;
         end;
      elsif To /= (To + 3) / 4 * 4 then
        Send_Buffer (Buffer, From, (To + 3) / 4 * 4);
         return;
      end if;

      TX_Sequence := Interfaces.Unsigned_8'Succ (TX_Sequence);

      Output :=
        (Prefix => Bus.Write_Prefix
           (Bus_Function => WLAN,
            Address      => 0,
            Length       => To - Bus.Write_Prefix_Length),
         SDPCM  =>
           (Tag      =>
                Packets.Make_Tag
                   (Interfaces.Unsigned_16 (To - Bus.Write_Prefix_Length)),
            Sequence => TX_Sequence,
            Channel  => Packets.Data,
            Next_Len => 0,
            Hdr_Len  => Packets.SDPCM_Header'Size / 8 + 2,
            Flow     => 0,
            Credit   => 0,
            Reserved => 0),
         Pad  => 0,
         BDC  => (16#20#, 0, 0, 0),
         Data => <>);

      Bus.Start_Writing_WLAN (Buffer (1 .. To));
   end Send_Buffer;

   ------------------
   -- Set_In_Place --
   ------------------

   procedure Set_In_Place
     (Buffer  : in out Buffer_Byte_Array;
      Command : IOCTL.Command;
      Write   : Boolean)
   is
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_16;
      use type Interfaces.Unsigned_32;

      type Output_Command is record
         Prefix  : Buffer_Byte_Array (1 .. Bus.Write_Prefix_Length);
         SDPCM   : Packets.SDPCM_Header;
         IOCTL   : IOCTL_Header;
      end record
        with Pack;

      First : constant Natural := Data_Offset (Bus.Write_Prefix_Length);
      Last  : constant Positive := Buffer'Last;

      Out_Length : constant Interfaces.Unsigned_16 :=
        Interfaces.Unsigned_16 (Last - First + 1 + 3) / 4 * 4;

      Output : Output_Command
        with Import, Address => Buffer'Address;

   begin
      TX_Sequence := TX_Sequence + 1;
      TX_Request := Interfaces.Unsigned_16'Succ (TX_Request);

      Output :=
        (Prefix => Bus.Write_Prefix
           (Bus_Function => WLAN,
            Address      => 0,
            Length       => Buffer'Length - Bus.Write_Prefix_Length),
         SDPCM  =>
           (Tag      =>
                Packets.Make_Tag
              (Interfaces.Unsigned_16
                   (Buffer'Length - Bus.Write_Prefix_Length)),
            Sequence => TX_Sequence,
            Channel  => Packets.Control,
            Next_Len => 0,
            Hdr_Len  => Packets.SDPCM_Header'Size / 8,
            Flow     => 0,
            Credit   => 0,
            Reserved => 0),
         IOCTL  =>
           (Command    => Command,
            Out_Length => Out_Length,
            In_Length  => 0,
            Flags      =>
              (if Write
               then Interfaces.Unsigned_32 (TX_Request) * 2**16 + 2
               else 0),
            Status     => 0));

      Bus.Start_Writing_WLAN (Buffer);
   end Set_In_Place;

end SDPCM.IOCTL;
