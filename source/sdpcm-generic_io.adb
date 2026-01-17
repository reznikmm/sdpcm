--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;
with Ada.Unchecked_Conversion;

package body SDPCM.Generic_IO is
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   package Backplane_Register is
      Win_Addr : constant := 16#1000a#;  --  Window addr
      Chip_Clock_CSR : constant := 16#1000e#;  --  Chip clock ctrl
      Pull_Up        : constant := 16#1000f#;
      Sleep_CSR      : constant := 16#1001f#;
      GPIO_Out_En    : constant := 16#8068#;
      GPIO_Out       : constant := 16#8064#;
   end Backplane_Register;

   subtype Frame_Tag is Interfaces.Unsigned_32;

   function Make_Tag (Length : Interfaces.Unsigned_16) return Frame_Tag is
     (Interfaces.Shift_Left (Interfaces.Unsigned_32 (not Length), 16) +
      Interfaces.Unsigned_32 (Length));

   package IOCTL is
      type Command is new Interfaces.Unsigned_32;

      SET_VAR      : constant Command := 263;

      procedure Set
        (Buffer  : in out Buffer_Byte_Array;
         Command : IOCTL.Command;
         Name    : Byte_Array;
         Data    : Byte_Array);

      procedure Set_In_Place
        (Buffer  : in out Buffer_Byte_Array;
         Command : IOCTL.Command);

      function Data_Offset return Positive;

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

   end IOCTL;

   package SDPCM is
      type BDC_Header is record
         Flags    : Interfaces.Unsigned_8;
         Priority : Interfaces.Unsigned_8;
         Flags2   : Interfaces.Unsigned_8;
         Offset   : Natural range 0 .. 255;
      end record;

      for BDC_Header use record
         Flags    at 0 range 0 .. 7;
         Priority at 1 range 0 .. 7;
         Flags2   at 2 range 0 .. 7;
         Offset   at 3 range 0 .. 7;
      end record;

      function Is_Valid (Tag : Frame_Tag) return Boolean is
        (((Interfaces.Shift_Right (Tag, 16) xor Tag) and 16#FFFF#) = 16#FFFF#);

      type SDPCM_Channel is new Interfaces.Unsigned_8;

      Control : constant SDPCM_Channel := 0;
      Event   : constant SDPCM_Channel := 1;
      Data    : constant SDPCM_Channel := 2;

      subtype Event_Or_Data is SDPCM_Channel range Event .. Data;

      type SDPCM_Header is record
         Tag      : Frame_Tag;
         Sequence : Interfaces.Unsigned_8;
         Channel  : SDPCM_Channel;
         Next_Len : Interfaces.Unsigned_8;
         Hdr_Len  : Natural range 0 .. 255;  --  SDPCM header plus any padding
         Flow     : Interfaces.Unsigned_8;
         Credit   : Interfaces.Unsigned_8;
         Reserved : Interfaces.Unsigned_16;
      end record;

      for SDPCM_Header use record
         Tag      at 0 range 0 .. 31;
         Sequence at 4 range 0 .. 7;
         Channel  at 5 range 0 .. 7;
         Next_Len at 6 range 0 .. 7;
         Hdr_Len  at 7 range 0 .. 7;
         Flow     at 8 range 0 .. 7;
         Credit   at 9 range 0 .. 7;
         Reserved at 10 range 0 .. 15;
      end record;

      type Packet (Channel : SDPCM_Channel := 0) is record
         case Channel is
            when Control =>
               IOCTL_Header : IOCTL.IOCTL_Header;
               IOCTL_Offset : Natural;
            when Event | Data =>
               Offset       : Natural;
            when others =>
               null;
         end case;
      end record;

      procedure Decode_Input
        (Input  : Buffer_Byte_Array;
         Result : out Packet);

   end SDPCM;

   package Executor is

      type Step_Kind is
        (Write_Register,
         Write_Register_Variable,
         Read_Register,
         Read_Register_Until,
         Upload_Firmware,
         Upload_CLM,
         Wait_Any_Event,
         Clear_Error,
         Sleep);

      type Step (Kind : Step_Kind := Sleep) is record
         case Kind is
            when Read_Register
               | Read_Register_Until
               | Write_Register
               | Write_Register_Variable
            =>
               Address : Interfaces.Unsigned_32;
               Length  : Positive;

               case Kind is
                  when Read_Register | Write_Register_Variable =>
                     null;

                  when Read_Register_Until =>
                     Mask : Interfaces.Unsigned_32;
                     Trys : Natural;

                  when Write_Register =>
                     Value : Interfaces.Unsigned_32;

                  when Upload_CLM
                     | Upload_Firmware
                     | Wait_Any_Event
                     | Clear_Error
                     | Sleep
                   =>
                     null;
               end case;

            when Upload_Firmware =>
               Firmware : Resource_Kind range Standard.SDPCM.Firmware .. NVRAM;

            when Upload_CLM | Clear_Error =>
               null;

            when Sleep | Wait_Any_Event =>
               Milliseconds : Natural;
         end case;
      end record;

      type Step_Array is array (Positive range <>) of Step;

      procedure Execute
        (Step         : Executor.Step;
         Index        : in out Positive;
         Offset       : in out Natural;
         Buffer       : in out Buffer_Byte_Array;
         Success      : in out Boolean;
         Command      : in out IOCTL.Command;
         Custom_Value : Interfaces.Unsigned_32 := 0);

      AI_IOCTRL_OSET    : constant := 16#408#;
      AI_RESETCTRL_OSET : constant := 16#800#;
      RAM_Core_Base : constant Interfaces.Unsigned_32 := 16#4000#;
      ARM_Core_Base : constant Interfaces.Unsigned_32 := 16#3000#;

      Start : constant Step_Array :=
        [  --  1 =>
           (Kind         => Read_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Length       => 1),
         --  Check Active Low Power (ALP) clock
         --  2 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Value        => 16#08#,
            Length       => 1),
         --  3 =>
           (Kind         => Read_Register_Until,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Mask         => 16#40#,
            Length       => 1,
            Trys         => 10),
         --  4 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Value        => 0,
            Length       => 1),
         --  Reset (RAM)
         --  5 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#1810_0000# / 256,
            Length       => 3),
         --  6 =>
           (Kind         => Read_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  7 =>
           (Kind         => Write_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Value        => 3,
            Length       => 1),
         --  8 =>
           (Kind         => Read_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  9 =>
           (Kind         => Write_Register,
            Address      => RAM_Core_Base + AI_RESETCTRL_OSET,
            Value        => 0,
            Length       => 1),
         --  10 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  11 =>
           (Kind         => Write_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Value        => 1,
            Length       => 1),
         --  12 =>
           (Kind         => Read_Register,
            Address      => RAM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  13 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  end of reset
           --  14 =>
           (Kind         => Executor.Read_Register,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Length       => 1),
           --  Write 0x18004010 and 0x18004044
           --  15 =>
           (Kind         => Executor.Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#180000#,
            Length       => 3),
           --  16 =>
           (Kind         => Executor.Write_Register,
            Address      => 16#04010#,
            Value        => 3,
            Length       => 4),
           --  17 =>
           (Kind         => Executor.Write_Register,
            Address      => 16#04044#,
            Value        => 0,
            Length       => 4),
           --  18 =>
           (Kind         => Executor.Upload_Firmware,
            Firmware     => Firmware),
           --  19 =>
           (Kind         => Executor.Sleep,
            Milliseconds => 5),
           --  20 =>
           (Kind         => Executor.Upload_Firmware,
            Firmware     => NVRAM),
           --  21 =>
           (Kind         => Executor.Write_Register_Variable,
            Address      => 16#FFFC#,  --  NVRAM size register
            Length       => 4),
         --  Reset (ARM)
         --  22 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#1810_0000# / 256,
            Length       => 3),
         --  23 =>
           (Kind         => Read_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  24 =>
           (Kind         => Write_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Value        => 3,
            Length       => 1),
         --  25 =>
           (Kind         => Read_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  26 =>
           (Kind         => Write_Register,
            Address      => ARM_Core_Base + AI_RESETCTRL_OSET,
            Value        => 0,
            Length       => 1),
         --  27 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  28 =>
           (Kind         => Write_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Value        => 1,
            Length       => 1),
         --  29 =>
           (Kind         => Read_Register,
            Address      => ARM_Core_Base + AI_IOCTRL_OSET,
            Length       => 1),
         --  30 =>
           (Kind         => Sleep,
            Milliseconds => 1),
         --  end of reset
         --  31 =>
           (Kind         => Executor.Read_Register_Until,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Mask         => 16#80#,
            Length       => 1,
            Trys         => 50),
         --  32 =>
           (Kind => Executor.Wait_Any_Event,
            Milliseconds => 100),
         --  33 =>
           (Kind         => Executor.Upload_CLM),
         --  34 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#1800_0000# / 256,
            Length       => 3),
         --  35 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.GPIO_Out_En,
            Value        => 1,
            Length       => 4),
         --  36 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.GPIO_Out,
            Value        => 1,
            Length       => 4)
          ];
   end Executor;

   package body IOCTL is

      TX_Sequence : Interfaces.Unsigned_8 := 0;
      TX_Request  : Interfaces.Unsigned_16 := 0;

      function Data_Offset return Positive is
         type Header is record
            S : SDPCM.SDPCM_Header;
            I : IOCTL_Header;
            D : Interfaces.Unsigned_8;
         end record
           with Pack;

         Dummy : constant Header :=
           (S => (others => <>),
            I => (others => <>),
            D => 0);

      begin
         return 1 + Dummy.D'Position + Bus.Write_Prefix_Length;
      end Data_Offset;

      ---------
      -- Set --
      ---------

      procedure Set
        (Buffer  : in out Buffer_Byte_Array;
         Command : IOCTL.Command;
         Name    : Byte_Array;
         Data    : Byte_Array)
      is
         use type Interfaces.Unsigned_8;

         First : Positive := Data_Offset;
         Last  : Positive := First + Name'Length + Data'Length - 1;

         Out_Length : constant Interfaces.Unsigned_16 :=
           (Name'Length + Data'Length + 3) / 4 * 4;

         Length     : constant Interfaces.Unsigned_16 :=
           Interfaces.Unsigned_16 (Bus.Write_Prefix_Length) +
           (SDPCM.SDPCM_Header'Size + IOCTL_Header'Size) / 8 +
           Out_Length;

      begin
         Buffer (First .. First + Name'Length - 1) := Buffer_Byte_Array (Name);

         Buffer (First + Name'Length .. First + Name'Length + Data'Length - 1)
           := Buffer_Byte_Array (Data);

         Set_In_Place (Buffer (1 .. Last), Command);
      end Set;

      ------------------
      -- Set_In_Place --
      ------------------

      procedure Set_In_Place
        (Buffer  : in out Buffer_Byte_Array;
         Command : IOCTL.Command)
      is
         use type Interfaces.Unsigned_8;
         use type Interfaces.Unsigned_16;

         type Output_Command is record
            Prefix  : Buffer_Byte_Array (1 .. Bus.Write_Prefix_Length);
            SDPCM   : Generic_IO.SDPCM.SDPCM_Header;
            IOCTL   : IOCTL_Header;
         end record
           with Pack;

         First : Positive := Data_Offset;
         Last  : Positive := Buffer'Last;

         Out_Length : constant Interfaces.Unsigned_16 :=
           Interfaces.Unsigned_16 (Last - First + 1 + 3) / 4 * 4;

         Output : Output_Command
           with Import, Address => Buffer'Address;

         Length : constant Interfaces.Unsigned_16 := Buffer'Length;

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
                 Make_Tag
                   (Interfaces.Unsigned_16
                     (Buffer'Length - Bus.Write_Prefix_Length)),
               Sequence => TX_Sequence,
               Channel  => SDPCM.Control,
               Next_Len => 0,
               Hdr_Len  => SDPCM.SDPCM_Header'Size / 8,
               Flow     => 0,
               Credit   => 0,
               Reserved => 0),
            IOCTL  =>
              (Command    => Command,
               Out_Length => Out_Length,
               In_Length  => 0,
               Flags      =>
                 Interfaces.Unsigned_32 (TX_Request) * 2**16 + 2,
               Status     => 0));

         Bus.Start_Writing_WLAN (Buffer);
      end Set_In_Place;

   end IOCTL;

   package body Executor is

      Block_Size  : constant := 64;
      Window_Size : constant := 16#8000#;
      Load_Size   : constant := 512;

      procedure Upload_Firmware
        (Kind   : Resource_Kind;
         Offset : in out Natural;
         Buffer : out Buffer_Byte_Array);

      procedure Upload_CLM_Blob
        (Offset  : in out Natural;
         Buffer  : out Buffer_Byte_Array;
         Command : out IOCTL.Command);

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Step         : Executor.Step;
         Index        : in out Positive;
         Offset       : in out Natural;
         Buffer       : in out Buffer_Byte_Array;
         Success      : in out Boolean;
         Command      : in out IOCTL.Command;
         Custom_Value : Interfaces.Unsigned_32 := 0)
      is
         procedure Increment_Step;

         --------------------
         -- Increment_Step --
         --------------------

         procedure Increment_Step is
         begin
            Index := Index + 1;
            Offset := 0;
         end Increment_Step;

         Value       : Interfaces.Unsigned_32;

      begin
         case Step.Kind is
            when Write_Register =>
               Bus.Write_Backplane_Register
                 (Step.Address, Step.Length, Step.Value);

            when Write_Register_Variable =>
               Bus.Write_Backplane_Register
                 (Step.Address, Step.Length, Custom_Value);

            when Read_Register =>
               Bus.Read_Backplane_Register (Step.Address, Step.Length, Value);

            when Read_Register_Until =>
               Bus.Read_Backplane_Register (Step.Address, Step.Length, Value);

            when Clear_Error =>
               Bus.Clear_Error;

            when Upload_Firmware =>
               Upload_Firmware (Step.Firmware, Offset, Buffer);

            when Upload_CLM =>
               Upload_CLM_Blob (Offset, Buffer, Command);

            when Wait_Any_Event =>
               Value := Boolean'Pos (Bus.Has_Event);

            when others =>
               null;
         end case;

         case Step.Kind is
            when Read_Register_Until =>
               if (Value and Step.Mask) /= 0 then
                  Increment_Step;
               elsif Offset < Step.Trys then
                  Offset := Offset + 1;
               else
                  Success := False;
               end if;

            when Upload_Firmware | Upload_CLM =>
               if Offset = 0 then
                  Increment_Step;
               end if;

            when Wait_Any_Event =>
               if Value = 1 then
                  Increment_Step;
               elsif Offset < Step.Milliseconds then
                  Offset := Offset + 1;
               else
                  Success := False;
               end if;

            when others =>
               Increment_Step;
         end case;
      end Execute;

      ---------------------
      -- Upload_CLM_Blob --
      ---------------------

      procedure Upload_CLM_Blob
        (Offset  : in out Natural;
         Buffer  : out Buffer_Byte_Array;
         Command : out IOCTL.Command)
      is
         type CLM_Load_Request is record
            Req  : String (1 .. 8);
            Flag : Interfaces.Unsigned_16;
            Tipe : Interfaces.Unsigned_16;
            Len  : Interfaces.Unsigned_32;
            Crc  : Interfaces.Unsigned_32;
         end record;

         for CLM_Load_Request use record
            Req  at 0  range 0 .. 8 * 8 - 1;
            Flag at 8  range 0 .. 15;
            Tipe at 10 range 0 .. 15;
            Len  at 12 range 0 .. 31;
            Crc  at 16 range 0 .. 31;
         end record;

         subtype Raw_Request is Buffer_Byte_Array
           (1 .. CLM_Load_Request'Size / 8);

         function To_Raw_Request is new Ada.Unchecked_Conversion
           (CLM_Load_Request, Raw_Request);

         NUL : constant Character := Character'Val (0);

         Name : Positive := IOCTL.Data_Offset;
         Data : Positive := Name + Raw_Request'Length;
         To   : Positive := Data + Load_Size - 1;
         Last : Natural := 0;

         function Flag return Interfaces.Unsigned_16 is
           ((if Offset = 0 then 2 else 0) +
            (if Last < To then 4 else 0) +
            16#1000#);

      begin
         if Offset /= Natural'Last then
            Read_Resource
              (CLM_Blob,
               Offset,
               Byte_Array (Buffer (Data .. To)),
               Last);
         end if;

         if Last >= Data then
            Buffer (Name .. Data - 1) := To_Raw_Request
              ((Req  => "clmload" & NUL,
                Flag => Flag,
                Tipe => 2,
                Len  => Interfaces.Unsigned_32 (Last - Data + 1),
                Crc  => 0));

            Command := IOCTL.SET_VAR;
            IOCTL.Set_In_Place (Buffer (1 .. Last), Command);

            Offset := (if Last = To then Offset + Load_Size else Natural'Last);
         else
            Offset := 0;
            Command := 0;
         end if;
      end Upload_CLM_Blob;

      ---------------------
      -- Upload_Firmware --
      ---------------------

      procedure Upload_Firmware
        (Kind   : Resource_Kind;
         Offset : in out Natural;
         Buffer : out Buffer_Byte_Array)
      is
         subtype Block_Range is Positive range
           Bus.Write_Prefix_Length + 1 .. Bus.Write_Prefix_Length + Block_Size;

         Last  : Natural;
         Value : Interfaces.Unsigned_32;
      begin
         Read_Resource
           (Kind,
            Offset,
            Byte_Array (Buffer (Block_Range)),
            Last);

         --  Calculate write Address
         Value := (if Kind = Firmware then 0 else 16#7_FCFC#) +
           Interfaces.Unsigned_32 (Offset);

         if Last in Block_Range then
            Buffer (1 .. Bus.Write_Prefix_Length) := Bus.Write_Prefix
              (Bus_Function => Backplane,
               Address      => Value mod Window_Size,
               Length       => Last - Bus.Write_Prefix_Length);

            if Offset = 0 or
              (Value - 1) / Window_Size /= Value / Window_Size
            then
               Bus.Write_Backplane_Register
                 (Address      => Backplane_Register.Win_Addr,
                  Value        => Value / 256,
                  Length       => 3);
            end if;

            Bus.Write_Backplane
              (Address => Value,
               Value   => Buffer (1 .. Last));

            Offset := Offset + Block_Size;
         else
            Offset := 0;
         end if;
      end Upload_Firmware;

   end Executor;

   -----------
   -- SDPCM --
   -----------

   package body SDPCM is

      ------------------
      -- Decode_Input --
      ------------------

      procedure Decode_Input
        (Input  : Buffer_Byte_Array;
         Result : out Packet)
      is
         subtype IOCTL_Header_Raw is
           Buffer_Byte_Array (1 .. IOCTL.IOCTL_Header'Size / 8);

         function To_IOCTL_Header is new Ada.Unchecked_Conversion
           (IOCTL_Header_Raw, IOCTL.IOCTL_Header);

         subtype BDC_Header_Raw is
           Buffer_Byte_Array (1 .. BDC_Header'Size / 8);

         function To_BDC_Header is new Ada.Unchecked_Conversion
           (BDC_Header_Raw, BDC_Header);

         SDPCM : SDPCM_Header
           with Import, Address => Input'Address;
      begin
         if not Is_Valid (SDPCM.Tag) or else
           (SDPCM.Channel = Control and then
            Input'Last < SDPCM.Hdr_Len + IOCTL.IOCTL_Header'Size / 8)
           or else
           (SDPCM.Channel in Event | Data and then
            Input'Last < SDPCM.Hdr_Len + BDC_Header'Size / 8)
         then
            Result := (Channel => SDPCM_Channel'Last);
         elsif SDPCM.Channel = Control then
            declare
               Skip : constant Natural := SDPCM.Hdr_Len;

               Header : constant IOCTL.IOCTL_Header := To_IOCTL_Header
                 (Input (Skip + 1 .. Skip + IOCTL.IOCTL_Header'Size / 8));

               Header_Length : constant Positive :=
                 Skip + IOCTL.IOCTL_Header'Size / 8;

            begin
               Result := (Control, Header, Header_Length + 1);
            end;
         elsif SDPCM.Channel in Event .. Data then
            declare
               Skip : constant Natural := SDPCM.Hdr_Len;

               BDC : constant BDC_Header := To_BDC_Header
                 (Input (Skip + 1 .. Skip + BDC_Header'Size / 8));

               Header_Length : constant Positive :=
                 Skip + BDC'Size / 8 + 4 * BDC.Offset;

            begin
               if Input'Last >= Header_Length + 1 then
                  Result :=
                    (Channel => Event_Or_Data (SDPCM.Channel),
                     Offset  => Header_Length + 1);
               end if;
            end;
         end if;
      end Decode_Input;

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

   end SDPCM;

   ----------------------
   -- Complete_Reading --
   ----------------------

   procedure Complete_Reading
     (State  : in out Generic_IO.State;
      Buffer : in out Buffer_Byte_Array;
      Found  : out Boolean)
   is
      use type IOCTL.Command;
      use type SDPCM.SDPCM_Channel;

      Got : SDPCM.Packet;
   begin
      State.Reading := 0;

      SDPCM.Decode_Input (Buffer, Got);

      if Got.Channel = SDPCM.Control then
         Found := Got.IOCTL_Header.Command = IOCTL.Command (State.Command);
      else
         Found := False;
      end if;
   end Complete_Reading;

   -------------
   -- Process --
   -------------

   procedure Process
     (State  : in out Generic_IO.State;
      Buffer : in out Buffer_Byte_Array;
      Length : Natural;
      Action : out Generic_IO.Action)
   is
      function To_Action
        (Step : Executor.Step) return Generic_IO.Action is
          (case Step.Kind is
              when Executor.Sleep =>
                (Sleep, Milliseconds => Step.Milliseconds),
              when Executor.Read_Register_Until
                | Executor.Wait_Any_Event =>
                  (if State.Offset = 0 then (Kind => Continue)
                   else (Sleep, Milliseconds => 1)),
              when Executor.Upload_CLM =>
                (if State.Offset = 0 then (Kind => Continue)
                 else (Kind => Complete_IO)),
              when others =>
                (Kind => Continue));

      function Need_Reading return Boolean is
         (case Executor.Start (State.Step).Kind is
             when Executor.Upload_CLM => State.Offset > 0,
             when others => False);

      Ok : Boolean := False;
   begin
      case State.Joining.Kind is
         when Boot_Up =>
            if State.Reading > 0 then
               Complete_Reading
                 (State, Buffer (1 .. State.Reading), Found => Ok);
            end if;

            if not Ok and Need_Reading then
               declare
                  Length : constant Interfaces.Unsigned_32 :=
                    Bus.Available_Packet_Length;
               begin
                  if Length = 0 then
                     Action := (Sleep, Milliseconds => 1);
                  elsif Length <= Buffer'Length then
                     State.Reading := Positive (Length);

                     Bus.Start_Reading_WLAN (Buffer (1 .. State.Reading));
                     Action := (Kind => Complete_IO);
                  else
                     raise Program_Error;  --  Buffer too small
                  end if;

                  return;
               end;
            end if;

            Ok := True;

            Executor.Execute
              (Step         => Executor.Start (State.Step),
               Index        => State.Step,
               Offset       => State.Offset,
               Buffer       => Buffer,
               Success      => Ok,
               Command      => IOCTL.Command (State.Command),
               Custom_Value => Make_Tag (16#300# / 4));

            if State.Step not in Executor.Start'Range then
               raise Program_Error;  --  List is completed
            else
               Action := To_Action (Executor.Start (State.Step));
            end if;
         when others =>
            raise Program_Error;
      end case;

      if not Ok then
         State.Joining := (Kind => Crashed);
      end if;
   end Process;

end SDPCM.Generic_IO;
