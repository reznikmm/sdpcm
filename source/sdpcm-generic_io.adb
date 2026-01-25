--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;
with Ada.Unchecked_Conversion;

with SDPCM.Events;
with SDPCM.Generic_IO;
with SDPCM.IOCTL;
with SDPCM.Packets;

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

   procedure IOCTL_Set_In_Place is new IOCTL.Set_In_Place (Bus);
   procedure IOCTL_Send_Buffer is new IOCTL.Send_Buffer (Bus);

   function IOCTL_Data_Offset return Natural is
     (IOCTL.Data_Offset (Bus.Write_Prefix_Length));

   procedure IOCTL_Set is new IOCTL.Set
     (IOCTL_Data_Offset, IOCTL_Set_In_Place);

   function IOCTL_Raw_Value
     (Name    : IOCTL.Output_Variable;
      Command : IOCTL.Command) return Byte_Array is
       (case Command is
        when IOCTL.SET_WSEC =>
          [(if Network.Security = WPA2_AES then 6 else 2), 0, 0, 0],
        when IOCTL.SET_WPA_AUTH =>
          [(if Network.Security = WPA2_AES then 16#80# else 4), 0, 0, 0],
        when IOCTL.SET_SSID => IOCTL.Encode (Network.SSID, 0),
        when IOCTL.SET_WSEC_PMK => IOCTL.Encode (Network.Password, 1),
        when others => (IOCTL.Raw_Value (Name, Command)));

   procedure Complete_Reading
     (State  : in out Generic_IO.State;
      Buffer : Buffer_Byte_Array;
      From   : out Natural;
      Found  : out Boolean);
   --  Process data from WiFi chip in Buffer. If it has a response to the
   --  current IOCTL command then set Found=True. If data is a packet
   --  set From to the first data byte, so Buffer(From..Buffer'Last) is
   --  the packet data.

   procedure On_Event
     (State : in out Generic_IO.State;
      Event : SDPCM.Events.Event_Record);

   procedure Change_State (State : in out Generic_IO.State);

   package Executor is

      type Step_Kind is
        (Write_Register,
         Write_Register_Variable,
         Read_Register,
         Read_Register_Until,
         Upload_Firmware,
         Upload_CLM,
         IOCTL_Get,
         IOCTL_Set,
         IOCTL_Set_32,
         Wait_Is_Ready,
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
                     | IOCTL_Get
                     | IOCTL_Set
                     | IOCTL_Set_32
                     | Wait_Is_Ready
                     | Clear_Error
                     | Sleep
                   =>
                     null;
               end case;

            when Upload_Firmware =>
               Firmware : Resource_Kind range Standard.SDPCM.Firmware .. NVRAM;

            when Upload_CLM | Clear_Error =>
               null;

            when IOCTL_Get | IOCTL_Set | IOCTL_Set_32 =>
               Variable : IOCTL.IO_Variable;
               Command  : IOCTL.Command;

               case Kind is
                  when IOCTL_Set_32 =>
                     Set_Value : Interfaces.Unsigned_32;

                  when others =>
                     null;
               end case;

            when Sleep | Wait_Is_Ready =>
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
           (Kind => Executor.Wait_Is_Ready,
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
            Value        => 0,
            Length       => 4),
         --  37 =>
           (Kind         => IOCTL_Get,
            Variable     => IOCTL.cur_etheraddr,
            Command      => IOCTL.GET_VAR),
         --  38 =>  START JOIN
           (Kind         => Write_Register,  --  Clear pullups
            Address      => Backplane_Register.Pull_Up,
            Value        => 16#0F#,
            Length       => 1),
         --  39 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Pull_Up,
            Value        => 0,
            Length       => 1),
         --  40 =>
           (Kind         => Read_Register,
            Address      => Backplane_Register.Pull_Up,
            Length       => 1),
         --  41 =>
           (Kind => Clear_Error),
         --  Set sleep KSO (should poll to check for success)
         --  42 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Sleep_CSR,
            Value        => 1,
            Length       => 1),
         --  43 =>
           (Kind         => Write_Register,
            Address      => Backplane_Register.Sleep_CSR,
            Value        => 1,
            Length       => 1),
         --  44 =>
           (Kind         => Read_Register,
            Address      => Backplane_Register.Sleep_CSR,
            Length       => 1),
         --  45 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.country,
            Command      => IOCTL.SET_VAR),
         --  46 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_ANTDIV,
            Set_Value    => 0),
         --  47 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.bus_txglom,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 0),
         --  48 =>  FAILS? STATUS=fffb flags=1,7
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.apsta,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 1),
         --  49 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.ampdu_ba_wsize,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 8),
         --  50 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.ampdu_mpdu,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 4),
         --  50 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.ampdu_rx_factor,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 0),
         --  51 =>
           (Kind         => Sleep,
            Milliseconds => 150),
         --  52 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.bsscfg_event_msgs,
            Command      => IOCTL.SET_VAR),
         --  53 =>
           (Kind         => Sleep,
            Milliseconds => 50),
         --  54 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.mcast_list,
            Command      => IOCTL.SET_VAR),
         --  Restart Join
         --
         --  55 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.None,
            Command      => IOCTL.UP),
         --  56 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_GMODE,
            Set_Value    => 1),
         --  57 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_BAND,
            Set_Value    => 0),
         --  58 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.pm2_sleep_ret,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 16#C8#),
         --  59 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.bcn_li_bcn,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 1),
         --  60 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.bcn_li_dtim,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 1),
         --  61 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.assoc_listen,
            Command      => IOCTL.SET_VAR,
            Set_Value    => 10),
         --  62 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_INFRA,
            Set_Value    => 1),
         --  63 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_AUTH,
            Set_Value    => 0),
         --  set security
         --  64 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_WSEC),
         --  65 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.bsscfg_sup_wpa,
            Command      => IOCTL.SET_VAR),
         --  66 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.bsscfg_sup_wpa2_eapver,
            Command      => IOCTL.SET_VAR),
         --  67 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.bsscfg_sup_wpa_tmo,
            Command      => IOCTL.SET_VAR),
         --  68 =>
           (Kind         => Sleep,
            Milliseconds => 2),
         --  69 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_WSEC_PMK),
         --  70 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_INFRA,
            Set_Value    => 1),
         --  71 =>
           (Kind         => IOCTL_Set_32,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_AUTH,
            Set_Value    => 0),
         --  72 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_WPA_AUTH),
         --  73 =>
           (Kind         => IOCTL_Set,
            Variable     => IOCTL.None,
            Command      => IOCTL.SET_SSID)
          ];
   end Executor;

   --------------
   -- Executor --
   --------------

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

      procedure IOCTL_Get
        (Variable : IOCTL.IO_Variable;
         Offset   : in out Natural;
         Buffer   : out Buffer_Byte_Array;
         Command  : IOCTL.Command);

      procedure IOCTL_Set
        (Variable : IOCTL.IO_Variable;
         Offset   : in out Natural;
         Buffer   : out Buffer_Byte_Array;
         Command  : IOCTL.Command);

      procedure IOCTL_Set
        (Variable : IOCTL.IO_Variable;
         Offset   : in out Natural;
         Buffer   : out Buffer_Byte_Array;
         Command  : IOCTL.Command;
         Value    : Interfaces.Unsigned_32);

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

            when IOCTL_Get =>
               Command := Step.Command;
               IOCTL_Get (Step.Variable, Offset, Buffer, Command);

            when IOCTL_Set =>
               Command := Step.Command;
               IOCTL_Set (Step.Variable, Offset, Buffer, Command);

            when IOCTL_Set_32 =>
               Command := Step.Command;
               IOCTL_Set
                 (Step.Variable, Offset, Buffer, Command, Step.Set_Value);

            when Wait_Is_Ready =>
               Value := Boolean'Pos (Bus.Is_Ready);

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

            when Upload_Firmware
               | Upload_CLM
               | IOCTL_Get
               | IOCTL_Set
               | IOCTL_Set_32 =>

               if Offset = 0 then
                  Increment_Step;
               end if;

            when Wait_Is_Ready =>
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

      ---------------
      -- IOCTL_Get --
      ---------------

      procedure IOCTL_Get
        (Variable : IOCTL.IO_Variable;
         Offset   : in out Natural;
         Buffer   : out Buffer_Byte_Array;
         Command  : IOCTL.Command)
      is
         Name : constant Byte_Array := IOCTL.To_Raw_Name (Variable);
      begin
         if Offset = 0 then
            Offset := 1;

            IOCTL_Set (Buffer, Command, Name, [], Write => False);
         else
            Offset := 0;
         end if;
      end IOCTL_Get;

      ---------------
      -- IOCTL_Set --
      ---------------

      procedure IOCTL_Set
        (Variable : IOCTL.IO_Variable;
         Offset   : in out Natural;
         Buffer   : out Buffer_Byte_Array;
         Command  : IOCTL.Command)
      is
         Name : constant Byte_Array := IOCTL.To_Raw_Name (Variable);
         Raw  : constant Byte_Array := IOCTL_Raw_Value (Variable, Command);
      begin
         if Offset = 0 then
            Offset := 1;

            IOCTL_Set (Buffer, Command, Name, Raw, Write => True);
         else
            Offset := 0;
         end if;
      end IOCTL_Set;

      ---------------
      -- IOCTL_Set --
      ---------------

      procedure IOCTL_Set
        (Variable : IOCTL.IO_Variable;
         Offset   : in out Natural;
         Buffer   : out Buffer_Byte_Array;
         Command  : IOCTL.Command;
         Value    : Interfaces.Unsigned_32)
      is
         subtype Word is Byte_Array (1 .. 4);
         function To_Word is new Ada.Unchecked_Conversion
           (Interfaces.Unsigned_32, Word);

         Name : constant Byte_Array := IOCTL.To_Raw_Name (Variable);
         Raw  : constant Byte_Array := To_Word (Value);
      begin
         if Offset = 0 then
            Offset := 1;

            IOCTL_Set (Buffer, Command, Name, Raw, Write => True);
         else
            Offset := 0;
         end if;
      end IOCTL_Set;

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

         Name : constant Positive := IOCTL_Data_Offset;
         Data : constant Positive := Name + Raw_Request'Length;
         To   : constant Positive := Data + Load_Size - 1;
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
            IOCTL_Set_In_Place (Buffer (1 .. Last), Command, Write => True);

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
                  Value        => Value / Window_Size * Window_Size / 256,
                  Length       => 3);
            end if;

            Bus.Write_Backplane
              (Address => Value mod Window_Size,
               Value   => Buffer (1 .. Last));

            Offset := Offset + Block_Size;
         else
            Offset := 0;
         end if;
      end Upload_Firmware;

   end Executor;

   ------------------
   -- Change_State --
   ------------------

   procedure Change_State (State : in out Generic_IO.State) is
   begin
      case State.Joining.Kind is
         when Crashed | Boot_Up =>
            null;

         when Idle =>
            State.Joining :=
              (Kind    => Joining,
               Timeout => Timeouts.New_Timeout (10));

            State.Link := (Failed => False, Up => False, Auth => False);

            --  Restart_Join (State);
         when Joining =>
            if State.Link.Failed
              or else Timeouts.Is_Expired (State.Joining.Timeout)
            then
               State.Joining :=
                 (Kind    => Failed,
                  Timeout => Timeouts.New_Timeout (10));
               --  Stop_Join (State);
            elsif State.Link.Up and State.Link.Auth then
               State.Joining := (Kind => Joined);
            end if;
         when Joined =>
            if State.Link.Failed then
               State.Joining :=
                 (Kind    => Failed,
                  Timeout => Timeouts.New_Timeout (10));
               --  Stop_Join (State);
            end if;
         when Failed =>
            if Timeouts.Is_Expired (State.Joining.Timeout) then
               State.Joining := (Kind => Idle);
            end if;
      end case;
   end Change_State;

   ----------------------
   -- Complete_Reading --
   ----------------------

   procedure Complete_Reading
     (State  : in out Generic_IO.State;
      Buffer : Buffer_Byte_Array;
      From   : out Natural;
      Found  : out Boolean)
   is
      use type IOCTL.Command;
      use type Packets.SDPCM_Channel;

      Got : Packets.Packet;
   begin
      Found := False;
      From := 0;
      State.Reading := 0;

      Packets.Decode_Input (Buffer, Got);

      case Got.Channel is
         when Packets.Event =>
            declare
               Event_Length : constant Positive :=
                 SDPCM.Events.Event_Record'Size / 8;

               subtype Event_Record_Raw is Buffer_Byte_Array (1 .. Event_Length);

               function To_Event_Record is new Ada.Unchecked_Conversion
                 (Event_Record_Raw, SDPCM.Events.Event_Record);

               Event : SDPCM.Events.Event_Record :=
                 (if Buffer'Last - Got.Offset + 1 >= Event_Length
                  then To_Event_Record
                    (Buffer (Got.Offset .. Got.Offset + Event_Length - 1))
                  else
                    (Eventh => (Event_Type => 0, others => <>),
                     others => <>));
            begin
               On_Event (State, Event);
            end;
         when Packets.Control =>
            if Got.IOCTL_Header.Command = IOCTL.Command (State.Command) then
               Found := True;

               if Got.IOCTL_Header.Command = IOCTL.GET_VAR then
                  declare
                     From : constant Natural := Got.IOCTL_Offset;
                     Size : constant Natural := Buffer'Last - From + 1;
                  begin
                     if Size >= State.MAC'Length then
                        State.MAC := Byte_Array
                          (Buffer (From .. From + State.MAC'Length - 1));
                     else
                        raise Program_Error;
                     end if;
                  end;
               end if;
            end if;
         when Packets.Data =>
            From := Got.Offset;
            null;
         when others =>
            null;
      end case;
   end Complete_Reading;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
     (State : in out Generic_IO.State;
      Event : SDPCM.Events.Event_Record)
   is
      WLC_E_LINK         : constant := 16#10_00_00_00#;  --  SWAP32 (16)
      WLC_E_PSK_SUP      : constant := 16#2e_00_00_00#;  --  SWAP32 (46)
      WLC_E_DISASSOC_IND : constant := 16#0c_00_00_00#;  --  SWAP32 (12)
   begin
      if State.Link.Failed then
         null;  --  Do notheng if link is down
      elsif Event.Eventh.Event_Type = WLC_E_LINK
        and Event.Eventh.Status = 0
      then
         State.Link.Up := (Event.Eventh.Flags and 16#01_00#) /= 0;
      elsif Event.Eventh.Event_Type = WLC_E_PSK_SUP then
         State.Link.Auth := Event.Eventh.Status = 16#06_00_00_00#;
      elsif Event.Eventh.Event_Type = WLC_E_DISASSOC_IND then
         State.Link := (Failed => True);
      end if;

      Change_State (State);
   end On_Event;

   -------------
   -- Process --
   -------------

   procedure Poll
     (State  : in out Generic_IO.State;
      Buffer : in out Buffer_Byte_Array;
      Send   : Buffer_Span := Empty;
      Action : out Generic_IO.Action)
   is
      function To_Action
        (Step : Executor.Step) return Generic_IO.Action is
          (case Step.Kind is
              when Executor.Sleep =>
                (Sleep, Milliseconds => Step.Milliseconds),
              when Executor.Read_Register_Until
                | Executor.Wait_Is_Ready =>
                  (if State.Offset = 0 then (Kind => Continue)
                   else (Sleep, Milliseconds => 1)),
              when Executor.Upload_CLM
                | Executor.IOCTL_Get
                | Executor.IOCTL_Set
                | Executor.IOCTL_Set_32 =>
                (if State.Offset = 0 then (Kind => Continue)
                 else (Kind => Complete_IO)),
              when others =>
                (Kind => Continue));

      function Need_Reading return Boolean is
         (case Executor.Start (State.Step).Kind is
             when Executor.Upload_CLM => State.Offset > 0,
             when Executor.IOCTL_Get => State.Offset = 1,
             when Executor.IOCTL_Set => State.Offset = 1,
             when Executor.IOCTL_Set_32 => State.Offset = 1,
             when others => False);

      Ok : Boolean := False;
      Offset : Natural;
   begin
      if Send.From <= Send.To then
         IOCTL_Send_Buffer (Buffer, Send.From, Send.To);
         Action := (Kind => Complete_IO);
         return;
      elsif State.Reading > 0 then
         Action :=
           (Kind => Process_Packet,
            Span => (From => 1, To => State.Reading));

         Complete_Reading
           (State,
            Buffer (1 .. State.Reading),
            Offset,
            Found => Ok);

         if Offset > 0 then
            Action.Span.From := Offset;
            return;
         end if;
      end if;

      if not Ok
        and (State.Step not in Executor.Start'Range or else Need_Reading)
      then
         --  No response found, (no active step or the step needs reading)
         declare
            Length : constant Interfaces.Unsigned_32 :=
              Bus.Available_Packet_Length;
         begin
            if Natural (Length) in Buffer'Range then
               State.Reading := Positive (Length);

               Bus.Start_Reading_WLAN (Buffer (1 .. State.Reading));
               Action := (Kind => Complete_IO);
            elsif Length > 0 then
               raise Program_Error;  --  Buffer too small
            elsif State.Step not in Executor.Start'Range
              and then Bus.Is_Ready
            then
               Change_State (State);
               Action := (Kind => Idle);
            else
               Change_State (State);
               Action := (Sleep, Milliseconds => 1);
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
         Custom_Value => Packets.Make_Tag (16#300# / 4));

      if State.Step not in Executor.Start'Range then
         State.Joining := (Kind => Idle);
         Action := (Kind => Continue);
      else
         Action := To_Action (Executor.Start (State.Step));
      end if;

      if not Ok then
         State.Joining := (Kind => Crashed);
      end if;
   end Poll;

   --------------
   -- Set_GPIO --
   --------------

   procedure Set_GPIO (Value : Interfaces.Unsigned_32) is
   begin
      Bus.Write_Backplane_Register
        (Address => Backplane_Register.GPIO_Out,
         Length  => 4,
         Value   => Value);
   end Set_GPIO;

end SDPCM.Generic_IO;
