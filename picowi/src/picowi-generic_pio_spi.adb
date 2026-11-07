--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Picowi.PIO_SPI_Code;

package body Picowi.Generic_PIO_SPI is

   Writing : Boolean := False;
   --  The last asynchronous transfer was a write

   procedure Wait_Write_End;
   --  Wait until the PIO program shifts out all data and stalls, then
   --  switch the data pin to input

   -----------------
   -- Chip_Select --
   -----------------

   procedure Chip_Select (On : Boolean) is
   begin
      if On then
         WL_CS.Clear;
      else
         WL_CS.Set;
      end if;
   end Chip_Select;

   --------------------
   -- Configure_GPIO --
   --------------------

   procedure Configure_GPIO is
   begin
      --  The WLAN host interface supports gSPI and SDIO v2.0 modes.
      --  This SDIO_DATA_2 pin selects the WLAN host interface mode. The
      --  default is SDIO. For gSPI, pull this pin low.
      --  Strapping Options sampling occurs a few milliseconds after an
      --  Power-On Reset.

      WL_ON.Configure (RP.GPIO.Output);
      WL_ON.Clear;  --  Power OFF

      WL_CS.Configure (RP.GPIO.Output);
      WL_CS.Set;

      WL_CLK.Configure (RP.GPIO.Output);
      WL_CLK.Clear;

      WL_D.Configure (RP.GPIO.Output);
      WL_D.Clear;
   end Configure_GPIO;

   -------------------
   -- Configure_PIO --
   -------------------

   procedure Configure_PIO is

      Config : RP.PIO.PIO_SM_Config := RP.PIO.Default_SM_Config;

   begin
      P.Enable;
      P.Load (Picowi.PIO_SPI_Code.Picowi_Pio_Program_Instructions, 0);

      --  Set I/O pins to be PIO controlled
      WL_CLK.Configure (RP.GPIO.Output, Func => P.GPIO_Function);
      WL_D.Configure (RP.GPIO.Output, Func => P.GPIO_Function);

      RP.PIO.Set_Wrap
        (Config,
         Wrap_Target => Picowi.PIO_SPI_Code.Picowi_Pio_Wrap_Target,
         Wrap        => Picowi.PIO_SPI_Code.Picowi_Pio_Wrap);

      RP.PIO.Set_Sideset (Config, 1, False, False);

      --  Configure data pin as I/O, clock pin as O/P (sideset)
      RP.PIO.Set_Out_Pins (Config, WL_D.Pin, 1);
      RP.PIO.Set_In_Pins (Config, WL_D.Pin);
      RP.PIO.Set_Sideset_Pins (Config, WL_CLK.Pin);

      --  Get 8 bits from FIFOs, disable auto-pull & auto-push
      RP.PIO.Set_Out_Shift (Config, False, False, 8);
      RP.PIO.Set_In_Shift (Config, False, False, 8);

      RP.PIO.Set_Clkdiv_Int_Frac (Config, 1, 0);

      P.SM_Initialize (SM, 0, Config);
      P.Clear_FIFOs (SM);
      P.Set_Enabled (SM, True);
      P.Set_Pin_Direction (SM, WL_CLK.Pin, RP.PIO.Output);
      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Input);

      RP.DMA.Enable;
   end Configure_PIO;

   --------------
   -- Power_On --
   --------------

   procedure Power_On is
   begin
      WL_ON.Set;  --  Power ON
   end Power_On;

   --------------
   -- Read_SPI --
   --------------

   procedure Read_SPI (Data : out HAL.UInt8_Array) is
      use type HAL.UInt32;
   begin
      P.Execute (SM, Picowi.PIO_SPI_Code.Offset_reader);
      P.Put (SM, Data'Length - 1);
      for Item of Data loop
         declare
            Value : HAL.UInt32;
         begin
            P.Get (SM, Value);
            Item := HAL.UInt8 (Value);
         end;
      end loop;
   end Read_SPI;

   ---------------
   -- Write_SPI --
   ---------------

   procedure Write_SPI (Data : HAL.UInt8_Array) is
   begin
      P.Clear_FIFOs (SM);
      P.Execute (SM, Picowi.PIO_SPI_Code.Offset_writer);
      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Output);

      for Item of Data loop
         P.Put (SM, HAL.Shift_Left (HAL.UInt32 (Item), 24));
      end loop;

      Wait_Write_End;
   end Write_SPI;

   ----------------
   -- Finish_SPI --
   ----------------

   procedure Finish_SPI is
   begin
      if Writing then
         Wait_Write_End;
         Writing := False;
      end if;
   end Finish_SPI;

   -----------------
   -- Is_DMA_Busy --
   -----------------

   function Is_DMA_Busy return Boolean is (RP.DMA.Busy (DMA_Channel));

   --------------------
   -- Start_Read_SPI --
   --------------------

   procedure Start_Read_SPI (Data : out HAL.UInt8_Array) is
      use type HAL.UInt32;
   begin
      --  Each RX FIFO entry has a byte in bits 7 .. 0, so a byte read of
      --  the FIFO register takes it.
      RP.DMA.Configure
        (DMA_Channel,
         (Data_Size       => RP.DMA.Transfer_8,
          Increment_Read  => False,
          Increment_Write => True,
          Chain_To        => DMA_Channel,  --  No chaining
          Trigger         => RX_Trigger,
          others          => <>));

      P.Execute (SM, Picowi.PIO_SPI_Code.Offset_reader);

      RP.DMA.Start
        (DMA_Channel,
         From  => P.RX_FIFO_Address (SM),
         To    => Data'Address,
         Count => Data'Length);

      P.Put (SM, Data'Length - 1);
      Writing := False;
   end Start_Read_SPI;

   ---------------------
   -- Start_Write_SPI --
   ---------------------

   procedure Start_Write_SPI (Data : HAL.UInt8_Array) is
   begin
      --  A byte write to the TX FIFO register is replicated to all byte
      --  lanes, so bits 31 .. 24 get the byte as the PIO program expects.
      RP.DMA.Configure
        (DMA_Channel,
         (Data_Size       => RP.DMA.Transfer_8,
          Increment_Read  => True,
          Increment_Write => False,
          Chain_To        => DMA_Channel,  --  No chaining
          Trigger         => TX_Trigger,
          others          => <>));

      P.Clear_FIFOs (SM);
      P.Execute (SM, Picowi.PIO_SPI_Code.Offset_writer);
      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Output);

      RP.DMA.Start
        (DMA_Channel,
         From  => Data'Address,
         To    => P.TX_FIFO_Address (SM),
         Count => Data'Length);

      Writing := True;
   end Start_Write_SPI;

   --------------------
   -- Wait_Write_End --
   --------------------

   procedure Wait_Write_End is
   begin
      while not P.TX_FIFO_Empty (SM) loop
         null;
      end loop;

      while RP.PIO.Current_Instruction_Address (P, SM) /=
        Picowi.PIO_SPI_Code.Offset_writer
      loop
         null;
      end loop;

      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Input);
      P.Execute (SM, Picowi.PIO_SPI_Code.Offset_stall);
   end Wait_Write_End;

end Picowi.Generic_PIO_SPI;
