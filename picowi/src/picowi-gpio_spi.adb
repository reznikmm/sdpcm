--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;

with RP.GPIO;

package body Picowi.GPIO_SPI is

   GP29 : constant RP.GPIO.GPIO_Point := (Pin => 29);

   WL_ON  : RP.GPIO.GPIO_Point := (Pin => 23);
   WL_D   : RP.GPIO.GPIO_Point := (Pin => 24);
   WL_CS  : RP.GPIO.GPIO_Point := (Pin => 25);
   WL_CLK : RP.GPIO.GPIO_Point := GP29;

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

   procedure Configure_GPIO (Power_On : Boolean) is
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

      if Power_On then
         --  delay 0.1; replaced with a loop
         for J in 1 .. 2_000_000 loop
            WL_D.Clear;
         end loop;

         GPIO_SPI.Power_On;
         --  delay 0.05; replaced with a loop
         for J in 1 .. 1_000_000 loop
            WL_D.Clear;
         end loop;

         Configure_PIO;
      end if;
   end Configure_GPIO;

   -------------------
   -- Configure_PIO --
   -------------------

   procedure Configure_PIO is
   begin
      WL_D.Configure (RP.GPIO.Input);
   end Configure_PIO;

   --------------
   -- Power_On --
   --------------

   procedure Power_On is
   begin
      WL_ON.Set;  --  Power ON
   end Power_On;

   ----------
   -- Read --
   ----------

   procedure Read (Data : out SDPCM.Buffer_Byte_Array) is
      use type Interfaces.Unsigned_8;
      Set : Boolean;
   begin
      WL_D.Configure (RP.GPIO.Input);

      for Byte of Data loop
         Byte := 0;
         for J in 1 .. 8 loop
            Set := WL_D.Set;
            --  WL_CLK.Set;
            WL_CLK.Set;

            Byte := Interfaces.Shift_Left (Byte, 1) + Boolean'Pos (Set);
            WL_CLK.Clear;
            --  WL_CLK.Clear;
         end loop;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Data : SDPCM.Buffer_Byte_Array) is
      use type Interfaces.Unsigned_8;
      Next : Interfaces.Unsigned_8;
   begin
      WL_D.Configure (RP.GPIO.Output);

      for Byte of Data loop
         Next := Byte;
         for J in 1 .. 8 loop
            if (Next and 128) /= 0 then
               WL_D.Set;
            else
               WL_D.Clear;
            end if;

            --  WL_CLK.Set;
            WL_CLK.Set;
            Next := Interfaces.Shift_Left (Next, 1);
            WL_CLK.Clear;
            --  WL_CLK.Clear;
         end loop;
      end loop;

      WL_D.Configure (RP.GPIO.Input);
   end Write;

end Picowi.GPIO_SPI;
