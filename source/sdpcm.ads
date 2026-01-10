--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  SDPCM is Serial Data / Packet Communication Mechanism.
--  It is used in SDIO/SPI devices from Broadcom, Infineon, Cypress for
--  communication between the host and the device.
--
--  This package defines types used in SDPCM protocol and provides
--  root namespace for driver's packages.

with Interfaces;

package SDPCM is
   pragma Pure;

   type Bus_Function is (Bus, Backplane, WLAN);
   --  Bus function identifiers used in SDPCM headers.
   --
   --  * @enum Bus - SDIO function 0 dedicatet to the bus (SPI/SDIO)
   --  * @enum Backplane - SDIO function 1 (backplane access)
   --  * @enum WLAN - SDIO function 2 (WLAN data)

   type Security_Mode is (None, WPA_TKIP, WPA2_AES);
   --  Security modes supported by the device.
   --
   --  * @enum None - no security
   --  * @enum WPA_TKIP - WPA with TKIP encryption
   --  * @enum WPA2_AES - WPA2 with AES encryption

   type Byte_Array is array (Positive range <>) of Interfaces.Unsigned_8
     with Component_Size => 8;
   --  Array of bytes.

   generic
      with procedure Read_Backplane_Register
        (Address : Interfaces.Unsigned_32;
         Length  : Positive;
         Value   : out Interfaces.Unsigned_32);
      --  Read backplane register from the device.

      with procedure Write_Backplane_Register
        (Address : Interfaces.Unsigned_32;
         Length  : Positive;
         Value   : Interfaces.Unsigned_32);
      --  Write backplane register to the device.

      Write_Prefix_Length : Natural;
      --  Length of the prefix added by Write_Prefix function.

      with function Write_Prefix
        (Bus_Function : SDPCM.Bus_Function;
         Address      : Interfaces.Unsigned_32;
         Length       : Positive) return Byte_Array;
      --  Create prefix for write operation. This allow bus to make a write
      --  operation in a single transaction, but user should take care of
      --  adding this prefix to the data to be written.

      with procedure Start_Writing_WLAN (Value : Byte_Array);
      --  Start asynchronous write to WLAN function.
      --  Value should be prefixed with Write_Prefix with these parameters
      --  * Bus_Function => WLAN
      --  * Address => 0
      --  * Length => Value'Length - Write_Prefix_Length

      with procedure Start_Reading_WLAN (Value : out Byte_Array);
      --  Start asynchronous read from WLAN function.

      with procedure Write_Backplane
        (Address : Interfaces.Unsigned_32;
         Value   : Byte_Array);
      --  Write to backplane function synchronously.
      --  Value should be prefixed with Write_Prefix with these parameters
      --  * Bus_Function => Backplane
      --  * Address => Address
      --  * Length => Value'Length - Write_Prefix_Length

      with function Has_Event return Boolean;
      --  Check if there is an event from the bus.

      with function Available_Packet_Length return Interfaces.Unsigned_32;
      --  Get length of the available packet to read. Returns 0 if no packet
      --  is available.

      with function Is_Ready_To_Send return Boolean;
      --  Check if the bus is ready to send data.

      with procedure Clear_Error;
      --  Clear any error state on the bus.

   package Generic_Bus is
   end Generic_Bus;
   --  Generic Bus Interface

   generic
      type Timeout is private;
      with function New_Timeout (Second : Natural) return Timeout;
      with function Is_Expired (Value : Timeout) return Boolean;
   package Generic_Timeouts is
   end Generic_Timeouts;
   --  Generic Timeouts Interface

   type Resource_Kind is (Firmware, NVRAM, CLM_Blob);
   --  Kinds of resources used by the driver.
   --
   --  * @enum Firmware - firmware binary
   --  * @enum NVRAM - NVRAM configuration
   --  * @enum CLM_Blob - CLM blob for regulatory compliance

   generic
      with procedure Read_Resource
        (Kind : Resource_Kind;
         Data : out Byte_Array;
         Last : out Natural);
      --  Read resource of given kind.
   package Generic_Resources is
   end Generic_Resources;
   --  Generic Resources Interface

   generic
      with function SSID return String;
      --  Get SSID for the network to connect to.

      with function Password return String;
      --  Get password for the network to connect to.

      with function Security return SDPCM.Security_Mode;
   package Generic_Network is
   end Generic_Network;
   --  Generic Network Configuration Interface

end SDPCM;
