--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with System;

with Enet_Picowi.Generic_Interface;
with Net.Buffers;
with Net.DHCP;
with Net.Interfaces;
with SDPCM.Generic_Mapped_Read_Resource;

with WiFi_Credentials;

package Global is

   procedure Initialize;

   --  Firmware read APU implementation
   -------------------------------------

   procedure Read_Resource is new SDPCM.Generic_Mapped_Read_Resource
     (Address         => System'To_Address (16#101c0000#),
      Firmware_Length => 224192,
      NVRAM_Length    => 16#300#,
      CLM_Blob_Length => 984);

   --  Network interface implementation
   ------------------------------------

   package Network is new SDPCM.Generic_Network
     (WiFi_Credentials.SSID,
      WiFi_Credentials.Password,
      Security => SDPCM.WPA2_AES);

   package Picowi_Interfaces is new Enet_Picowi.Generic_Interface
     (Read_Resource,
      Network);

   MAC : aliased Picowi_Interfaces.Ifnet;

   DHCP : Net.DHCP.Client;

   procedure ICMP_Handler
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type);
   --  Custom ICMP handler to print ICMP echo responses

end Global;
