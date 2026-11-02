--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with System;

with Enet_BCM43362.Generic_Interface;
with Net.Buffers;
with Net.DHCP;
with Net.Interfaces;
with SDPCM.Generic_Mapped_Resources;

with WiFi_Credentials;

package Global is

   procedure Initialize (Success : out Boolean);
   --  Create the network interface, start the receiver task, wait for
   --  joining and start DHCP

   --  Firmware read API implementation
   -------------------------------------

   package Firmware is new SDPCM.Generic_Mapped_Resources
     (Address         => System'To_Address (16#0804_0000#),
      Firmware_Length => 210432,
      NVRAM_Length    => 1024,
      CLM_Blob_Length => 0);
   --  See README.md how to flash firmware and NVRAM

   --  Network interface implementation
   ------------------------------------

   package Network is new SDPCM.Generic_Network
     (WiFi_Credentials.SSID,
      WiFi_Credentials.Password,
      Security => SDPCM.WPA2_AES);

   package WiFi_Interfaces is new Enet_BCM43362.Generic_Interface
     (Resources => Firmware.Resources,
      Network   => Network);

   MAC : aliased WiFi_Interfaces.Ifnet;

   DHCP : Net.DHCP.Client;

   procedure ICMP_Handler
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type);
   --  Custom ICMP handler to print ICMP echo responses

end Global;
