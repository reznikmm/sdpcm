--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with System;

with Enet_BCM43362.Generic_Interface;
with Net.Buffers;
with Net.DHCP;
with Net.Interfaces;
with SDPCM.Generic_Mapped_Read_Resource;

package Global is

   procedure Initialize (Success : out Boolean);
   --  Create the network interface, start the receiver task, wait for
   --  joining and start DHCP

   --  Firmware read API implementation
   -------------------------------------

   NVRAM_Length : constant := 1024;

   procedure Read_Resource is new SDPCM.Generic_Mapped_Read_Resource
     (Address         => System'To_Address (16#0804_0000#),
      Firmware_Length => 210432,
      NVRAM_Length    => NVRAM_Length,
      CLM_Blob_Length => 0);
   --  See examples/emw3165/README.md how to flash firmware and NVRAM

   --  Network interface implementation
   ------------------------------------
   function SSID return String is ("guest");

   function Password return String is ("guest123");

   package Network is new SDPCM.Generic_Network
     (SSID, Password, Security => SDPCM.WPA2_AES);

   package WiFi_Interfaces is new Enet_BCM43362.Generic_Interface
     (Read_Resource => Read_Resource,
      NVRAM_Size    => NVRAM_Length,
      Network       => Network);

   MAC : aliased WiFi_Interfaces.Ifnet;

   DHCP : Net.DHCP.Client;

   procedure ICMP_Handler
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type);
   --  Custom ICMP handler to print ICMP echo responses

end Global;
