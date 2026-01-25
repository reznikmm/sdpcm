--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Text_IO;
with Net.Generic_Receiver;
with Net.Headers;
with Net.Protos.Icmp;
with Net.Utils;

package body Global is

   Buffer_Memory : array (1 .. 16 * 1024) of Net.Uint8;

   package LAN_Receiver is new Net.Generic_Receiver
     (Net.Interfaces.Ifnet_Type'Class (MAC));

   ------------------
   -- ICMP_Handler --
   ------------------

   procedure ICMP_Handler
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      use type Net.Uint8;
      IP : constant Net.Headers.IP_Header_Access := Packet.IP;
      ICMP : constant Net.Headers.ICMP_Header_Access := Packet.ICMP;
   begin
      if ICMP.Icmp_Type = Net.Headers.ICMP_ECHO_REPLY then
         Ada.Text_IO.Put (Packet.Get_Length'Image);
         Ada.Text_IO.Put (" bytes from ");
         Ada.Text_IO.Put (Net.Utils.To_String (IP.Ip_Src));
         Ada.Text_IO.Put (" seq=");
         Ada.Text_IO.Put (Net.Headers.To_Host (ICMP.Icmp_Seq)'Image);
         Ada.Text_IO.New_Line;
      else
         Net.Protos.Icmp.Receive (Ifnet, Packet);
      end if;
   end ICMP_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      MAC.Create;

      Net.Buffers.Add_Region
        (Addr => Buffer_Memory'Address,
         Size => Buffer_Memory'Length);

      LAN_Receiver.Start;

      while not MAC.Is_Joined loop
         delay 0.1;
      end loop;

      Ada.Text_IO.Put_Line ("Joined network!");

      DHCP.Initialize (MAC'Access);
   end Initialize;

end Global;
