--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Real_Time;

with Net.Generic_Receiver;
with Net.Headers;
with Net.Protos.Icmp;
with Net.Utils;

with Console;

package body Global is

   Buffer_Memory : array (1 .. 16 * 1024) of Net.Uint8
     with Alignment => 4;

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
      IP   : constant Net.Headers.IP_Header_Access := Packet.IP;
      ICMP : constant Net.Headers.ICMP_Header_Access := Packet.ICMP;
   begin
      if ICMP.Icmp_Type = Net.Headers.ICMP_ECHO_REPLY then
         Console.Put_Line
           (Packet.Get_Length'Image
            & " bytes from "
            & Net.Utils.To_String (IP.Ip_Src)
            & " seq="
            & Net.Headers.To_Host (ICMP.Icmp_Seq)'Image);
      else
         Net.Protos.Icmp.Receive (Ifnet, Packet);
      end if;
   end ICMP_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Success : out Boolean) is
      use type Ada.Real_Time.Time;

      type Interface_Status is (Starting, Joining, Failed, Joined);

      function Status return Interface_Status is
        (if MAC.Is_Starting then Starting
         elsif MAC.Is_Joined then Joined
         elsif MAC.Is_Failed then Failed
         else Joining);

      Next     : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Previous : Interface_Status := Starting;
   begin
      MAC.Create (Success);

      if Success then
         Console.Put_Line ("BCM43362 found, loading firmware");
      else
         return;
      end if;

      Net.Buffers.Add_Region
        (Addr => Buffer_Memory'Address,
         Size => Buffer_Memory'Length);

      LAN_Receiver.Start;

      while Previous /= Joined loop
         Next := Next + Ada.Real_Time.Milliseconds (100);
         delay until Next;

         if Status /= Previous then
            Previous := Status;
            Console.Put_Line (Previous'Image);
         end if;
      end loop;

      Console.Put_Line
        ("Joined network! MAC " & Net.Utils.To_String (MAC.Mac));

      DHCP.Initialize (MAC'Access);
   end Initialize;

end Global;
