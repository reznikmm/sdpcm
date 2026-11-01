--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Real_Time;

with Net.Buffers;
with Net.DHCP;
with Net.Protos.Arp;
with Net.Protos.Dispatchers;
with Net.Protos.Icmp;
with Net.Protos.IPv4;
with Net.Utils;

with Console;
with Global;

procedure EMW3165_Ping is
   use type Ada.Real_Time.Time;
   use type Net.Ip_Addr;

   procedure Send_Ping (Host : Net.Ip_Addr; Sequence : in out Net.Uint16);
   --  Send ICMP Echo Request to given Host and Sequence. Increment Sequence.

   ---------------
   -- Send_Ping --
   ---------------

   procedure Send_Ping (Host : Net.Ip_Addr; Sequence : in out Net.Uint16) is
      Packet : Net.Buffers.Buffer_Type;
      Status : Net.Error_Code;
   begin
      Net.Buffers.Allocate (Packet);

      if not Packet.Is_Null then
         Packet.Set_Length (64);
         Net.Protos.Icmp.Echo_Request
           (Ifnet     => Global.MAC,
            Target_Ip => Host,
            Packet    => Packet,
            Seq       => Sequence,
            Ident     => 1234,
            Status    => Status);
         Sequence := Net.Uint16'Succ (Sequence);
      end if;
   end Send_Ping;

   Success             : Boolean;
   Previous_DHCP_State : Net.DHCP.State_Type := Net.DHCP.STATE_INIT;
   Gateway             : Net.Ip_Addr := (0, 0, 0, 0);
   Sequence            : Net.Uint16 := 0;
   Next                : Ada.Real_Time.Time := Ada.Real_Time.Clock;
begin
   Console.Initialize;
   Console.Put_Line ("EMW3165 ping demo");

   Global.Initialize (Success);

   if not Success then
      Console.Put_Line ("Initialization failed");
      return;
   end if;

   declare
      Ignore : Net.Protos.Receive_Handler;
   begin
      Net.Protos.Dispatchers.Set_Handler
        (Proto    => Net.Protos.IPv4.P_ICMP,
         Handler  => Global.ICMP_Handler'Access,
         Previous => Ignore);
   end;

   loop
      declare
         use all type Net.DHCP.State_Type;
         Ignore     : Ada.Real_Time.Time;
         DHCP_State : Net.DHCP.State_Type;
      begin
         Net.Protos.Arp.Timeout (Global.MAC);
         Global.DHCP.Process (Ignore);
         DHCP_State := Global.DHCP.Get_State;

         if DHCP_State /= Previous_DHCP_State then
            Previous_DHCP_State := DHCP_State;
            Console.Put_Line (DHCP_State'Image);

            if DHCP_State = STATE_BOUND then
               Console.Put_Line
                 (Net.Utils.To_String (Global.DHCP.Get_Config.Ip));
               Gateway := Global.DHCP.Get_Config.Router;

               if Gateway = (0, 0, 0, 0) then
                  Gateway := Global.DHCP.Get_Config.Server;
               end if;
            end if;
         end if;

         if Gateway /= (0, 0, 0, 0) then
            Send_Ping (Gateway, Sequence);
         end if;

         Next := Next + Ada.Real_Time.Seconds (1);
         delay until Next;
      end;
   end loop;
end EMW3165_Ping;
