--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package body WiFi_Credentials is
   function SSID     return String is ($WIFI_SSID);
   function Password return String is ($WIFI_PASSWORD);
end WiFi_Credentials;
