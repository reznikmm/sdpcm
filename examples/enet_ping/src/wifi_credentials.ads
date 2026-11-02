--  SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  WiFi network credentials. The values come from WIFI_SSID and
--  WIFI_PASSWORD environment variables through the preprocessor (see the
--  project file). Run `alr build -- -s` after changing them to recompile.

package WiFi_Credentials is
   pragma Pure;

   function SSID     return String;
   function Password return String;

end WiFi_Credentials;
