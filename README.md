SDPCM
=====

[![Build status](https://github.com/reznikmm/sdpcm/actions/workflows/alire.yml/badge.svg)](https://github.com/reznikmm/sdpcm/actions/workflows/alire.yml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/sdpcm.json)](https://alire.ada.dev/crates/sdpcm.html)
[![REUSE status](https://api.reuse.software/badge/github.com/reznikmm/sdpcm)](https://api.reuse.software/info/github.com/reznikmm/sdpcm)


> SDPCM (Software-defined Peripheral Control Module) driver for
> Broadcom WiFi chipsets

This repository provides Ada drivers for Broadcom WiFi chipsets
using the SDPCM protocol, enabling wireless connectivity for embedded systems.

## Crates

This repository contains three crates:

### `sdpcm`

**WiFi driver based on SDPCM/BCDC host implementation in Ada**

The main SDPCM protocol driver that handles low-level communication with
Broadcom WiFi chipsets. This is the core protocol implementation that other
crates build upon.

**Supported devices:**
* CYW43439 (with Raspberry Pi Pico W)

### `picowi`

**WiFi driver for Raspberry Pi Pico W**

Located in `./picowi` - provides hardware-specific support for Raspberry
Pi Pico W integration with the CYW43439 wireless chip. This crate depends
on `sdpcm`, `rp2040_hal`, and `pico_bsp`.

### `enet_picowi`

**ENet WiFi driver for Raspberry Pi Pico W**

Located in `./enet_picowi` - provides
[ENet IP stack](https://github.com/stcarrez/ada-enet)
integration for network communication. This crate combines the `picowi`
driver with the ENet lightweight IP stack, enabling TCP/IP networking
capabilities.

## Install

Add crates as dependencies to your project with Alire:

    alr with sdpcm
    alr with picowi
    alr with enet_picowi

## Usage

The SDPCM driver uses generic packages to provide flexibility in
configuring firmware access and network parameters. When instantiating
the driver's `Enet_Picowi.Generic_Interface` and `SDPCM.Generic_IO`
package, you need to provide two key interfaces:

### Firmware Access Interface

The driver requires access to firmware binary blobs through a generic
procedure:

```ada
with procedure Read_Resource
  (Kind   : SDPCM.Resource_Kind;
   Offset : Natural;
   Data   : out SDPCM.Byte_Array;
   Last   : out Natural);
```

This procedure reads firmware resources of the following kinds:
* **Firmware** - the main firmware binary for the WiFi chip
* **NVRAM** - NVRAM configuration data
* **CLM_Blob** - Country Localization Module blob for regulatory compliance

The implementation is responsible for providing access to these firmware
blobs, which can be stored in flash memory, SD card, or embedded as constants
in your application.
The current driver implementation has a limitation that the NVRAM size must
be 0x300 bytes.

#### Helper for Memory-Mapped Firmware

For the common case where all three firmware blobs are stored sequentially
in memory without gaps, the driver provides a ready-to-use generic procedure:

```ada
generic
   Address         : System.Address;
   Firmware_Length : Positive;
   NVRAM_Length    : Positive;
   CLM_Blob_Length : Positive;
procedure SDPCM.Generic_Mapped_Read_Resource
  (Kind   : Resource_Kind;
   Offset : Natural;
   Data   : out Byte_Array;
   Last   : out Natural);
```

This can be instantiated directly to provide the `Read_Resource` procedure
when your firmware blobs are embedded in a contiguous memory region.

### Network Configuration Interface

Network parameters (SSID, password, and security mode) are provided through
a generic package:

```ada
with package Network is new SDPCM.Generic_Network (<>);
```

The `SDPCM.Generic_Network` package requires three functions:

```ada
generic
   with function SSID return String;
   --  Get SSID for the network to connect to

   with function Password return String;
   --  Get password for the network to connect to

   with function Security return SDPCM.Security_Mode;
   --  Get security mode (None, WPA_TKIP, WPA2_AES)
package Generic_Network is
   --  Generic Network Configuration Interface
end Generic_Network;
```

### Example Instantiation

```ada
--  Define network configuration
function SSID return String is ("guest");

function Password return String is ("guest123");

package My_Network is new SDPCM.Generic_Network
  (SSID     => Get_SSID,
   Password => Get_Password,
   Security => SDPCM.WPA2_AES);

--  Instantiate the driver
package Picowi_Interfaces is new Enet_Picowi.Generic_Interface
  (Read_Resource => Read_Resource,
   Network       => Network);
```

## Examples

The repository includes two example applications demonstrating different
usage scenarios:

### `blink` - Minimal Runtime Example

**Blink WiFi LED demo**

Located in `./examples/blink`

This example runs on a no-tasking Ada Runtime and demonstrates basic WiFi
functionality by blinking the onboard WiFi LED. It does not require a full
IP stack and cannot send IP packets - perfect for testing basic WiFi chip
communication.

**Requirements:**
* Raspberry Pi Pico W board
* [pico-debug](https://github.com/majbthrd/pico-debug/) and OpenOCD
* Download and flash [CYW43439 Firmware](#firmware)
* No-tasking Ada Runtime
* No IP stack required

**Dependencies:** `pico_bsp`, `picowi`, `sdpcm`

Build and flash the example:

    alr -C examples/blink build
    cd examples/blink/bin
    openocd -f board/pico-debug.cfg -c 'program blink.bin verify reset exit 0x10000000'

### `enet_ping` - Full Network Example

**Ping example with ENet IP stack and light_tasking_rp2040 runtime**

Located in `./examples/enet_ping`

This example demonstrates full network functionality using the ENet IP stack
and can send/receive IP packets, including ICMP ping. It uses
the `light_tasking_rp2040` runtime which provides task and protected object
support.
This example uses Put_Line output in ARM semihosting mode and therefore only
works when connected to a debugger. You can remove all Put_Line output, which
will allow it to work without a debugger, but without any messages.

**Requirements:**
* Raspberry Pi Pico W board
* [pico-debug](https://github.com/majbthrd/pico-debug/) and OpenOCD
* Download and flash [CYW43439 Firmware](#firmware)
* Ada Runtime with task/protected objects support (`light_tasking_rp2040`)
* ENet IP stack

**Dependencies:** `light_tasking_rp2040`, `enet_picowi`

Build the example:

1. Use GNAT Studio to build, flash and run the demo in debugger.
   See ping in "Messages" View while debugger is running.

2. Or use command line

```shell
alr -C examples/enet_ping build
cd examples/enet_ping
openocd -f board/pico-debug.cfg -c 'program bin/enet_ping.bin verify reset exit 0x10000000'
openocd -f board/pico-debug.cfg
```
   In second terminal run gdb: `alr -C examples/enet_ping exec arm-eabi-gdb`
   ```
   target extended-remote :3333
   monitor arm semihosting enable
   mon reset init
   cont
   ```
   See ping in OpenOCD terminal.

## Firmware

**Note:** Firmware blobs for Broadcom WiFi chipsets are not included in this
repository for intellectual property reasons, i.e. to avoid unnecessarily
mixing free with non-free code.

### Information on downloading and flashing firmware.

* Version: 7.95.49 (2271bb6 CY) CRC: b7a28ef3 Date: Mon 2021-11-29 22:50:27 PST Ucode Ver: 1043.2162 FWID 01-c51d9400
  * Source: https://github.com/georgerobotics/cyw43-driver
  * CLM RaspberryPi.PicoW 1.29.4 ClmImport: 1.47.1 v5 22/06/24
  * *Note*: NVRAM was changed in Nov 2024 (commit `cf924bb`) to set
    > `pa2ga0=-168,7161,-820`
  * This version is used in `pico-sdk`, but stored in C `.h` files.
    To fetch and convert them to binaries run:
	```shell
	./scripts/fetch_firmware.sh
	```

* Version: 7.95.61 (abcd531 CY) CRC: 4528a809 Date: Wed 2023-01-11 10:29:38 PST Ucode Ver: 1043.2169 FWID 01-7afb0879
  * Source: https://github.com/yogaxpto/pico-conduit/ cyw43-firmware/
  * The same CLM RaspberryPi.PicoW 1.29.4 ClmImport: 1.47.1 v5 22/06/24
  * NVRAM still uses `pa2ga0=-168,6649,-778`
  * **I haven't tried it yet**.

* Version: 7.95.62 (b03806e CY) CRC: ffda5346 Date: Sun 2023-04-02 23:45:28 PDT Ucode Ver: 1043.2169 FWID 01-d07ed25
  * Source: https://github.com/tabemann/cyw43-firmware
  * https://github.com/tabemann/zeptoforth/blob/master/extra/rp_common/cyw43/cyw43_nvram.fs
  * CLM 9.10.39 1.29.4 ClmImport: 1.36.3 Broadcom-0.0 2023-04-02 
  * **I haven't tried it yet**.

* Version: 7.95.88 (cf1d613 CY) CRC: e12ff094 Date: Wed 2024-06-05 06:36:00 PDT Ucode Ver: 1043.2171 FWID 01-7b7cf51a
  * Source: https://github.com/Infineon/wifi-host-driver/blob/release-v4.3.1/WHD/COMPONENT_WIFI5/resources/firmware/COMPONENT_43439/43439A0.bin
  * CLM 9.10.39 1.29.4 ClmImport: 1.36.3 Broadcom-0.0 2024-04-16
    * Source: https://github.com/Infineon/wifi-resources/raw/refs/tags/release-v2.0.4/clm/COMPONENT_WIFI5/COMPONENT_43439/COMPONENT_MURATA-1YN/43439A0.clm_blob
  * NVRAM: N/A
  * **I haven't tried it yet**.

## License

This project is licensed under the MIT License. See the `LICENSES/` directory
for full licensing information.
