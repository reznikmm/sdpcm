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
* No-tasking Ada Runtime
* Raspberry Pi Pico W board
* No IP stack required

**Dependencies:** `pico_bsp`, `picowi`, `sdpcm`

Build the example:

    alr -C examples/blink build

### `enet_ping` - Full Network Example

**Ping example with ENet IP stack and light_tasking_rp2040 runtime**

Located in `./examples/enet_ping`

This example demonstrates full network functionality using the ENet IP stack
and can send/receive IP packets, including ICMP ping. It uses
the `light_tasking_rp2040` runtime which provides task and protected object
support.

**Requirements:**
* Ada Runtime with task/protected objects support (`light_tasking_rp2040`)
* Raspberry Pi Pico W board
* ENet IP stack

**Dependencies:** `light_tasking_rp2040`, `enet_picowi`

Build the example:

    alr -C examples/enet_ping build

## Firmware

**Note:** Firmware blobs for Broadcom WiFi chipsets are not included in this
repository. 

Information on downloading and flashing firmware is **TBD** (To Be Done).

## License

This project is licensed under the MIT License. See the `LICENSES/` directory
for full licensing information.
