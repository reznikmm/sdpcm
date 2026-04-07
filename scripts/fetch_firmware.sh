#!/bin/bash
# SPDX-FileCopyrightText: 2026 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT

set -e
FILE=w43439A0_7_95_49_00_combined.h

# Get C header file with Firmware
curl -s -L -o "$FILE" https://raw.githubusercontent.com/georgerobotics/cyw43-driver/main/firmware/w43439A0_7_95_49_00_combined.h

# Get main firmware length
FW_LEN=$(grep CYW43_WIFI_FW_LEN "$FILE" |sed -e 's/.*(//;s/).*//')
# Round it to word size (4 bytes)
FW_LEN=$((($FW_LEN+3)/4*4))

echo Extract $FW_LEN bytes to 43439A0.bin
sed -e '/^[^ ]/d;s/0x//g;s/[ ,]//g' "$FILE" | xxd -r -p |head -c $FW_LEN > 43439A0.bin

echo Extract rest of dump to 43439_raspberrypi_picow_v5_220624.clm_blob
sed -e '/^[^ ]/d;s/0x//g;s/[ ,]//g' "$FILE" | xxd -r -p |tail -c +$((($FW_LEN+511)/512*512+1)) > 43439_raspberrypi_picow_v5_220624.clm_blob

echo Extract nvram_43439.bin as null terminated string list
curl -s -L https://raw.githubusercontent.com/georgerobotics/cyw43-driver/main/firmware/wifi_nvram_43439.h |\
  grep '^  *".*"$'|sed -e 's/^ *"//;s/".*//' | tr '\n' '\000' > nvram_43439.bin

echo Round nvram_43439.bin length to 0x300
truncate --size=$((0x300)) nvram_43439.bin

echo Merge all 3 pieces files into firmware.bin
cat 43439A0.bin nvram_43439.bin 43439_raspberrypi_picow_v5_220624.clm_blob > firmware.bin

echo ""
echo "Piece size:"
echo "     Firmware_Length => $(wc -c < 43439A0.bin),"
echo "     NVRAM_Length    => $(wc -c < nvram_43439.bin),"
echo "     CLM_Blob_Length => $(wc -c < 43439_raspberrypi_picow_v5_220624.clm_blob)"

echo ""
echo "If you want to put it at the end of Pico W flash, run"
echo "  openocd -f board/pico-debug.cfg -c 'program firmware.bin verify reset exit 0x101c0000'"

rm -f "$FILE"
