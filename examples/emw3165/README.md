
## Flash firmware

1. Pad BCM43362 firmware to 64 block (I have 210432 bytes)
1. Pad NVRAM to 64 block (I have 1024 bytes)
2. Flash BCM43362 firmware and NVRAM content at the end of STM32 flash:

```bash
truncate -s %64 43362A2.bin
truncate -s %64 nvram.bin
ls -l 43362A2.bin
cat 43362A2.bin nvram.bin > fw.bin
st-flash --connect-under-reset write fw.bin 0x08040000
```
