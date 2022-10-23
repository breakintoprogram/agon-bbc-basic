# BUILD

To create the bbcbasic.bin executable

### Requirements:

- [ZDS II tools](https://zilog.com/index.php?option=com_zcm&task=view&soft_id=38&Itemid=74)
- A hex to binary convertor, for example [hex2bin](https://hex2bin.sourceforge.net)

### Instruuctions:

1. Clone the BBC BASIC repository, or download the latest source files
2. Copy the latest [mos_api.inc](https://github.com/breakintoprogram/agon-mos/blob/main/src/mos_api.inc) from the MOS repository to the local BBC BASIC folder
3. Open BBC BASIC.zdsproj in ZDS Studio
4. Click 'Rebuild All' from the build menu
5. This creates a BBC BASIC.hex file in the Debug directory
6. Use hex2bin to convert the hex file to a bin file
7. Rename the bin file bbcbasic.bin and copy to the SD card
