# BUILD

To create the bbcbasic.bin executable

### Requirements

- [ZDS II tools](https://zilog.com/index.php?option=com_zcm&task=view&soft_id=38&Itemid=74)
- A hex to binary convertor, for example [hex2bin](https://hex2bin.sourceforge.net)

### Instruuctions

1. Clone the BBC BASIC repository, or download the latest source files
2. Copy the latest [mos_api.inc](https://github.com/breakintoprogram/agon-mos/blob/main/src/mos_api.inc) from the MOS repository to the local BBC BASIC folder
3. Open BBC BASIC.zdsproj in ZDS Studio
4. Click 'Rebuild All' from the build menu
5. This creates a BBC BASIC.hex file in the Debug directory
6. Use hex2bin to convert the hex file to a bin file
7. Rename the bin file bbcbasic.bin

### Creating the SD Card Image

1. Format the SD card FAT32. Note that cards larger than 32GB may not automatically format FAT32
2. Copy bbcbasic.bin to the root folder of the card
3. Copy the following folders from this project to the root of the card:
	- `examples`: Original example files by R.T.Russell
	- `tests`: My example files (see [here](https://github.com/breakintoprogram/agon-bbc-basic/blob/main/tests/README.md) for more details)
	- `resources`: Various resource files used by the sample programs
4. See the [MOS instructions for creating an autoexec.txt file](https://github.com/breakintoprogram/agon-mos#the-autoexectxt-file) for launching BBC BASIC automatically on boot