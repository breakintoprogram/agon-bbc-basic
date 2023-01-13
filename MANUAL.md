# manual

## Copying to the SD card

Copy the bbcbasic.bin file to the root folder of the SD card and launch using:
```
LOAD bbcbasic
RUN
```
If you want to launch a BBC BASIC program immediately after running, you can specify a path
```
LOAD bbcbasic
RUN &40000 tests/cube.bbc
```
Note that you have to pass the load address (default is &40000) to the run as subsequent parameters will be passed to the executable.

## BBC BASIC for Z80 (Agon)

In addition to the core BBC Basic for Z80 core language (details of which [can be found here](bbcbasic.txt)), BBC Basic for Agon adds the following functionality:

## Editor

BBC BASIC currently uses the MOS editor

- Use the cursor keys to navigate around the current edit line
- Line lengths must be less than 256 characters

## Star Commands

The following * commands are supported

### *BYE

Exit BASIC and return to MOS.

### •EDIT linenum

Pull a line into the editor for editing.

### *FX osbyte, params

Execute an OSBYTE command.

### *VERSION

Display the current version of BBC BASIC

In addition, any of the MOS commands can be called by prefixing them with a *

* `*CAT`: Directory listing of the current directory. Aliases include `DIR` and `.`
* `*CD path`: Change current directory
* `*LOAD filename <addr>`: Load a file from the SD card to the specified address
* `*MKDIR filename`: Make a folder on the SD card
* `*SAVE filename addr size`: Save a block of memory to the SD card
* `*RUN <addr>`: Call an address in memory (switching to Z80 mode - ADL=0)
* `*DEL filename`: Delete a file or folder (must be empty). Aliases include `ERASE`
* `*REN filename1 filename2`: Rename a file
* `*JMP addr`: Jump to the specified address in memory
* `*SET option value`: Set a system option

See the [MOS documentation](https://github.com/breakintoprogram/agon-mos/blob/main/README.md) for more details

## BASIC

The following statements differ from the BBC Basic standard:

### LOAD
### SAVE

If a file extension is omitted, ".BBC" is assumed. The file is saved as it is stored in memory, in BBC BASIC for Z80 tokenised format. This differs slightly from the Acorn format.

### MODE n

Three modes currently supported by the VDP.
These can be tweaked by modifying the function set_mode in the VDP code.

- `MODE 0`: 640 x 480 @ 60Hz (VGA)
- `MODE 1`: 512 x 384 @ 60hz (VGA)
- `MODE 2`: 320 x 200 @ 75Hz (VGA)

### COLOUR n
### COLOUR r,g,b

If one parameter is passed, it will pick one of 8 primary colours:

- 0: Black
- 1: Red
- 2: Green
- 3: Yellow
- 4: Blue
- 5: Magenta
- 6: Cyan
- 7: White

If three parameters are passed, then the colour will be set to the specified rgb colour where each component is an integer between 0 and 255.

### GCOL mode,n
### GCOL mode,r,g,b

As COLOUR. Mode is currently ignored

### POINT(x,y)

This returns the rgb value of the pixel as a 24-bit integer

### PLOT mode,x,y

Plot supports the following operations:

- 4: Move
- 5: Line
- 80: Filled Triangle
- 144: Circle with radius specified either by x or y
- 148: Circle passing through point x,y

### GET$(x,y)

Returns the ASCII character at position x,y

### VDU

The VDU command is a work-in-progress with a handful of mappings implemented:

- `VDU 8`: Backspace
- `VDU 9`: Advance one character
- `VDU 10`: Line feed
- `VDU 11`: Move cursor up one line
- `VDU 12`: CLS
- `VDU 13`: Carriage return
- `VDU 16`: CLG
- `VDU 18, mode, r, g, b`: GCOL mode, r, g, b
- `VDU 23, n`: VDP commands
- `VDU 22,n`: Mode n
- `VDU 25,mode, x; y;`: PLOT mode,x,y
- `VDU 29, x; y;`: Set graphics origin to x,y
- `VDU 30`: Home cursor
- `VDU 31, x, y`: TAB(x,y)
- `VDU 127`: Backspace

Examples:

`VDU 25,64,128;88;` Plot point in middle of screen
`VDU 22,1` Change to Mode 1

See the [VDP documentation](https://github.com/breakintoprogram/agon-vdp/blob/main/MANUAL.md) for more details
