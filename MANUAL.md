# manual

In addition to the core BBC Basic for Z80 core language (details of which [can be found here](bbcbasic.txt)), BBC Basic for Agon adds the following functionality:

## Editor

BBC BASIC currently uses the MOS editor - this is not fully implemented yet.

## BASIC

The following statements differ from the BBC Basic standard:

### MODE n

Two modes currently supported by the VDP

- `MODE 2`: 320 x 240 @ 75Hz
- `MODE 6`: 640 x 480 @ 60Hz with a 640 x 400 canvas

### GCOL mode, r,g,b

Set the graphics colour to the specified rgb colour. Each component is a number between 0 and 255

### VDU

The VDU command is a work-in-progress with a handful of mappings implemented:

- `VDU 8` Backspace
- `VDU 9` Advance one character
- `VDU 10` Line feed
- `VDU 11` Move cursor up one line
- `VDU 12` CLS
- `VDU 13` Carriage return
- `VDU 16` CLG
- `VDU 18,mode,r,g,b` GCOL mode,r,g,b
- `VDU 22,n` Mode n
- `VDU 25,mode,x;y;` PLOT mode,x,y
- `VDU 29,x;y;` Set graphics origin to x,y
- `VDU 30` Home cursor
- `VDU 31,x,y` TAB(x,y)

Examples:

`VDU 25,64,128;88;` Plot point in middle of screen

`VDU 22,1` Change to Mode 1