# agon-bbc-basic

A port of BBC Basic for Z80 to the Agon

### What is the Agon

Agon is a modern, fully open-source, 8-bit microcomputer and microcontroller in one small, low-cost board. As a computer, it is a standalone device that requires no host PC: it puts out its own video (VGA), audio (2 identical mono channels), accepts a PS/2 keyboard and has its own mass-storage in the form of a µSD card.

https://www.thebyteattic.com/p/agon.html

### What is BBC Basic for Z80?

The original version of BBC Basic was written by Sophie Wilson at Acorn in 1981 for the BBC Micro range of computers, and was designed to support the UK Computer Literacy Project. R.T.Russell was involved in the specification of BBC Basic, and wrote his own Z80 version that was subsequently ported to a number of Z80 based machines. [I highly recommend reading his account of this on his website for more details](http://www.bbcbasic.co.uk/bbcbasic/history.html).

As an aside, R.T.Russell still supports BBC Basic, and has ported it for a number of modern platforms, including Android, Windows, and SDL, which are [available from his website here](https://www.bbcbasic.co.uk/index.html).

### Why am I doing this?

I've worked with Bernardo Kastrup aka The Byte Attic on another one of his projects, porting BBC Basic for Z80 to the Cerberus 2080. When he decided to design the Agon, he asked me whether I'd be interested in providing the firmware for it.

### Assembling and Running

This project is designed to be assembled and linked using the [Zilog ZDS II toolkit](https://t.co/dT5DHWYcB5). You will also need a ZUSBSC00100ZACG USB Smart Cable to connect to the Agon in order to upload this to download and run.

NB:
- It currently compiles to load in RAM at &40000, so can be downloaded once compiled.
- Source level debugging not working correctly as this runs in a 64K Z80 segment in the eZ80 RAM

### License

This code is distributable under the terms of a zlib license. Read the file [COPYING](COPYING) for more information.

The BASIC interpreter, as originally written by R.T. Russell and [downloaded from David Given's GitHub page](https://github.com/davidgiven/cpmish/tree/master/third_party/bbcbasic), has been modified slightly, either for compatibility reasons when assembling using sjasmplus, or for development reasons for this release:

The original files are: [eval.z80](eval.z80), [exec.z80](exec.z80), [fpp.z80](fpp.z80), [patch.z80](patch.z80), [ram.z80](ram.z80) and [sorry.z80](sorry.z80).

The source code is equivalent to the code originally authored by R.T.Russell, downloaded on David Given's website: 

http://cowlark.com/2019-06-14-bbcbasic-opensource/index.html

Any additions or modifications I've made to port this to the Agon have been released under the same licensing terms as the original code, along with any tools, examples or utilities contained within this project. Code that has been copied or inspired by other sources is clearly marked, with the appropriate accreditations.

Dean Belfield

Twitter: [@breakintoprogram](https://twitter.com/BreakIntoProg)
Blog: http://www.breakintoprogram.co.uk
