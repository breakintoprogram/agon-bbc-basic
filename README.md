# ago-bbc-basic

A port of BBC Basic for Z80 to the Agon

### What is the Agon

https://www.thebyteattic.com/p/cerberus-2080.html

### What is BBC Basic for Z80?

The original version of BBC Basic was written by Sophie Wilson at Acorn in 1981 for the BBC Micro range of computers, and was designed to support the UK Computer Literacy Project. R.T.Russell was involved in the specification of BBC Basic, and wrote his own Z80 version that was subsequently ported to a number of Z80 based machines. [I highly recommend reading his account of this on his website for more details](http://www.bbcbasic.co.uk/bbcbasic/history.html).

As an aside, R.T.Russell still supports BBC Basic, and has ported it for a number of modern platforms, including Android, Windows, and SDL, which are [available from his website here](https://www.bbcbasic.co.uk/index.html).

### Why am I doing this?

### The Challenges

The foundation of this port is R.T.Russell's original BBC Basic for Z80 code (for CP/M), with the CP/M specific code stripped out. This foundation code is just pure BASIC interpreter with no graphics, sound or file I/O support.

### Assembling

### Running

### Releases

I'll dump significant updates as executable bin files in the [releases](releases) folder

### License

This code is distributable under the terms of a zlib license. Read the file [COPYING](COPYING) for more information.

The code, as originally written by R.T. Russell and [downloaded from David Given's GitHub page](https://github.com/davidgiven/cpmish/tree/master/third_party/bbcbasic), has been modified slightly, either for compatibility reasons when assembling using sjasmplus, or for development reasons for this release:

The original files are: [eval.z80](eval.z80), [exec.z80](exec.z80), [fpp.z80](fpp.z80), [patch.z80](patch.z80), [ram.z80](ram.z80) and [sorry.z80](sorry.z80).

The source code is equivalent to the code originally authored by R.T.Russell, downloaded on David Given's website: 

http://cowlark.com/2019-06-14-bbcbasic-opensource/index.html

Any additions or modifications I've made to port this to the Agon have been released under the same licensing terms as the original code, along with any tools, examples or utilities contained within this project. Code that has been copied or inspired by other sources is clearly marked, with the appropriate accreditations.

Dean Belfield

Twitter: [@breakintoprogram](https://twitter.com/BreakIntoProg)
Blog: http://www.breakintoprogram.co.uk
