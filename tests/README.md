# tests

A handful of BBC BASIC programs I use for regression and soak testing:

- `cube.bbc`: Tests the graphics and floating point maths
- `gpio_port_1.bbc`: Writes out data to an eZ80 GPIO port
- `mandlebrot_1.bbc`: A Mandlebrot set renderer
- `sound_1.bbc`: Tests the SOUND command, should play Amazing Grace
- `sprites_1.bbc`: Demonstrates sprites in BBC BASIC
- `sprites_2.bbc`: As sprite_1.bbc, but using PEEK and POKE rather than DIM to store the data
- `sprites_3.bbc`: As sprite_2.bbc, but using machine code to move the sprites
- `sprites_4.bbc`: As sprite_3.bbc, but loading a sprite animation from a file
- `udg.bbc` Test VDU 23

And there are also a set of Rugg/Feldman (PCW) benchmark programs; benchm1.bbc to benchm8.bbc