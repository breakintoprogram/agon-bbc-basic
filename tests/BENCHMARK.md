# benchmarks

Tested using the [Rugg/Feldman (PCW) benchmarks](https://en.wikipedia.org/wiki/Rugg/Feldman_benchmarks#:~:text=This%20expanded%20set%20became%20known,many%20computer%20magazines%20and%20journals.)

NB:

- The BBC Micro CPU is a 6502 running BBC Basic IV
- The Spectrum is a Z80 running Sinclair BASIC
- The Cerberus is a Z80 running BBC Basic for Z80

| # | Cerberus (8Mhz)| Spectrum (3.5Mhz) | BBC Micro (2Mhz) |
|---|---------------:|------------------:|-----------------:|
| 1 |          0.42s |              4.4s |             0.8s |
| 2 |          1.82s |              8.2s |             3.1s |
| 3 |          5.78s |             20.0s |             8.1s |
| 4 |          6.40s |             19.2s |             8.7s |
| 5 |          6.84s |             23.1s |             9.0s |
| 6 |          9.84s |             53.4s |            13.9s |
| 7 |         14.16s |             77.6s |            21.1s |
| 8 |         16.84s |            239.1s |            49.9s |
