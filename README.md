# mane
[![Haskell CI](https://github.com/standardsemiconductor/mane/actions/workflows/haskell.yml/badge.svg)](https://github.com/standardsemiconductor/mane/actions/workflows/haskell.yml)

## Library
See [`src/Mane.hs`](https://github.com/standardsemiconductor/mane/blob/master/src/Mane.hs)

## Executable

```
mane - VELDT programmer - Standard Semiconductor

Usage: mane [(-f|--find) | (-j|--jedec) | (-r|--reset) | FILE]
  program bitstream

Available options:
  -f,--find                Find FPGA device, print description if found.
  -j,--jedec               Read JEDEC ID
  -r,--reset               Toggle CRESETB
  -h,--help                Show this help text 
```

### Example executable usage: 
Program Blinker bitstream onto the VELDT...
```console
foo@bar:~/mane$ cabal run mane -- example/Blinker.bin
```