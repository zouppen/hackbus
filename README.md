# HackBus

[![Matrix chat](https://img.shields.io/badge/chat-via%20matrix-brightgreen.svg)](https://matrix.to/#/#jkl:hacklab.fi)

Transactional memory bridge to Modbus RTU. Expandable to other buses
such as DMX.

Uses Haskell Software Transactional Memory to translate imperative
nature of field buses to declarative definition.

Still work in progress, but used in daily basis at Hacklab Jyväskylä
club room for controlling the alarm system, lighting, and power
sockets.

## Installation

If you have Debian or Ubuntu, I'd recommend installing all available
dependencies from their repositories to minimize package clutter.

Then, only curl-aeson and hackbus itself are installed user-wide.

```
sudo apt install \
	cabal-install \
	libghc-stm-dev \
	libghc-text-dev \
	libghc-aeson-dev \
	libghc-network-dev \
	libghc-readline-dev \
	libghc-curl-dev \
	libghc-utf8-string-dev \
	libghc-monad-loops-dev \
	libmodbus-dev
cabal v1-update
cabal v1-install
```

On other systems with Cabal, run

```
cabal update
cabal install
```

## Interactive usage

There is interactive GHCi shell available for testing and setting
module parameters such as addresses. Tab completion works in both
device file entry and in the shell.

Before it works, the cabal package needs to be installed, of course.

### Interactive Modbus

```
$ cd interactive/modbus
$ ghci
GHCi, version 8.4.4: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Interactive      ( Interactive.hs, interpreted )
Ok, one module loaded.
Device file: /dev/ttyUSB0
Initialized, ModbusHandle in "h"
Loaded GHCi configuration from /home/user/hackbus/interactive/.ghci
*Interactive System.Hardware.Modbus> setSlave h 1 >> writeRegister h 0x40002 2
```

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.
