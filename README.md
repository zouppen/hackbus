# HackBus

Transactional memory bridge to Modbus RTU. Expandable to other buses such as DMX.

Uses Haskell Software Transactional Memory to translate imperative nature of field buses to declarative definition.

Still work in progress, but used in daily basis at Hacklab Jyväskylä club room for controlling lighting and power sockets.

## Installation

If you have Debian or Ubuntu, I recommend installing all dependencies
from their repositories to minimize package clutter.

```
sudo apt install \
	cabal \
	libghc-stm-dev \
	libghc-text-dev \
	libghc-aeson-dev \
	libghc-network-dev \
	libmodbus-dev 
cabal update
cabal install
```

On other systems with Cabal, run

```
cabal update
cabal install
```
