# Haskell implementation of the Futhark server protocol

[![Hackage](https://img.shields.io/hackage/v/futhark-server.svg?style=flat)](https://hackage.haskell.org/package/futhark-server)[![CI](https://github.com/diku-dk/futhark-server-haskell/workflows/build/badge.svg)](https://github.com/diku-dk/futhark-server-haskell/actions)

This Haskell library provides an implementation of the
[Futhark](https://futhark-lang.org) [server
protocol](https://futhark.readthedocs.io/en/latest/server-protocol.html).
This can be used to interact with Futhark code in a more decoupled
manner than through an FFI.
