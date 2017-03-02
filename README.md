SafeCopy-store
==============

[![Build Status](https://travis-ci.org/acid-state/safecopy.svg?branch=master)](https://travis-ci.org/ncrashed/safecopy)
[![Public Domain](http://b.repl.ca/v1/license-public-blue.png)](https://en.wikipedia.org/wiki/Public_domain_software)
[![Haskell](http://b.repl.ca/v1/language-haskell-4e6272.png)](Http://www.haskell.org)
[![Hackage Status](https://img.shields.io/hackage/v/safecopy-store.svg)][hackage]

[hackage]: https://hackage.haskell.org/package/safecopy-store

**This is a fork** of original [safecopy](https://hackage.haskell.org/package/safecopy) with replacement of [cereal](https://hackage.haskell.org/package/cereal)
with [store](https://hackage.haskell.org/package/store) backend.

SafeCopy extends the parsing and serialization capabilities of
[`Data.Store`](https://hackage.haskell.org/package/store) to include nested
version control.  Nested version control means that you can change the
definition and binary format of a type nested deep within other types without
problems.
