# ToyLib

ToyLib is my Haskell library for solving AtCoder problems.

[Documentation](https://toyboot4e.github.io/toy-lib/)

## CLI

`toy-lib-exe` is the source file bunlder.

Generate a template that bundles all the source files:

```sh
$ cabal run toy-lib-exe > template.hs
```

Minify the toy-lib modules and their dependencies into one line:

```sh
$ cabal run toy-lib-exe -e Data.SparseGraph Data.UnionFind.Mutable
```

Embed toy-lib modules:

```sh
$ cat Example.hs
-- {{{ toy-lib import
import Math.Manhattan
-- }}} toy-lib import

$ cabal run toy-lib-exe -e Examle.hs
rot45 :: (Int, Int) -> (Int, Int);rot45 (!x, !y) = (x - y, x + y)
```
## Project setting notes

### Installation

- GHC 9.4.5 (corresponds to [lts-21.7](https://www.stackage.org/lts-21.7))  
  Source: [AtCoder judge environment 2023](https://img.atcoder.jp/file/language-update/language-list.html), [the spreadsheet](https://docs.google.com/spreadsheets/d/1HXyOXt5bKwhKWXruzUvfMFHQtBxfZQ0047W7VVObnXI/edit#gid=408033513&range=F38)
- HLS 2.2.0  
  See [GHC version support](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)

### `toy-lib.cabal`

No exports are assumed, as they're bundled into one file (`Main.hs`):

```yaml
ghc-options:
- -Wno-missing-export-lists
```

We're using `Haskell2010` because `haskell-src-exts` (the parser) does not understand `GHC2021`, but all the extensions enabled by it are enabled as `default-extensisons`:

```yaml
default-language: Haskell2010

default-extensions:
- BangPatterns
# and lots more
```

### `doctest`

Run [`doctest`] via REPL:

```sh
$ cabal repl --with-ghc=doctest --repl-options='-w -Wdefault'
```

[`doctest`]: https://github.com/sol/doctest

### `haddock`

Run [`haddock`] from command line:

```sh
$ cabal haddock
$ cabal haddock --open
```

### Limitations

We have limitations coming from [haskell-src-exts](https://github.com/haskell-suite/haskell-src-exts):

- Can't use `MultiWayIf`  
  `haskell-src-exts` does not understand it well.

- Can't use `GHC2021` syntax  
  For example `import X qualified as Y` is not available.

## Thanks

[cojna/iota](https://github.com/cojna/iota)

[`haddock`]: https://haskell-haddock.readthedocs.io/en/latest/index.html

