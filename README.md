# ToyLib

ToyLib is my Haskell library for solving AtCoder problems. Note that most of the contents has been ported into the `Extra` module of [`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs).

[API documentation](https://toyboot4e.github.io/toy-lib/)

## CLI

`toy-lib-exe` is the source file bunlder.

### Usage

Generate a template that bundles all the source files:

```sh
$ cabal run toy-lib-exe > template.hs
```

Minify the toy-lib modules and their dependencies into one line:

```sh
$ cabal run toy-lib-exe -m Data.Graph.Sparse Data.UnionFind.Mutable
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

As of 2025, AtCoder is using GHC 9.8.4 (Source: [使用できる言語とライブラリの一覧](https://img.atcoder.jp/file/language-update/2025-10/language-list.html)). Install GHC 9.8.4 and the corresponding tools with `ghcup`.

If you're a Nix Flakes user, run `direnv allow` to enable [`nix-direnv`]([https://github.com/nix-community/nix-direnv][=nix-direnv=]).

### `toy-lib.cabal`

No explicit exports are assumed, as they're bundled into one file (`Main.hs`):

```yaml
ghc-options:
- -Wno-missing-export-lists
```

We're using `Haskell2010` because `haskell-src-exts` (the parser) does not understand `GHC2021`, but all the extensions enabled by `GHC20210` are enabled via `default-extensisons`:

```yaml
default-language: Haskell2010

default-extensions:
- BangPatterns
# and lots more
```

### Limitations

We have limitations coming from [haskell-src-exts](https://github.com/haskell-suite/haskell-src-exts):

- Can't use `MultiWayIf`  
  `haskell-src-exts` does not understand it well.

- Can't use `GHC2021` syntax  
  For example `import X qualified as Y` is not available.

## Useful commands

See the [`Justfile`](./Justfile) for details. Otherwise, you could run the following commands manually.

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

## Similar repositories

- [cojna/iota](https://github.com/cojna/iota)
  iota is surely the best resource!
- [meooow25/haccepted](https://github.com/meooow25/haccepted)
  Beautiful library compatible with Codeforces.
- [minoki/my-atcoder-solutions](https://github.com/minoki/my-atcoder-solutions)
  Solutions to EDPC and more.
- [unnohideyuki/AtHaskell](https://github.com/unnohideyuki/AtHaskell)
- [mizunashi-mana/haskell-atcoder-template](https://github.com/mizunashi-mana/haskell-atcoder-template)

[`haddock`]: https://haskell-haddock.readthedocs.io/en/latest/index.html

