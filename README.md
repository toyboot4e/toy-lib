# ToyLib

ToyLib is my Haskell library for solving AtCoder problems.

- Most of the library was orted into the `Extra` module of [`ac-library-hs`](https://github.com/toyboot4e/ac-library-hs).
- The bundler was replaced with [`bundler-hs`](https://github.com/toyboot4e/bundler-hs).

[API documentation](https://toyboot4e.github.io/toy-lib/)

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
- [cjp240/cjp-lib](https://github.com/cjp240/cjp-lib)
- [meooow25/haccepted](https://github.com/meooow25/haccepted)
  Beautiful library compatible with Codeforces.
- [minoki/my-atcoder-solutions](https://github.com/minoki/my-atcoder-solutions)
  Solutions to EDPC and more.
- [unnohideyuki/AtHaskell](https://github.com/unnohideyuki/AtHaskell)
- [mizunashi-mana/haskell-atcoder-template](https://github.com/mizunashi-mana/haskell-atcoder-template)

[`haddock`]: https://haskell-haddock.readthedocs.io/en/latest/index.html

