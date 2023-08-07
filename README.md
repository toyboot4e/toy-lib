# ToyLib

ToyLib is my Haskell library for solving AtCoder problems.

[Documentation](https://toyboot4e.github.io/toy-lib/)

## Features (CLI)

`toy-lib-exe` is the source file bunlder. It has the following features.

### Source file bundling

Bundle the whole / part of the ToyLib modules into one `Main.hs` with a specified format.

#### Available inputs

| Input         | Status                       |
|---------------|------------------------------|
| Whole library | :heavy_check_mark: Available |
| One module    | :construction: In progress   |
| stdin         | :construction: In progress   |

#### Available formats

| Format                      | Status                       |
|-----------------------------|------------------------------|
| Minify all the declarations | :heavy_check_mark: Available |
| Minify each declaration     | :construction: In progress   |
| Pick up declarations        | :construction: In progress   |

### Templates

Embed `ToyLib` modules with specified formats.

TODO

## Project setting notes

### `stack.yaml`

Disable Nix pureity for NixOS, where Nix integration seems to be enabled by default:

```yaml
nix:
  # Let `toy-lib-exe` find `stack` in user `PATH`:
  pure: false
```

### `package.yaml`

No exports are assumed, as they're bundled into one file (`Main.hs`):

```yaml
ghc-options:
- -Wno-missing-export-lists
```

We're using `Haskell2010` because `haskell-src-exts` (the parser) does not understand it, but all the extensions enabled by `GHC2021` are enabled as `default-extensisons`:

```yaml
language: Haskell2010

default-extensions:
- BangPatterns
# and lots more
```

### `hacddock`

- Run `stack test` for running [`doctest`] over [`haddock`] doctests.
- Run `stack haddock toy-lib:lib --fast` to build local library documentation. Add `--open` to it for opening it on your browser.
  - TODO: Include it in `stack test` or project-specific command?

[`doctest`]: https://github.com/sol/doctest
[`haddock`]: https://haskell-haddock.readthedocs.io/en/latest/

### GHC version

GHC 9.4.5, [lts-21.6](https://www.stackage.org/lts-21.6).

### Incremental builds?

TODO

### Limitations

We have limitations coming from [haskell-src-exts](https://github.com/haskell-suite/haskell-src-exts):

- Can't use `MultiWayIf`
  `haskell-src-exts` does not understand it well.

- Can't use `GHC2021` syntax
  For example `import X qualified as Y` is not available.
  > But [the default language extensisons enabled by `GHC2021`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2021) is supported and needs not to be declared.

## Thanks

[cojna/iota](https://github.com/cojna/iota)

