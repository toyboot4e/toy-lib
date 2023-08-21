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

We're using `GHC2021` as the default langauge:

```yaml
language: GHC2021
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

## Thanks

[cojna/iota](https://github.com/cojna/iota)

