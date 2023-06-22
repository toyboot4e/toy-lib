# ToyLib

ToyLib is a private Haskell library for solving AtCoder problems.

## Source file bundling

AtCoder only accepts a single `Main.hs`. The library modules can be bundled into one `Main.hs` using `toy-lib-exe` (`stack run`).

### Available inputs

| Input           | Status                       |
|-----------------|------------------------------|
| Whole library   | :heavy_check_mark: Available |
| One module      | :construction: In progress   |
| One source file | :construction: In progress   |

### Available formats

| Format                              | Status                       |
|-------------------------------------|------------------------------|
| minify declarations                 | :heavy_check_mark: Available |
| format each declaration in one line | :construction: In progress   |
| just pick up declarations           | :construction: In progress   |

## Project setting notes

### `stack.yaml`

For NixOS where Nix integration seems to be on by default:

```yaml
nix:
  # Let `toy-lib-exe` find `stack` in user `PATH`:
  pure: false
```

### `package.yaml`

No exports are assumed, as they're bundled into one file (`Main.hs`):

```hs
ghc-options:
- -Wno-missing-export-lists
```

### GHC version

FIXME: It's not compatible with AtCoder GHC version!

## Thanks

- [cojna/iota](https://github.com/cojna/iota)

