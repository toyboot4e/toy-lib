# ToyLib

ToyLib is a Haskell library for solving AtCoder problems. Library components can be bundled into one `Main.hs` using [./script/toylib](./script/toylib).

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

