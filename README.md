# macOS Preferences Manager

A CLI utility for working with macOS preferences. Lists domains, lists keys
in a domain, and watches domains for changes — useful for figuring out which
`defaults` key a System Settings toggle actually flips.

## Install

### With Nix (flakes)

```sh
# One-shot
nix run github:malob/prefmanager -- domains

# Persistent install
nix profile install github:malob/prefmanager
```

### Without Nix

Install [GHCup](https://www.haskell.org/ghcup/), then:

```sh
git clone https://github.com/malob/prefmanager
cd prefmanager
cabal install
```

The bundled `cabal.project.freeze` pins the exact dependency versions used in
CI, so you'll build with the same plan that's tested.

## Usage

```
> prefmanager --help
macOS Preferences Manager - a utility for working with macOS preferences.

Usage: prefmanager COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  watch                    Watch domain(s) for changes
  domains                  List all domains
  keys                     List the current keys in a domain
```

Watch:

```
> prefmanager watch --help
Usage: prefmanager watch (DOMAIN... | (-a|--all))

  Watch domain(s) for changes

Available options:
  DOMAIN...                Domain(s) that will be watched
  -a,--all                 Watch all domains including NSGlobalDomain
  -h,--help                Show this help text
```

![Watch command example](./screenshot.png)

## Development

For day-to-day development:

```sh
nix develop          # drops you into a shell with GHC, cabal, HLS
cabal build
cabal run prefmanager -- domains
```

Maintenance procedures (bumping GHC, adding deps, refreshing the freeze file,
extending the GHC matrix) are documented in [CLAUDE.md](./CLAUDE.md).
