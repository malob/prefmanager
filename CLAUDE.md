# prefmanager — maintenance notes

A CLI for managing macOS preferences. Single-package Haskell project.
This file is the runbook for routine maintenance — read before touching
build config, bumping GHC, or adding dependencies.

## Toolchain shape

- **Build tool:** Cabal only (no Stack, no hpack).
- **Manifest:** hand-written `prefmanager.cabal`. A `common shared` stanza
  carries the mixins, base/relude deps, and default-language for every
  component.
- **Reproducibility:** `cabal.project` pins the GHC version
  (`with-compiler:`); `cabal.project.freeze` pins every dependency version.
  Both are committed.
- **Nix layer:** `flake.nix` uses [haskell-flake](https://github.com/srid/haskell-flake)
  on top of nixpkgs `haskellPackages`. Each tested GHC is a separate
  `haskellProjects.<name>` entry.
- **Prelude:** Relude is the implicit prelude via Cabal `mixins` (see the
  `common shared` stanza). No `import Relude` needed in source files.
- **Plist fork:** the upstream `plist` package on Hackage doesn't support
  `MonadFail`. We pull from `github:malob/plist#monadfail` in both
  `cabal.project` (via `source-repository-package`) and `flake.nix` (via
  the `plist-source` flake input + `packages.plist.source` override).
  Until upstream merges the fix, both pins must move together.

## Day-to-day commands

```sh
nix build                       # build with default GHC (9.10)
nix build .#ghc912-prefmanager  # build with another matrix entry
nix run . -- domains            # run the executable
nix develop                     # dev shell with GHC, cabal, HLS, hlint
nix flake check                 # build everything declared in the flake
```

Inside `nix develop`:

```sh
cabal build
cabal run prefmanager -- domains
cabal test
```

## Adding a dependency

1. Edit `prefmanager.cabal`. Put it in `common shared` if every component
   uses it; otherwise put it under the relevant component's `build-depends`.
2. Inside `nix develop`, run `cabal freeze` to regenerate
   `cabal.project.freeze`.
3. Commit both files together. CI will rebuild against the new plan.

> [!note]
> If `cabal freeze` runs in an environment without a populated Hackage
> index (sandboxed Nix shells sometimes don't have one), it can rewrite
> `index-state:` from the pinned timestamp to `HEAD`. Spot-check the
> `cabal.project.freeze` diff and restore the timestamp manually if so.

## Bumping GHC

1. Pick the new version (must be available in current `nixpkgs-unstable` —
   check `nix eval nixpkgs#haskell.compiler --apply builtins.attrNames`).
2. In `flake.nix`, change the `mkProject` argument inside
   `haskellProjects.default` (and update other matrix entries if needed).
3. In `cabal.project`, update `with-compiler: ghc-X.Y.Z` to match.
4. Inside `nix develop`, run `cabal freeze` to regenerate the freeze file
   (constraint solver may pick different dep versions for the new GHC).
5. Update `.github/workflows/ci.yml`: the `cabal-build` job's
   `ghc-version`, and the `nix-build` matrix if the default name changed.
6. Run `nix build`, `nix build .#ghc912-prefmanager`, `nix flake check` to
   verify.

## Adding a GHC to the matrix

1. Add `haskellProjects.ghc<NNN> = mkProject "ghc<NNN>" // { autoWire = [ "packages" ]; };`
   to `flake.nix`.
2. Add `ghc<NNN>-prefmanager` to the `attr:` list under
   `nix-build.strategy.matrix` in `.github/workflows/ci.yml`.
3. Verify with `nix build .#ghc<NNN>-prefmanager`.

If the build fails because a transitive dep has a stale upper bound for the
new GHC's `base`/`containers`/etc., the right fix is usually
`settings.<pkg>.jailbreak = true;` on that project in `flake.nix`. Don't
add `allow-newer:` to `cabal.project` — that affects every Cabal user, not
just the matrix entry.

## Updating nixpkgs / flake inputs

```sh
nix flake update                          # bump everything
nix flake update --update-input nixpkgs   # just nixpkgs
```

After updating nixpkgs, also re-run `cabal freeze` if Hackage's
`index-state` should advance — the freeze file embeds the index timestamp.

## Built-in ignore list

The watch command's default ignore filter lives at
`src/Defaults/Filter.hs:builtinIgnores` — a list of pattern strings parsed
through `unsafeRule`. **Malformed patterns crash the program at startup**
(neither the type checker nor hlint catches them); after editing, run
`prefmanager ignore-defaults` to verify the list still loads.

The list is empirically derived against macOS 26.x and is expected to
drift across major macOS releases. Entries that no longer fire are
harmless; new noise sources are added by observation. It's organized
into four sections:

- Cross-app framework patterns (everything matching `*:`)
- Apple system daemons (per-domain, grouped by subsystem)
- Persisted runtime UI state
- Auto-tracked "recent" lists

## Error model

Two project-defined exception types surface user-actionable errors:

- `ConfigError` (`src/Defaults/Filter.hs`) — ignore-config file missing,
  not UTF-8, or parse-failed. Thrown from `buildFilter` / `loadFileRules`.
- `DefaultsError` (`src/Defaults.hs`) — `/usr/bin/defaults` failures:
  spawn `IOException`, non-zero exit (with captured stderr), strict
  UTF-8 decode failure, plist parse failure (with the failing
  `DomainName`). Thrown from `defaultsCmd` / `export`.

Both are caught at the top level by `runCommand` in `app/Main.hs`, which
prints `Error: <displayException>` to stderr and `exitFailure`s. New
user-facing failure modes should extend one of these sums rather than
introduce a new exception type.

## Things to avoid

- **Don't reintroduce `package.yaml` / hpack.** Hand-written cabal is the
  source of truth; hpack adds a generated-file step and historically caused
  bundled-vs-system version skew.
- **Don't reintroduce Stack.** Cabal + freeze covers the same reproducibility
  story for non-Nix users via GHCup. If a Stack-only contributor ever shows
  up, point them at GHCup + `cabal install`.
- **Don't add `allow-newer:` to `cabal.project`.** Use per-project
  `settings.<pkg>.jailbreak` in `flake.nix` instead, scoped to the matrix
  entry that needs it.
- **Don't use `import Relude` in source files.** The mixin handles it.

## Current GHC matrix

- **9.10** (default, `prefmanager`)
- **9.12** (`ghc912-prefmanager`)
- **9.14** (`ghc914-prefmanager`) — needs `settings.relude.check = false;`
  in `flake.nix` because relude-1.2.2.2's doctests fail on the new
  output format. The library itself builds clean.

## Known compatibility limits

- **GHC 9.6 / 9.8** aren't currently in the `nixpkgs-unstable` binary cache
  for darwin (would require building GHC from source). They could be added
  back if the cache regains them, but it's not worth source-building.
