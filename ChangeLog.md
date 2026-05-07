# Changelog for prefmanager

## 0.4.0.0

- **Replaced the plist parser.** `src/Defaults/Plist.hs` is a new
  hand-rolled parser on top of `xml-conduit`. The previous stack
  (the `plist` package + the `Text.XML.HXT` toolkit) is gone, along
  with the `malob/plist#monadfail` fork pin in `cabal.project` and
  the corresponding `plist-source` flake input.

  Headline reason: the upstream `plist` parser silently dropped
  `<key>/<data>` pairs whose `<data>` element contained any
  whitespace — exactly what `defaults export` always emits. As a
  result, every binary preference value (security tokens, recent-
  files bookmarks, app state, NSStatusItem data, ...) was invisible
  to prefmanager. Owning the parser fixes this; binary keys now show
  up in diffs, `keys` listings, and `watch` output.

- **New project-local `PrefValue` ADT** in `Defaults.Types` replaces
  the upstream `PlObject`. Notably:
  - `PvDict` is `[(Text, PrefValue)]` to preserve source order
    faithfully when rendering nested dict values; equality is
    overridden to be order-insensitive so diffs treat dicts as
    semantic dictionaries. Duplicate keys are rejected at parse time.
    `Domain` (the per-domain top-level container) stays
    `Map Text PrefValue` since alphabetical key listings are more
    useful than source order for `prefmanager keys`, `EventAdded`,
    and `EventRemoved` rendering.
  - `PvData` carries the decoded `ByteString` so renders can show a
    real fingerprint; the previous `[binary data]` placeholder hid
    real changes between two different binary values.
  - We deliberately do not derive `Ord` (`Double`'s `Ord` is partial
    via NaN, and we don't compare values ordinally anywhere).

- **`Key = String` → `Key = Text`.** Long-standing accidental from
  having to match the upstream `plist` API. With the new parser we
  control the type and drop a layer of `toText`/`toString`
  round-trips throughout `Filter` and `Pretty`.

- **`<data>` rendering** now shows length and a deterministic base64
  fingerprint of the bytes. For small blobs (≤ 16 base64 chars) the
  whole encoded form is shown; longer blobs sample the first and
  last 4 base64 chars (`<data>[256 bytes, base64:AbCd…WxYz]</data>`)
  so changes anywhere in the blob remain visible — a prefix-only
  fingerprint would let two same-length blobs that share their
  first six bytes render identically.

- **Stricter parser.** Non-whitespace text mixed in among collection
  children (e.g. `<dict>junk<key>k</key>...</dict>`) now errors
  loudly rather than being silently dropped. Strings preserve their
  newlines through rendering (the renderer uses `hardline` rather
  than the flattenable `line`). Base64 whitespace inside `<data>` is
  recognized via a strict ASCII-only predicate (space/tab/CR/LF) so
  exotic whitespace like NBSP fails decoding rather than being
  silently stripped.

- **`PlistError` rendered for users**, not via derived `Show`. CLI
  error messages now read like `couldn't parse "abc" as an integer`
  rather than `PlistBadInteger "abc"`.

- **New test suite.** `cabal test` runs `prefmanager-test` (`tasty` +
  `tasty-hunit`, 35 tests). Coverage includes scalars (with very-
  large integers), nested collections, the `<data>` whitespace bug
  as a permanent regression guard, malformed input (non-whitespace
  text between collection children, missing values, bad
  base64/numbers), dict semantics (duplicate-key detection,
  source-order independence under equality, source-order
  preservation in the underlying list), and whitespace discipline
  (around scalar payloads, inside `<data>`, between collection
  children).

- **Dependencies:** dropped `hxt` (and the transitive
  `hxt-charproperties` / `hxt-regex-xmlschema` / `hxt-unicode`
  closure) and `plist`. Added `xml-conduit`, `base64-bytestring`, and
  test-only `tasty` + `tasty-hunit`.

- GHC 9.14 (`ghc914-prefmanager`) needs `settings.blaze-markup.jailbreak`
  and `settings.blaze-html.jailbreak` (transitive deps of
  `xml-conduit` with `containers < 0.8` upper bounds).

## 0.2.0.0

- Added an ignore filter to `watch` that suppresses macOS housekeeping keys
  which change constantly without representing real preferences. The filter
  ships with ~30 built-in patterns derived from real macOS observation; opt
  out per-rule with `--no-builtin-ignores`, extend with `--ignore` (repeatable)
  or `--ignore-file PATH`. Default config location is
  `~/.config/prefmanager/ignore.conf`.
- New `--plain` flag for `watch`: timestamps each change event and drops the
  ANSI status line, suitable for piping to a log file during long sessions.
- New `ignore-defaults` subcommand prints the built-in patterns and the
  default config path.
- `watch` now isolates per-domain export failures: a single failing domain
  logs a warning to stderr and carries forward the previous snapshot rather
  than aborting the whole watcher.
- `--ignore` rules are validated at option-parse time; config-file errors
  surface with line numbers and accumulate (every error in one pass).
- `defaultsCmd` now uses `proc` with argv instead of shell interpolation,
  removing a small command-injection surface for exotic domain names.

## 0.3.0.0

- Added GHC 9.14 to the build matrix (`ghc914-prefmanager`), now that
  `patience-map`'s `containers < 0.8` upper bound is no longer in the
  way. relude-1.2.2.2's doctests are skipped under 9.14 due to output-
  format drift; the library itself compiles clean.
- Dropped the `patience-map` (`patience`) dependency. `diffDomain` now
  uses `Data.Map.Merge.Strict.merge` directly with a local 3-constructor
  `Delta` ADT (`Old | New | Delta`) — no `Same` because the renderer
  never used it. Behavior is unchanged.

- `watch` auto-switches to `--plain` when stdout is not a TTY (e.g.
  redirected to a file). Avoids leaking ANSI escape sequences into
  logs without needing the user to remember the flag.
- `watch` exits cleanly on broken pipes — `prefmanager watch | head`
  no longer prints a Haskell traceback on pipe close.
- `--plain` timestamps now use local time (via `getZonedTime`) rather
  than UTC, matching the user's wall clock when correlating diffs
  with System Settings actions.
- `watch` exits with an error when the ignore filter leaves no
  domains to watch (instead of polling silently forever).
- `watch --all` now re-lists domains each poll and renders genuinely
  new domains (those that didn't exist when the watcher started) as
  `<domain> (Domain added, N keys)` followed by the key list.
  Previously these were invisible because the domain set was captured
  once at startup. If `defaults domains` itself fails mid-watch, the
  watcher warns once and carries forward the previous set rather than
  crashing.
- Per-domain export warnings are now rate-limited: a permanently-
  failing domain produces one warning on the transition into the
  failing state, then nothing while it stays failing. Recovery is
  silent — the next actual diff is the user-visible signal.
- The watch loop now tracks five per-domain states (snapshotted, lost
  contact, never contacted, newly appeared, vanished) so post-startup-
  new domains render as added even when their first export failed,
  baseline-failure recoveries stay quiet, and a domain that disappears
  from the list renders a one-line `(Domain removed, N keys)`
  acknowledgement (no key list) before being kept in 'vanished' state
  for potential return.

- New `--interval SECS` flag for `watch` controls the polling rate
  (default 1s, fractional values allowed, `0` preserves the previous
  spin-as-fast-as-possible behavior). Previously the watch loop polled
  with no delay between iterations, which on `--all` could spawn
  hundreds of `defaults` subprocesses per second.
- Watch loop now catches synchronous exceptions only when applying its
  per-domain failure tolerance. Async exceptions (`UserInterrupt`,
  `ThreadKilled`, etc.) propagate cleanly, so Ctrl-C reliably stops
  the watcher. Previously a `try @SomeException` could swallow
  `ThreadKilled` and turn it into a per-domain warning while the loop
  kept running.
- New `DefaultsError` type wraps failures from `/usr/bin/defaults`
  (spawn failure, process exit, UTF-8 decode, plist parse) with structured context
  (argv, exit code, captured stderr, domain name). One-shot commands
  (`domains`, `keys`, `ignore-defaults`) print a clean `Error: ...`
  message and exit non-zero on `DefaultsError`; the watch loop logs
  the failing domain to stderr and carries forward the previous
  snapshot.
- `defaultsCmd` now captures stderr (rather than letting it leak to
  the parent terminal and corrupt the ANSI display) and decodes stdout
  strictly as UTF-8. Non-UTF-8 output surfaces as `DefaultsError`
  rather than silently producing replacement characters.
- Five additional built-in ignore patterns from observed real-world
  noise: Sparkle auto-update timestamps (`*:SULastCheck*`), IDS
  state-machine transitions, Background Sounds playback timer,
  Spotlight bundle index, and Control Center transient module
  visibility.
- Reorganized the built-in ignore list into four sections (cross-app
  framework patterns, Apple system daemons by subsystem, persisted
  runtime UI state, auto-tracked recent lists). The list had grown
  chronologically with each round of observation; merging duplicate
  groupings (DuetKit, Spotlight) and lifting the universal `*:`
  patterns to the top makes scanning for "is X covered?" feasible.
  No rules added or removed by the reorganization itself.
