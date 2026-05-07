# Changelog for prefmanager

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

## Unreleased changes

- Dropped the `patience-map` (`patience`) dependency. `diffDomain` now
  uses `Data.Map.Merge.Strict.merge` directly with a local 3-constructor
  `Delta` ADT (`Old | New | Delta`) — no `Same` because the renderer
  never used it. Behavior is unchanged. Side effect: GHC 9.14
  (`containers >= 0.8`) is no longer blocked by `patience-0.3`'s
  upper bound.

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
