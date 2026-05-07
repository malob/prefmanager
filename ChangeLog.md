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
