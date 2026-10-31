# Tasks

- [x] T001 Specify recovery behavior and inspect watchdog/renderer boundaries.
- [x] T002 [US1] Retain verified staging and implement replay recovery in watchdog/src/mithril.rs and protocol.rs.
- [x] T003 [US2] Honor explicit node continuation in watchdog/src/supervisor.rs.
- [x] T004 [US1] Add typed command, store action, localized error action and explanation.
- [x] T005 Validate recovery with Rust integration tests and renderer tests; run formatting and TypeScript checks.

## Validation evidence

- Rust: 363 watchdog unit tests passed; 19 Mithril regression integration tests passed. After the final chain-status change, all 6 conversion-recovery integration tests passed, including the additional retry/cancel case.
- Jest: 244 tests passed across MithrilSyncContainer, BackendStore and WatchdogManager.
- TypeScript: `tsc --noEmit` passed.
- ESLint: no errors; existing warnings in touched files.
- Rust and TypeScript formatting completed; `git diff --check` passed.
- Translation extraction could not run because the installed FormatJS native binding is missing. With explicit user authorization, the two descriptors were added directly to both generated catalogs. A TypeScript AST-based check confirmed exact agreement with source IDs, default text and descriptions, their unique membership in both catalogs, English text agreement, and Japanese translation presence. No empty extraction output was written.
- Pre-commit checks: project-devshell treefmt passed with zero changes; TypeScript and Clippy (`--all-targets -- --deny warnings`) passed. Full Jest passed 217 suites (2,888 tests, 3 skipped) and failed 3 tests in the untouched hardware capability suite. A project-devshell rerun confirmed missing Ledger/Trezor `.yarn-metadata.json` and `.yarn-tarball.tgz` cache files, with the other 13 hardware tests passing.
