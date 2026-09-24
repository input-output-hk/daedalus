# Analytics validation and remaining acceptance gates — 2026-09-24

All changes remain local and uncommitted on `feature/ariadne-analytics-upstream`,
based on official upstream `6c57eb94753211f66d3a63f49d031bf746044755`.
The personal fork's master, installed wallets and their data are unchanged.
No wallet, development server, renderer server or telemetry relay was started.
Electron was invoked only as Node for dependency probes. Backend binaries were
invoked only for version/help output. No live endpoint received telemetry.

## Completed automated validation

- Windows portable **Node 24.21.0**, installed privately after verifying the
  official archive checksum; no global Node change. Full Jest: **80 suites,
  1,150 passed tests, three skipped, six passed snapshots**. Focused analytics
  suites cover 18 tests. TypeScript and full ESLint pass (existing repository
  warnings remain). Main development webpack build passes without watch/start.
- Ubuntu WSL, **Nix 2.35.1**, the repository's pinned **Node 24.15.0** and cache:
  actual `checks.x86_64-linux` derivations for compile, Jest, Cucumber unit,
  lint, stylelint, Storybook and i18n all produced valid successful outputs.
  Nix production main/renderer compilation also completed before the packaging
  failure below. All source exports include the new files; nothing was staged.
- Windows and Linux FormatJS traversal produced different duplicate-message
  ordering. The new catalogs were regenerated using the supported Linux
  `yarn i18n:manage`; its artifact consistency gate then passed. Only the already
  changed catalogs were copied back. Locale files retain the generator's byte
  format; no untouched whitelist or source file was reformatted.
- The Nix `nativeModules-x86_64-windows` target built. Pinned Electron **41.3.0**,
  ABI **145**, loaded HID and USB; `node-hid`, `usb` and `js-chain-libs-node`
  require successfully in that runtime. This does not test attached hardware.
- The matching `daedalus-bridge-x86_64-windows-preprod` target built. Binary probes:
  cardano-node **11.1.2**, commit `fef83fed01d7926f3de83b3b917be5a4a48768b5`;
  cardano-wallet **v2026-09-16**, commit `33fcb5e4104db4d0979669a1adc9f5b782acd1c0`.
  The matching watchdog and generated Preprod configuration are present.
- Ariadne's cross-repository harness passes: actual client tracker, consent owner
  and normalizer; mocked transport into the real HTTP handler; disposable DuckDB
  migration/restart; authorized aggregate responses. Nine records include two
  matched attempts, three page views and two custom events; reconsent yields two
  installations. Pending/rejected/revoked states send nothing. No UUID is exposed.
- Ariadne `just format`, then `just check`, pass including all tests, lint,
  TypeScript, formatting and production build/native DuckDB verification. New
  coverage includes strict v2 validation, migration from v1, committed v2 WAL
  recovery, deduplication across restart, expiry/order/window boundaries, consent
  races, ticket authorization/metadata coverage, independent read failures and
  selective fixture cleanup. No configured user database was opened by the tests.

## Required gate and packaged limitations

The literal `nix fmt -- --ci` gate was executed on disposable exact-byte source
exports. Clean upstream flags **1,707 files**; the feature flags **1,688**, all
already flagged by baseline. **No new formatting failures; no touched source
failures.** The Windows checkout and committed source contain line-ending issues;
unrelated files were preserved. `check:all` therefore cannot be reported as a
whole-gate pass. Its downstream equivalent checks above do not waive formatting.

For further native checks only, separate disposable source exports converted
UTF-8 CRLF to LF. This did not reformat or modify untouched checkout files. The
exact-byte Windows installer attempt fails in Yarn's Nix `postFixup` with
`$'\r': command not found`. The LF-only attempt gets through compilation but
Electron packager's `rcedit-x64.exe` uses WSL native execution inside the Nix
sandbox and fails with `WSL ... UtilGetPpid ... Failed to parse: /proc/1/stat`.
The same failure is reproduced on the unchanged upstream LF baseline. A supported
native Linux Nix build host (or an upstream-supported WSL packaging correction)
is still required for the complete installer. No host configuration, sandbox,
dependency version or gate was changed to bypass this failure.

Packaged HTTPS enforcement has automated configuration/transport coverage, but
there is **no successful full installer or interactive packaged acceptance**.
Unpackaged native probes and webpack output do not substitute for that check.
No production destination was configured to test it.

## Prepared but not interactively tested

The private `.git/analytics-completion-review/prepared-profile.json` records a new
empty profile beneath `%LOCALAPPDATA%\DaedalusAriadneReview`, matching dependencies,
unique node pipe, watchdog config and entry script. Electron userData/sessionData,
logs/crashes, backend chain/wallet/TLS and legacy paths are isolated. The profile
has no installed-wallet settings, wallet keys or funds. Telemetry points only to
`http://127.0.0.1:3000/api/analytics/event` with the explicit development exception.

Follow [the exact operator walkthrough](ariadne-analytics-review.md) to start the
applications yourself. Fresh consent, acceptance, accepted restart, rejection,
revocation during delivery, rejected restart and reacceptance with a new identity
are still outstanding interactive checks. Real delegation/hardware and registration
transactions are not authorized test prerequisites; use the synthetic attempts.
Actual wallet success/confirmation and packaged UI tests remain distinct future
acceptance. English and Japanese drafts need human review, and **Sam and Adam's
Matomo replacement/fallback decision remains unresolved**.

Private full patches, file inventories, checksum manifests, logs, helper scripts
and baseline comparisons are under `.git/analytics-completion-review`. They are
review artifacts, not a request to commit. Explicit personal commit approval is
still required; it would not authorize push, PR creation, merge or deployment.
