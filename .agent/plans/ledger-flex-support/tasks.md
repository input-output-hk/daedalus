# Ledger Flex tasks

- [x] T001 Trace pairing, detection, SDK compatibility, IPC errors and renderer recovery.
- [x] T002 Record investigation evidence, implementation plan and remaining hardware gates.
- [x] T003 [US1] Update `package.json` and `yarn.lock` to SDK 8.1.0.
- [x] T004 [US2] Preserve operation/error details in Ledger pairing IPC and main logs.
- [x] T005 [US1] Recognize Flex in shared model types and renderer classification.
- [x] T006 [US2] Recognize v8 user rejection alongside legacy rejection.
- [x] T007 [US1/US2] Test actual SDK v7/v8 key export, serial reads, rejection/retry and serialized diagnostics.
- [x] T008 Run focused Jest, existing Ledger conversion tests, TypeScript and format checks through Nix.
- [ ] T009 [US1] Physically pair Flex, verify address and reconnect; record exact versions.
- [ ] T010 [US1] Physically regression-test Nano X pairing and record exact versions.
- [ ] T011 [US2] Physically check rejection/disconnection recovery; decide whether more UI guidance is needed.

T003–T006 follow T002; T007 covers those changes; T008 follows implementation
and tests. T009–T011 require connected hardware and remain completion gates.

## Validation record (2026-10-21)

Project `nix develop .#mainnet` environment:

- Full Jest suite after SDK/API migration: 88 suites passed; 1,258 tests passed,
  three skipped; six snapshots passed.
- Final focused pairing/SDK and Shelley conversion suites: 20 tests passed,
  including added Flex transport-identifier and numeric rejection checks.
- Final `tsc --noEmit`: passed.
- ESLint on all changed TypeScript files: zero errors, 157 warnings. Existing
  legacy warnings remain; the new diagnostic/rejection modules have no warnings.
- Final `treefmt --ci`: passed, zero changed files.
- `git diff --check`: passed.

Initial tests found package-root status exports unavailable and SDK device
statuses carried in numeric `code`; adapted imports and normalized status fields.
TypeScript found removed `utils.hex_to_buf` calls and an old string-typed version
contract; migrated those. Lint found a nested ternary, replaced with branches.
An incorrectly invoked check emitted JavaScript beside TypeScript sources; only
those newly generated untracked outputs were removed, then TypeScript was run
directly with `--noEmit`. No commits or pushes were made.

At automated validation time, physical checks were unperformed. No claim of
corrected app-7.3.0 behavior, Nano X regression, or on-device signing validation.

Next physical test configuration, confirmed by the user: Secure Element
**1.7.0** and Cardano app **8.0.9**. The user subsequently reported the updated
build works. Address verification, reconnect, Nano X regression and recovery
scenarios have not been individually confirmed, so their task gates remain open.

## Nix package build follow-up (2026-10-21)

The new source modules were initially untracked and therefore excluded from the
Git-backed flake source, despite local Jest/TypeScript succeeding. Staged the
intended additions, without committing, and verified all three new source files
exist in the exact flake source returned by `nix flake metadata`.

`nix build --no-link .#nixos-package-preview` then passed (exit 0), including
the JavaScript derivation and preview package. No wallet was launched. There
are no remaining untracked files. The user subsequently confirmed the build works
and requested a commit with an SDK-update-only message.

Pre-commit revalidation passed in the Nix development shell: treefmt,
`tsc --noEmit`, and all 88 Jest suites (1,261 tests passed, three skipped,
six snapshots passed).

## Rebase validation on `nodejs-deps-update` (`83ccfb868`)

Preserved the new hardware service, IPC envelopes, validated hex helpers, and
dependency updates; carried pairing diagnostics into `HardwareWalletService`.
Kept archived SDK 8.0.0 evidence unchanged and bound installed SDK 8.1.0 source
to its current lock and SHA-1/SRI-verified tarball in the provenance test.

Regenerated ignored SCSS declarations for the newer base. The first full test
run detected stale nested Trezor packages from the previous dependency tree.
With user approval, rebuilt this worktree's generated `node_modules` using the
project's Nix installation pattern, patches, and native-module rebuild.

Final checks in the Nix development shell:

- TypeScript (`tsc --noEmit`): passed.
- Full Jest: 208 suites passed, 2,566 tests passed, three skipped; six snapshots
  passed. Includes both Ledger and Trezor live provenance checks.
- Changed-file ESLint: zero errors, 143 legacy warnings.
- `treefmt --ci`: passed, zero changed files.
- `nixos-package-preview` derivation evaluation: passed. The full package build
  and physical test recorded above preceded this rebase and were not repeated.
