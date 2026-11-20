# Tasks: Test Daedalus with Quetzal

## Preparation

- [x] T001 Read project instructions, inspect worktrees/branches/spec numbers, and create/verify sibling worktree `002-quetzal-wallet`.
- [x] T002 Bootstrap `.specify/`, define constitution and write `spec.md` plus requirements checklist.
- [x] T003 Inspect backend launch/API boundaries and upstream source; write `research.md` and `plan.md`, recording Sam's TLS assumption.

## US1 — Isolated backend selection

- [x] T004 Pin Quetzal in `flake.nix`/`flake.lock` and add its Nix package in `perSystem/quetzal.nix`.
- [x] T005 Add guarded experimental config in `perSystem/common.nix`, isolate all state and frontend data, adapt genesis filenames, retain TLS/lifecycle contracts and disable dApp policy.
- [x] T006 Add `quetzal-preview`/`quetzal-preprod` shells in `perSystem/devshells/daedalus.nix`.
- [x] T007 Build generated-config contracts for both experiments and reference defaults in `perSystem/quetzal.nix`.

## US2 — Actual compatibility checks

- [x] T008 Build Quetzal with Nix; exercise its real parser with Daedalus's complete launch contract and record exact results in `validation.md`.
- [x] T009 Start real node and Quetzal through watchdog, verify authenticated readiness and existing renderer client network/list/create/address requests; record results in `validation.md`.
- [x] T010 Verify disposable-wallet persistence after restart and orderly shutdown without orphaned processes; record results in `validation.md`.
- [ ] T011 Exercise manual UI sync/create/restore/receive/settings and record passed, failed or unexecuted checks in `validation.md`.

## US3 — Reproducibility and handoff

- [x] T012 Write `quickstart.md` including pin update, startup commands, isolation, API/UI matrix and upstream prerequisites.
- [x] T013 Run applicable formatting, consistency and diff checks; update task statuses and `validation.md` honestly. Do not mark blocked live checks complete.

Dependencies: T004–T006 follow T003; T007/T008 follow T004–T006; T009 follows
successful T008 and upstream prerequisites; T010/T011 follow T009; T012 can be
prepared after T006; T013 follows implementation and attempted validation.

Execution note: the updated candidate `61fb677d711a021c547e29e32648d0e077b9e95f`
passes the real mTLS boundary check, but T008 still exposes unsupported
`--shutdown-handler`. T009–T011 await shutdown compatibility upstream or Sam's
explicit direction to adapt Daedalus lifecycle.

## Updated upstream compatibility candidate

- [x] T014 Pin `f72b38b2558e6d8b8f9f07c2eb345eec833ca4c7`, restore standard launch options and remove the obsolete genesis alias; rebuild config contracts.
- [ ] T015 Recheck mTLS and run the real watchdog/node/backend launch, renderer request functions, stdin shutdown and restart persistence. Preserve pending UI/funded scenarios.
- [x] T016 Correct the existing development-shell `--testnet` path in `perSystem/common.nix` (`nodeConfigFiles` contains `genesis.json`, not `genesis-byron.json`) and assert file existence/network identity in the configuration contract before rerunning T015.

Native run result: T009/T010 pass using the actual renderer HTTP request
functions and real preview node/backend. T015 remains incomplete because the
additional `/network/parameters` check fails on a fresh Alonzo-era node. T011
(graphical UI), funded workflows, hardware and restart performance remain pending.

## Packaged launch requested by Sam

- [x] T017 Extend `perSystem/common.nix` experimental config to local Linux preview/preprod Nix packages with isolated state/user data and libexec Quetzal.
- [x] T018 Refactor `perSystem/linux.nix` NixOS package factory and expose `nixos-package-preview-quetzal` / `nixos-package-preprod-quetzal` with the existing parent-watchdog/production-Electron path.
- [x] T019 Check packaged configs and build the preview Quetzal package; update quickstart and evidence with the direct `nix run` command and wallet-database limitation.

## Shared state explicitly requested by Sam

- [x] T020 Reuse the normal packaged network/frontend state and select `wallets/quetzal.sled/` for Quetzal in common/Linux configuration; amend the spec and constitution.
- [x] T021 Verify packaged/development configuration contracts and test the real Quetzal binary alongside an unchanged SQLite fixture.
- [x] T022 Rebuild `nixos-package-preview-quetzal`, inspect its assembled launcher/config and update launch documentation.

## Upstream conformance update — 2026-11-17

- [x] T023 Update the immutable Quetzal pin and lockfile to upstream master `84f2f4d691df3a32928766733def609164888475` after Sam reported conformance passing; update the quickstart.
- [x] T024 Rebuild Quetzal, the preview package and both networks' configuration contracts; record results without treating upstream conformance as Daedalus UI qualification.
