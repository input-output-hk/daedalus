Implementation: Iteration 1
Timestamp: 2026-05-19T16:17:14Z

Changes made:
- Updated `.agent/plans/mithril-partial-sync/task-plans/task-001.md` to mark planning approved and record the current build outcome.
- Added `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` with the repo-verified spike findings, current environment limitation, and the exact operator-run validation procedure and evidence checklist needed to finish the task truthfully.
- Verified additional pinned Mithril source details directly from the locked source tree, including current `cardano-db download` flags and the ancillary conversion hints emitted after successful restore.

Files touched:
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`

Verification run:
- Verified Daedalus packaging/pinning assumptions from live repo files (`flake.nix`, `flake.lock`, `nix/internal/common.nix`, `nix/internal/cardano-bridge.nix`, `nix/internal/any-darwin.nix`, `nix/internal/x86_64-windows.nix`).
- Verified Daedalus runtime assumptions from `source/main/mithril/MithrilBootstrapService.ts`, `source/main/mithril/mithrilCommandRunner.ts`, and `source/main/mithril/mithrilNetworkConfig.ts`.
- Resolved the locked Mithril source path with `nix eval --json .#inputs.mithril`.
- Verified locked Mithril CLI command definitions from the pinned source tree under `/nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source`.
- Attempted to realize the pinned Linux `mithril-client-cli` package with `nix build --no-link /nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source#packages.x86_64-linux.mithril-client-cli`, but the build exceeded the available execution window because this environment cannot use the project\s trusted cache and fell back to a large local build.

Deviations from the approved plan:
- No deviation in scope. The task remained a validation spike.
- Switched to the planned `manual_execution` path once it became clear that full pinned-artifact execution was not truthfully completable within the current environment constraints.

User interaction is now required:
- Yes. The task requires operator-run execution to capture actual binary identity, live command help output, partial restore filesystem results, allow-override behavior, and the mandatory cancellation evidence.

User Handoff:
- Why user interaction is required now:
- The task cannot be completed truthfully without executing the actual pinned or bundled `mithril-client` artifact and capturing the missing live evidence, especially cancellation behavior.
- This environment can inspect the repo and locked Mithril source, but it could not finish realizing the pinned binary within the available execution window.
- Exact manual steps:
1. Review `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
2. Realize or use the actual Daedalus-pinned/bundled `mithril-client` artifact.
3. Capture artifact identity using:
- `readlink -f <path-to-mithril-client>`
- `<path-to-mithril-client> --version`
- `<path-to-mithril-client> cardano-db download --help`
4. Run the four validation scenarios documented in `02-validation-spike-results.md`:
- partial restore without ancillary
- partial restore with ancillary
- allow-override into a populated target
- cancellation during active download
5. Capture commands, exit codes, stdout/stderr excerpts, and resulting directory trees for each case.
6. Return the evidence plus conclusions on safe/unsafe target assumptions, conversion requirement, and cancellation fallout.
- Expected results:
- Live artifact identity is proven.
- Actual partial restore syntax is confirmed from the executed binary.
- Directory layout and override behavior are evidenced.
- Cancellation behavior is evidenced and classified as reusable, cleanup-required, or unsafe.
- What output or decision the user should return:
- The command outputs and directory-tree evidence requested in `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
- Whether work is blocked or can continue in parallel:
- Blocked for truthful completion of `task-001`. Later restore-strategy tasks should not proceed until this evidence is returned.

Outcome: Manual validation handoff issued; task remains in progress pending operator evidence

Implementation: Iteration 2
Timestamp: 2026-05-19T16:32:45Z

Changes made:
- Updated `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` with executed-artifact evidence from a live `mithril-client 0.13.9` run.
- Updated `.agent/plans/mithril-partial-sync/task-plans/task-001.md` to record the completed build outcome and the validated spike conclusions.
- Completed all four required validation scenarios: no ancillary, ancillary, override, and cancellation.

Files touched:
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`

Verification run:
- Captured live artifact identity from `/home/westbam/.cargo/bin/mithril-client` with version `0.13.9`.
- Captured live help output proving the executable accepts `cardano-db download` with `--download-dir`, `--include-ancillary`, `--start`, `--end`, and `--allow-override`.
- Queried the preview aggregator for latest snapshot metadata and used snapshot digest `ce163ce52fefe202cd51683da18bca062f2ad96b01443ebca46316691f80cc42` at immutable file `26049`.
- Ran no-ancillary partial restore for range `26048..26049` into `/tmp/opencode/mithril-task-001/no-ancillary` and verified resulting layout `clean/`, `immutable/`, `protocolMagicId`.
- Ran ancillary partial restore for range `26048..26049` into `/tmp/opencode/mithril-task-001/with-ancillary` and verified resulting layout `clean/`, `immutable/`, `ledger/`, `protocolMagicId` plus emitted converter commands for `LSM`, `LMDB`, and `Legacy`.
- Ran `--allow-override` successfully against both previously populated scratch targets.
- Interrupted an ancillary partial restore during step 3 with `timeout -s INT 28s ...` and captured `EXIT_CODE=124`; post-cancel layout contained both a partially materialized `db/` and a separate ancillary temp directory, demonstrating cleanup is required.

Deviations from the approved plan:
- No scope deviation. The work stayed within the validation spike.
- The executed artifact came from `/home/westbam/.cargo/bin/mithril-client` rather than a realized Nix output, but the live version matched the pinned CLI version `0.13.9` and the note records that distinction explicitly.

User interaction is now required:
- No. The previously required operator evidence has now been captured.

Outcome: Live validation evidence captured; task ready for code review

Code Review: Iteration 2
Timestamp: 2026-05-19T16:34:48Z

Summary:
- Requires changes. The spike still overstates completion against the shipped artifact family. The live validation runs were executed with `/home/westbam/.cargo/bin/mithril-client` and only matched the pinned CLI by semver `0.13.9`; they were not proven to be the flake-pinned or bundled Daedalus artifact. That means the current docs do not yet satisfy the task's own bar of validating behavior against the exact shipped or pinned artifact family before unblocking `task-002`. References: `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:5-7`, `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:65-76`, `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:233-243`, `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:266-278`, `.agent/plans/mithril-partial-sync/task-plans/task-001.md:86-88`, `.agent/plans/mithril-partial-sync/task-plans/task-001.md:109-116`, `.agent/plans/mithril-partial-sync/task-plans/task-001.md:169-177`, `.agent/plans/mithril-partial-sync/task-plans/task-001-impl-review.md:69-80`, `.agent/plans/mithril-partial-sync/task-plans/task-001-impl-review.md:85`.

Blocking findings:
- The completion state is not yet justified because the executed binary provenance is insufficient. The task note and spike results both frame the work as validated against the exact Daedalus-pinned/bundled Mithril family, but the only live execution evidence comes from a Cargo-installed binary outside the pinned Nix output or shipped bundle. Matching version text is not enough to rule out drift from the flake-pinned revision or build configuration, so `task-001` should not currently be marked complete or ready to unblock `task-002`.

Non-blocking observations:
- The repo-verification portion is solid: the Nix packaging, runtime command path resolution, and supported-network assumptions all match the live files I checked.
- The override scenario in the spike note would be more durable if the documented command snippets were self-contained; the `--allow-override` examples omit the env/key setup shown in the earlier runs, which weakens reproducibility of the evidence note.

Approval bar:
- Prove the live runs used the actual flake-pinned or shipped artifact: realize the pinned `mithril-client-cli` output or extract the bundled Daedalus binary, then capture `realpath`, `--version`, and `cardano-db download --help` from that artifact and rerun the validation matrix against it.
- If that cannot be done in this environment, move the task back to an in-progress/manual-handoff state and explicitly separate "pinned source inspection" from "live behavior observed on non-pinned binary".

Decision: requires_changes

Implementation: Iteration 3
Timestamp: 2026-05-19T16:43:36Z

Changes made:
- Addressed the review blocker by realizing and executing the actual flake-pinned Mithril artifact from `/nix/store/2cvcbs7dka54adqf6j6dc7jcrq3mw2lp-mithril-client-x86_64-unknown-linux-musl-0.13.9/bin/mithril-client`.
- Updated `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` to distinguish the earlier exploratory Cargo-binary run from the final provenance-confirmed pinned-artifact runs.
- Updated `.agent/plans/mithril-partial-sync/task-plans/task-001.md` to record the provenance-confirmed artifact and the additional live finding about latest-snapshot drift.

Files touched:
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`

Verification run:
- Built the flake-pinned Mithril CLI with `nix build --no-link /nix/store/p6iq4whk2j8ipy7x4a22njsk2bngcwr4-source#packages.x86_64-linux.mithril-client-cli` and confirmed the realized path with `nix path-info`.
- Captured provenance from the realized binary: real path, `--version`, and `cardano-db download --help`.
- Re-ran no-ancillary partial restore against the pinned artifact and confirmed the same successful behavior.
- Re-ran ancillary partial restore against the pinned artifact and confirmed converter-command emission from the pinned binary.
- Re-ran override and cancellation scenarios against the pinned artifact.
- Observed live latest-snapshot drift during pinned reruns: a first ancillary rerun failed after the latest snapshot advanced from immutable `26049` to `26050`, then the rerun succeeded once the range was refreshed to `26049..26050`.

Deviations from the approved plan:
- No scope deviation. This iteration only closed the reviewer\s artifact-provenance blocker and recorded the additional live drift finding.

User interaction is now required:
- No.

Outcome: Review blocker addressed with provenance-confirmed pinned-artifact evidence; task ready for code review

Code Review: Iteration 3
Timestamp: 2026-05-19T16:44:48Z

Summary:
- Approved. The previous blocker is closed: the validation note now anchors the live runs to a realized flake-pinned Mithril artifact under `/nix/store`, not just a Cargo-installed binary, and the updated task note and implementation review accurately reflect that provenance. The core claims also remain consistent with the live repo packaging/runtime files I re-checked. References: `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:65-79`, `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:114-246`, `.agent/plans/mithril-partial-sync/task-plans/task-001.md:107-116`, `.agent/plans/mithril-partial-sync/task-plans/task-001.md:165-178`, `.agent/plans/mithril-partial-sync/task-plans/task-001-impl-review.md:106-132`, `flake.lock:176-199`, `nix/internal/common.nix:54-59`, `nix/internal/cardano-bridge.nix:40-47`, `nix/internal/any-darwin.nix:241-242,377-378`, `nix/internal/x86_64-windows.nix:630-631`, `source/main/mithril/mithrilCommandRunner.ts:208-216`, `source/main/mithril/mithrilNetworkConfig.ts:13-38`.

Blocking findings:
- None.

Non-blocking observations:
- The spike note\s cancellation section is slightly less direct than the other scenarios because it references the earlier matching cancellation layout alongside the pinned rerun, instead of fully restating the pinned post-cancel tree. It is still sufficient for `task-001`, but tightening that evidence would make the note more self-contained.
- The new latest-snapshot drift finding is useful and appropriately scoped: it does not overclaim safety, but it should be carried forward explicitly into `task-002` and `task-003` as a design constraint for atomic range derivation.

Approval bar:
- Current iteration meets the review bar for `task-001`.
- Next work should treat staged restore, conversion-required ancillary restores, cleanup-required cancellation, and latest-snapshot drift as locked inputs for restore-strategy design.

Decision: approved

