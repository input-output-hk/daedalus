Planner: Iteration 1
Timestamp: 2026-09-14T18:31:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-002.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`: every acceptance criterion resolves to a Jest spec or to one of the four Nix checks.
- Split the work into three edits inside `source/renderer/app/utils/assets.ts` (the merge helper, the deleted filter, a guard in the comparator) plus a new colocated `assets.spec.ts`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the merge-helper section at `:1254-1275` and the two further consumers at `:1277-1292`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-002` entry, plus `task-016`, `task-017` and `task-023` to establish who owns each consequence.
- `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
- `.agent/plans/asset-metadata-cache/task-plans/task-001.md` as the immediate precedent.
- `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.

Repo-Verified Findings Used To Shape The Plan:
- `utils/assets.ts:126-128` destructures `uniqueId` out of the lookup and `:148` filters on it, so a row whose lookup missed is dropped. `AssetsStore.ts:60` returns `[]` before the first successful poll, so every lookup misses then.
- Three call sites for `getNonZeroAssetTokens`, and only one of them passes tokens that lack `uniqueId` and `assetNameASCII`: `WalletTransactionsList.tsx:240`, fed by `api.ts:3321-3329`.
- `api.ts:3216` and `:3227` set `uniqueId`; `:3221` and `:3232` set `assetNameASCII` with `hexToString`. The task graph cites `:3218` and `:3229`, which are two lines later in this tree.
- `sortAssets` dereferences `fingerprint` unguarded at `:189`, `:192`, `:197` and `:200`. The `:148` filter is the only thing that keeps an undefined fingerprint away from it today.
- `isLoadingAssets` becomes permanently false at `WalletSendPage.tsx:148` and `WalletTransactionsList.tsx:244`. `task-023` owns it and names `task-002` as a dependency.
- No `utils/assets.spec.ts` exists, and no Cucumber feature or story reads either helper.
- `perSystem/checks.nix:15-32` builds from `srcWithoutNix`, derived from `inputs.self`, so an unstaged new file is invisible to every check.

Planned Approach:
- Identity comes from the token; only `metadata`, `decimals`, `recommendedDecimals` and `fingerprint` come from the lookup.
- A transaction-originated row derives `uniqueId` as policy id followed by asset name, and `assetNameASCII` with the same `hexToString` the wallet-balance mappings use.
- The `uniqueId` filter is deleted, and the comparator stops throwing on an absent fingerprint.

Scope Guard / Self-Review:
- No cache, no IPC, no network, no dependency.
- No change to `api.ts`, to `AssetsStore`, to the CSV generator, or to the `isLoadingAssets` conditions.
- No fingerprint invented anywhere.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T18:34:10Z

Four gaps, in descending severity.

1. The one deviation from the task graph is argued, not evidenced. The plan asserts that removing the `uniqueId` filter makes `sortAssets` throw, and then changes `sortAssets` on the strength of that assertion. The reasoning is sound and it is still only reasoning. A deviation that a reviewer cannot check is indistinguishable from scope creep. Run the new `getNonZeroAssetTokens` case once against the unguarded comparator, record the `TypeError` verbatim, and only then apply the guard.

2. Nothing checks that the change is a no-op where it is supposed to be one. Two of the three call sites pass tokens that already carry `uniqueId` and `assetNameASCII`, and the entire argument that this is a safe change rests on those rows merging exactly as they do today. The plan has no assertion to that effect. A field-by-field parity case belongs in the spec.

3. Acceptance criterion one is reported as settled when half of it is not. The criterion says every token renders "with fingerprint and raw quantity", and this task supplies no fingerprint by design. The plan explains that honestly in prose and then lists the criterion among those the task meets. Say partially met, in the criterion and again in the Final Outcome.

4. A task-graph inconsistency found while tracing ownership is not recorded anywhere. `task-016`'s implementation notes require the locally computed fingerprint from `task-005`, but `task-005` is absent from `task-016`'s `dependencies`. Whoever picks up `task-016` from the graph alone will be missing a prerequisite. `CLAUDE.md` says to record a divergence rather than work around it, and that applies to the plan's own artifacts.

Nothing in the plan is wrong. The decision to derive `assetNameASCII` with `hexToString` rather than the stricter predicate `task-001` added, on the grounds that the field is a search key and not a rendered one, is correctly reasoned and correctly evidenced, as is the decision to leave `api.ts` alone.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T18:36:30Z

Plan Updates In Response To Critique:
- Added a step to the Verification Plan that runs the `getNonZeroAssetTokens` case against the unguarded comparator first and records the `TypeError` in the implementation log, so the one deviation is evidenced rather than argued.
- Added a field-by-field parity case to the Verification Plan and a matching acceptance criterion: a fully populated token merged against a fully populated lookup must produce exactly what the current implementation produces.
- Reworded acceptance criterion one to say it is partially met by this task, with the fingerprint half attributed to `task-005` and `task-016`, and required the Final Outcome to repeat that rather than report a pass.
- Recorded the `task-016` dependency gap under Required Docs, Research, and Tracking Updates, with the note that it is surfaced rather than silently edited into another task's entry.
- Extended the last closure criterion to cover `@ts-expect-error` as well as `@ts-ignore`.

Resulting Approved Plan Shape:
- Three edits in one file plus one new spec, with one recorded deviation that a test proves necessary.
- Every acceptance criterion resolves to a command or an assertion in the Verification Plan, with the negative cases driven explicitly.
- Three consequences that belong to `task-016` and `task-023` recorded with their owners, and one task-graph inconsistency surfaced.

Scope Guard / Self-Review:
- The revision adds tests and pins claims. It adds no behaviour that was not already planned.
- Scope is unchanged: still renderer-only, still one source file, still no cache, no IPC, no network, no dependency.

Outcome: Canonical task plan revised after critique and approved for build execution
