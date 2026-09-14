Planner: Iteration 1
Timestamp: 2026-09-14T17:52:10Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-001.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`: every acceptance criterion resolves to a Jest spec or to `yarn lint` / `yarn compile` / `yarn test:jest`, none of which need a node, a network call or an operator.
- Split the work into a predicate in `utils/strings.ts`, a provenance resolver in a new `utils/assetName.ts`, a marking in `Asset.tsx` plus `Asset.scss`, and the same predicate applied to the second decode in `AssetContent.tsx`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md` (problem statement `:31-38`, goal one `:1094-1130`, requirements `:246-252`, testing strategy `:1454`, locked decision `:1641-1643`).
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-001` entry.
- `.agent/plans/asset-metadata-cache/prompt.md`, outcome one.
- `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and section list.
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md` as the section precedent.
- `.agent/skills/i18n-messaging/SKILL.md` for the message schema and the `yarn i18n:manage` flow.
- `.agent/workflows/test.md` for the Jest invocation, read against the `CLAUDE.md` trust map, which records that the hooks it describes do not exist.

Repo-Verified Findings Used To Shape The Plan:
- Six render sites import `components/assets/Asset`, and all six route the name through `renderPillContent`. The three surfaces the acceptance criterion names are among them, so one change reaches all three.
- `grep -rn "metadata?.name\|metadata.name" source/renderer --include=*.tsx` returns only `Asset.tsx:82`, `:176` and `:178`: nothing else resolves a display name.
- `grep -rn "styles.ascii" source` returns `Asset.tsx:209` and `Asset.scss:79`. The class has no other reader, so deleting it is safe.
- `AssetContent.tsx:122-126` performs a second unconditional UTF-8 decode of the same bytes. It is outside the task graph's `targetPaths` and inside the acceptance criterion's wording.
- `domains/Asset.ts:22-25` cannot host the predicate for the component's benefit: `Asset.tsx:71` types its prop as the plain `Asset` from `api/assets/types.ts:22-30`, and the values passed are `AssetToken` objects built by `utils/assets.ts`, not domain instances.
- Measured with the repository's Node v22.23.1: `Buffer.from('55534443zz','hex')` yields the four bytes `USDC`, and `Buffer.from('abc','hex')` yields one byte. A byte-range test alone would accept a silently truncated decode.
- `jest.config.js:180-203` transforms SCSS through `jest-css-modules-transform`, so a class assertion in jsdom is meaningful; `:156` picks up colocated specs with no config change; `:18` collects coverage with no threshold, so coverage cannot fail a run.
- No Cucumber feature and no other spec reads the `assetName` test id: only `Asset.tsx:206` and the three assertions in `Asset.spec.tsx`.

Planned Approach:
- Predicate accepts only a well-formed, even-length, non-empty hex string whose every decoded byte is `0x20` to `0x7E`.
- Resolver returns name plus provenance in the PRD order: registry ticker, registry name, printable decoded name, nothing.
- A minter-chosen name renders in an outlined chip, italic and muted, under its own test id, with a `title` explaining the provenance.
- The pop-over's decoded annotation renders only when the predicate accepts and is worded as a minter-chosen name.

Scope Guard / Self-Review:
- No cache, no IPC, no network, no dependency.
- No change to `assetNameASCII`, to the two sites that populate it, or to the search predicate.
- The `metadataNameChars` defect found in passing is recorded, not fixed.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T17:53:40Z

Five gaps, in descending severity.

1. Acceptance criterion one is argued rather than checked. The plan settles "distinct on the token list, the send form and the transaction list" by pointing at a table showing all three route through one component. That is a sound argument and it is not a test. The three surfaces do not pass identical props: `AssetInput.tsx:113` passes `hidePopOver small`, `WalletTokenHeader.tsx:78-89` passes `small={false}` with `metadataNameChars`, and `Transaction.tsx:699` passes neither. A prop combination that suppressed the marking would satisfy the argument and fail the criterion. The verification plan must drive the marking under each of those prop shapes.

2. `AssetContent.tsx` is changed with no test. The plan adds a file to the change list and no spec for it. The second decode is the same class of defect as the first and deserves the same treatment.

3. The resolver's input contract is unstated. `AssetsDropdown.tsx:20` types its assets as `Array<Partial<AssetToken>>`, so `metadata` and `assetName` can both be absent at the call site. The plan must say the resolver tolerates a partial asset and returns `null` rather than throwing, and a spec must drive it.

4. The message ids are described but not written down. Two new ids are being added to a namespace that already carries seven `assets.assetToken.param.*` entries at `en-US.json:66-73`, and `Asset.tsx` and `AssetContent.tsx` already share ids across files. The exact ids belong in the plan so the collision check is a reading rather than a discovery at implementation time.

5. Phase 1 is described in the task graph as four independently revertible commits. The plan never says this one is. It touches eight source files plus three generated artifacts and it should state, explicitly, that reverting it restores the previous rendering with no dependency on any other phase-1 task.

Nothing in the plan is wrong. The predicate's hex-shape check, the placement argument against `domains/Asset.ts`, and the decision to implement the ticker rung despite its visible consequence are all correctly reasoned and correctly evidenced. The gaps are all of the form "stated but not made checkable".

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T17:56:05Z

Plan Updates In Response To Critique:
- Added the three prop shapes to the Verification Plan, so acceptance criterion one is driven under `hidePopOver small`, under `small={false}` with `metadataNameChars`, and under the bare props the transaction list uses, rather than argued from the render-site table.
- Added `source/renderer/app/components/assets/AssetContent.spec.tsx` to Files Expected To Change and to the Verification Plan, covering the accepted and the rejected decode on the asset-name parameter row. Recorded as a fourth deviation from the task graph's `targetPaths`, for the same reason as the third.
- Stated the resolver's input contract in the Implementation Approach: it takes a partial asset, treats a missing `metadata` and a missing `assetName` as absent rungs, and returns `null` rather than throwing. Added the case to the Verification Plan.
- Wrote both message ids into the Implementation Approach: `assets.assetToken.minterChosenName` for the pill's title and `assets.assetToken.param.assetNameMinterChosen` for the pop-over annotation. Neither collides with the seven existing `assets.assetToken.*` ids.
- Added a revertibility line to Scope, naming what a revert restores and confirming no other phase-1 task depends on this one.

Resulting Approved Plan Shape:
- One predicate, one resolver, one marking, applied at the two places that decode asset-name bytes as text.
- Every acceptance criterion resolves to a command in the Verification Plan, with the negative cases driven explicitly rather than implied by a passing positive case.
- Four deviations from the task graph's `targetPaths`, each recorded with its reason; `domains/Asset.ts` explicitly not changed, with the evidence for why the suggested placement does not work.

Scope Guard / Self-Review:
- The revision adds tests and pins contracts. It adds no behaviour that was not already planned.
- Scope is unchanged: still renderer-only, still no cache, no IPC, no network, no dependency.
- The one decision with a visible consequence beyond the defect, promoting `ticker` to the first rung, is unchanged, still implemented as the PRD and the task graph specify, and still named in Risks with the duplication it produces against `formatters.ts:93-96`.

Outcome: Canonical task plan revised after critique and approved for build execution
