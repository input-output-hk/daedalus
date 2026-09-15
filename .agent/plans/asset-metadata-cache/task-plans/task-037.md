## Task ID and Title

`task-037` — Jest coverage across the source setting and the chain channel.

## Why Chosen Now

Every phase 7 module was tested by the task that built it, against the criteria
that task was written to. Nothing has yet asked which lines nothing reaches, and
`task-026` established what that question finds: every unreached line in this
plan's main-process modules was a failure path, which is to say a line that runs
on the day something breaks.

## Interaction Mode

`agent_execution`.

## Scope

The PRD's Testing Strategy items that name phase 7, checked off; then a coverage
run over the seven modules phase 7 added or changed, and the paths it shows
nothing reaches.

## Non-Goals

- No source change to make a line reachable. If a case cannot be written without
  changing the module, that is a finding to record.
- No manual QA. That is `task-038`.
- No network in any spec.
- No pursuit of a number. A line that cannot be reached without contriving a
  state the module cannot be in is recorded, not chased.

## Dependencies

`task-034`, `task-035`, `task-036`.

## Research Consulted

- `asset-metadata-cache-prd.md:1448-1517`, the Testing Strategy, and the four
  items `task-026` named as phase 7's rather than skipping silently: the
  metadata source URL validator, `getAssetMetadataSourceIdFromUrl`, pointer
  resolution with its three local checks, and the chain-row assertion.

## Docs, Workflows, and Skills Consulted

- `task-026`'s plan, as the model: coverage first, then one case per unreached
  path, with the ones that stay unreached written down.

## Live Repo Findings Verified For Planning

Each Testing Strategy item the PRD assigns to phase 7, against the suite as it
stands after `task-036`:

1. **The metadata source URL validator.** Covered.
   `config/assetsConfig.spec.ts` has nine cases: five accepting, four rejecting.
2. **`getAssetMetadataSourceIdFromUrl`.** Covered. `utils/assets.spec.ts`, five
   cases including the trailing-slash form that maps to `custom`.
3. **Pointer resolution over a recorded pair, and the three local checks.**
   Covered. `chainPointerVerification.realfs.spec.ts` runs against a real
   preprod block, and each of the three byte checks has a tampering case.
4. **The chain-row assertion.** Covered.
   `assetMetadataResolver.realfs.spec.ts` asserts all six columns.

The coverage run over the phase 7 modules, taken on 2026-09-16 before this task:

5. **`chainPointerVerification.ts` was the weakest, at 84.86 percent of
   statements and 68.13 of branches.** The unreached region was almost all of
   the metadatum reader, the auxiliary-shape fall-throughs and six of the eleven
   rejection reasons.
6. **`immutableBlockReader.ts` was at 89.06 and 70.21**, with five of the
   fail-closed paths unreached: the slot mismatch, the malformed secondary
   offset, the block bounds, an unknown version on the block's own chunk, and a
   tip that cannot be found.
7. **`koiosClient.ts` was at 92 and 82.6**, missing the throttle that falls
   between a batch's two calls, the retry delay, and three response-shape
   rejections.
8. **`httpTransport.ts` was at 92.3 and 70.58**, and its response cap was never
   driven as a parameter, only as its default. That is exactly the property
   `task-033` moved it for.
9. **`AssetMetadataSettings.tsx` was at 94.64 percent of statements**, with the
   two external links and the unavailable option's description unreached.

## Files Expected To Change

- `source/main/assets/chainPointerVerification.realfs.spec.ts`
- `source/main/assets/immutableBlockReader.realfs.spec.ts`
- `source/main/assets/koiosClient.spec.ts`
- `source/main/assets/cborSpan.spec.ts`
- `source/main/assets/assetRegistryClient.spec.ts`
- `source/renderer/app/components/settings/categories/AssetMetadataSettings.spec.tsx`
- `source/main/assets/chainPointerVerification.ts`, only if a case cannot be
  written without it, and then as a finding
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**One case per unreached path, and the case says what the path is for.** A
rejection reason gets a case that asserts that reason and not merely that the
result was a rejection, because six of them are reached from the same block of
guards and asserting the shape would let five of them rot.

**The fail-closed paths in the reader need fixture options rather than fakes.**
The reader takes a directory, so a malformed index is a malformed file. The
spec's writer grows the options to produce each shape: an entry whose slot is
not the one the index points at, an offset that is not a whole entry, a block
offset past the end of its chunk, a version byte on the block's own chunk that
chunk zero does not have, and an index that stops short of the slot asked for.

**The transport's cap is driven as a parameter.** Two calls against the same
loopback server with different caps, one refused and one not. That is the change
`task-033` made and nothing asserted it.

## Acceptance Criteria

1. Every Testing Strategy item the PRD assigns to phase 7 has a spec, named in
   this task's closing note.
2. Every fail-closed path in `immutableBlockReader` is driven, or the reason one
   is not is recorded.
3. Every rejection reason `confirmChainPointer` can return is driven by a case
   that asserts that reason, or is recorded as unreachable with the argument.
4. The response cap is asserted as a parameter rather than as a default.
5. `jest` passes, and the other five checks with it.
6. No network access from any spec.
7. No source change, or a source change recorded as a finding with its reason.
8. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- A coverage run over the same module set before and after, quoted in the
  closing note, so the claim is a measurement rather than an impression.
- Criterion 6 by construction: every spec here uses a stubbed transport, a
  temporary directory, or a loopback server the spec itself starts.
- Criterion 3 by enumerating the reasons in the module and matching each to a
  case, and by writing down the ones that stay unreached with why.
- All six Nix checks.

## Risks and Open Questions

- **A high coverage number for a hand-written parser is not proof.** The
  structural reader has 99 percent of its statements covered and that says the
  cases exercise it, not that its boundaries are right against every encoder a
  minter might use. What says that is the real block, which came from a real
  encoder nobody here wrote.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-037.targetPaths` corrected to the
  spec files that exist, which are not the ones the graph names, because each
  task tested its own module; `task-037.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-037-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-037-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The paths that run when a pointer is wrong, a chain database is damaged or an
index misbehaves are driven rather than assumed.

## Final Outcome

Complete.

## Self-Review

The useful version of this task is not "add tests until the number goes up". It
is to ask which lines nothing reaches and why, and phase 7's answer is the same
as phase 6's: almost all of them are refusals. A module whose entire job is to
refuse a bad pointer, and whose refusals are untested, is a module whose passing
tests say only that it accepts good ones.
