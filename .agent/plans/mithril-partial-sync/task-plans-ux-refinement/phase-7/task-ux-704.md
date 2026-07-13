# task-ux-704 — Code-quality remediation wave (CAT-A–H)

> Remediation wave from the 2026-07-04 deep code-quality review of the whole
> `feat/mithril-partial-sync-ux-refinement` branch (diff base `48b557a02`), cross-validated by an
> adversarial 4-agent verification pass against the code at HEAD and the locked design decisions
> (DD-703-1…14, PRD locked boundaries, ADR D-702a-1/2). Only findings that survived verification
> AND rated better than diminishing returns are in scope. Every CAT is behavior-preserving up to
> four documented accepted deltas: C-1's probe-failure error path (a strict correctness
> improvement recorded in the CAT-C equivalence argument); CAT-B B-3's transient
> `'already in progress'` rejection window during cancel teardown; CAT-C's one new warn line
> (the `_getActiveCustomPath` failure); and CAT-D Step 1.7's story-visible removal of the
> story-invented error-code line.

- Sprint: Mithril Partial Sync UX Refinement — phase-7
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved` (2026-07-04 — 2-agent validation pass over all nine docs: 5 blockers
  + 3 majors found, all doc-level, resolved via reviewer-specified edits and re-verified at HEAD;
  see `task-ux-704-plan-review.md`)
- Build status: `not-started`
- Interaction mode: `autonomous` — judgment calls were resolved during verification; implementers
  follow the per-CAT docs and escalate rather than improvise.
- Priority: high · Dependencies: task-ux-703 (completed)

## Structure

One thin master (this doc) + one plan doc per CAT. Each CAT is one commit (subject-only message,
no task IDs in source comments).

| CAT | Doc | Scope (one line) |
| --- | --- | --- |
| A | `task-ux-704-plan-cat-a.md` | Dead IPC facade + dead main-process plumbing — pure deletions |
| B | `task-ux-704-plan-cat-b.md` | Mithril main-process structure: import-cycle break, error-class home, param deadweight, decline dedupe |
| C | `task-ux-704-plan-cat-c.md` | Chain-storage/disk layer: de-fork `getManagedChainPath`, inert branch, dead API, lock dedupe, trivia |
| D | `task-ux-704-plan-cat-d.md` | Type-contract tightening: error-code union, `certifiedEpoch`, observables, status-guard casts |
| E | `task-ux-704-plan-cat-e.md` | Renderer cleanups: dead deep-link prop pipeline, forged validation, picker/message hygiene |
| F | `task-ux-704-plan-cat-f.md` | Storybook hardening: anchor-id constant, copy-proof story seam, typed stories, `??` fallbacks |
| G | `task-ux-704-plan-cat-g.md` | Repo hygiene: remove agent-tooling debris from repo root |
| H | `task-ux-704-plan-cat-h.md` | Comment cleanup (the pre-existing inventory, updated to ride last in this wave) |

## Execution order and seam contracts

Execute in letter order A → H. Hard constraints:

- **S1 (A↔B, `mithrilPartialSyncChannel.ts`):** CAT-A deletes only the four dead partial-sync
  shims and MUST keep `emitMithrilPartialSyncStatus`/`getMithrilPartialSyncStatus` (still imported
  by `mithrilPartialSyncNodeStartup.ts`). CAT-B rewires that import to injected dependencies and
  then deletes the two exports iff orphaned.
- **S2 (B↔D, stage-error class):** CAT-B moves `MithrilPartialSyncStageError` into
  `mithrilErrors.ts` (adding a `createPartialSyncStageError` helper beside it, with `code?: string`
  left untyped there). CAT-D types `.code` as the union on the class, on that helper, and on the
  surviving `code?` params — at their post-CAT-B homes, not at HEAD's.
- **S3 (E↔F, `Diagnostics.stories.tsx`):** CAT-E deletes the `showMithrilPartialSyncConfirmationOnOpen`
  pipeline and reworks the confirmation story's seam; CAT-F retypes the same stories afterwards.
- **S4 (H last):** CATs A–G delete code that the CAT-H inventory annotates. CAT-H re-locates
  entries by quoted text and marks entries mooted by A–G (listed in the CAT-H doc).
- CAT-A (A-3 import retarget at `handleDiskSpace.ts:23`, plus its spec), CAT-B (`:124` wiring),
  and CAT-C (~251-308 inert branch) touch different regions of the same file; letter order avoids
  conflicts. The other multi-CAT files (`MithrilPartialSyncService.ts` A/B/C-comments/D;
  `MithrilStartupGate.ts` A/B; `MithrilBootstrapService.ts` A-rider/B;
  `MithrilPartialSyncOverlay.stories.tsx` D/F; `MithrilStepIndicator.tsx` D/F;
  `DaedalusDiagnostics.tsx` E/F; `MithrilPartialSyncSection.tsx` and
  `DaedalusDiagnosticsDialog.tsx` D/E) were verified disjoint-region, with letter order plus each
  doc's relocate-by-quote rule preventing conflicts.
- CAT-C additionally owns fixing the four in-tree comments that its de-fork falsifies
  (`MithrilPartialSyncService.ts:157-159`, `:979-981`, `:1032-1034`, service spec `:2380-2381`)
  in the same commit rather than deferring them to CAT-H.

## Finding → CAT traceability (verified findings only)

| Finding | Short description | CAT | Verifier verdict |
| --- | --- | --- | --- |
| B2/D13 | 12 dead channel pass-through exports incl. a literal no-op; canonical-import retarget | A | CONFIRMED ×2 (independent) |
| A4a | `_logStream` write-only field + registration | A | CONFIRMED |
| A4b | `stdinInput` option: zero callers; `runCommand` silently drops it | A | CONFIRMED |
| A4c | `getStartupGateState()` / `get state()` zero callers | A | CONFIRMED |
| A4d | service `listSnapshots()`/`showSnapshot()` spec-only public API | A | CONFIRMED |
| A6 (subset) | production-empty listener arrays in `MithrilController` | A | PARTIAL → deletion subset only |
| B1 | mithril→ipc→mithril import cycle; inject via `NodeStartupDependencies` | B | CONFIRMED, feasible |
| B4 | stage-error factory threading; class trapped in service file | B | CONFIRMED |
| B3 (corrected) | workDir param deadweight (keep both fields) | B | PARTIAL → corrected scope |
| A2 (subset) | gate decline handlers ~90% copy-paste ×4; dead `value` param | B | CONFIRMED |
| A5 | identity-map fallback stage fn; duplicate cancel log | B | CONFIRMED |
| B9 | mixed log prefixes + narration triplet (in-diff) | B | CONFIRMED |
| C3 | `getManagedChainPath()` forks `checkDiskSpace`; 2 forks per disk-status IPC; equivalence proven | C | CONFIRMED |
| C4 | inert `chainEmpty` branch: identical arms, byte-verified | C | CONFIRMED |
| C5 (+C10f) | dead chain-storage API + byte-identical resolver delegations + stale docs | C | CONFIRMED |
| C6 (partial) | `_withMutationLock` duplicated line-for-line (dedupe only) | C | CONFIRMED |
| C10 (a–h) + C1 partial | trivia batch: unreachable throw, twin switch arms, shadowed var, layout-literal builder, unused `_ctx` params | C | CONFIRMED |
| D6 (narrowed) | error-code union erased to `string`; typed at the declaration sites after appending 5 codes missing from the union (copy-neutral `FAILED` fallthrough proven; `:114` lookup stays string) | D | PARTIAL → narrowed, corrected during planning |
| D9 | `certifiedEpoch` stale `| null` arm never crosses the wire | D | CONFIRMED |
| D3 (typing only) | `@observable progressItems` infers `any[]` | D | PARTIAL → typing only |
| E8 | status type-guard double-casts ×3 | D | CONFIRMED |
| E9b,c | loose handler/param types in new code | D | CONFIRMED |
| D7 (optional) | prod-dead `isActive` + `isMithrilPartialSyncActiveStatus` pair | D | PARTIAL → optional micro |
| E2 | dead `showMithrilPartialSyncConfirmationOnOpen` 3-layer pipeline (PRD D13 residual) | E | CONFIRMED |
| D12 | renderer forges `ChainStorageValidation` (`will-create` can be wrong, drives copy) | E | CONFIRMED |
| E9f,g,h | picker dead branch/dead field; duplicated message descriptors | E | CONFIRMED (g PARTIAL: dead data) |
| E9a,d | `as any` on typed prop; spec-only re-export | E | CONFIRMED |
| F7 (export variant) | `'step-3'` anchor magic string hand-copied ×6 | F | CONFIRMED |
| F8 | story drives UI via hardcoded copy text | F | CONFIRMED |
| F6 | story `as any` hides a missing required prop + a Props inaccuracy | F | CONFIRMED |
| F12 | `||` treats 0 as unset in story fallbacks | F | CONFIRMED |
| F1 | `ralph.sh` / `opencode.jsonc` / `review_output.json` at repo root, branch-introduced | G | CONFIRMED |
| — | comment-density inventory (~414 blocks) | H | pre-existing approved inventory |

## Verified but deferred (post-merge follow-up candidates; do NOT do in this wave)

- A1 `MithrilPartialSyncService` file split (1412 lines; seams verified — probe + metadata module) — large churn mid-PR; CAT-A/B already shrink the file.
- A3 `trackAsCancelable` → runner injection — lateral refactor; invalidates the slot-clobber regression suite.
- A2 gate enum/`_transition` removal + a dedicated `MithrilStartupGate` spec (transition logs are the only startup-gate breadcrumbs pre-rollout).
- A6 EventEmitter rewrite / broadcast signature change; B6 marker fail-safe sentinel type; B7 `setup.ts` twin suppression blocks; B8 metadata key-path trim; B5 bootstrap spec split (dropped).
- C1 full manager re-decomposition (+C11 spec rebalance — 1095-line spec spies on private trampolines); C2 coordinator re-architecture (optional: build the 9-callback bag once — it is stateless); C8 layout-error taxonomy move; C9 validation ternary normalization.
- D2 availability push model — re-plumbs the freshly locked DD-703-10 seam; pull must survive for window-reload resume anyway. D8 dead `logPath` wire field; D11 `partialSyncErrorCopy` layering move.
- E1 finalize-orchestration relocation to the store (allowed by DD-703-4 — behavior-locked, not location-locked) together with E3 `MithrilPartialSyncOverlayContainer`; E4 shared start-handler util (store-owned `startError` REJECTED — T12/DD-703-11 made surfacing caller-owned; extraction variant only); E5 variant copy table; E6 cancel-state helper; E9e modal shell extraction; E9i message-id namespace rename (would churn shipped ja-JP ids).
- F5 fixture literal-union types — `satisfies` is blocked by prettier 2.1.2/ts-eslint 5.20; post-merge use bare inference or an identity helper. F9a harness validation hook; F10 preset builder.

## Refuted / dropped / out-of-scope (do not re-raise)

- C7 Windows path-casing divergence — REFUTED: `path.win32.relative` lowercases both sides; `isPathWithin` is also case-insensitive on win32.
- F3 (all four task-401 shared-path changes) — documented QA fixes in `task-401-impl-review.md` (unhandled-rejection overlay during intentional wallet shutdown; user-requested Trezor log demotion). Keep.
- D5 `isOpaqueShutdownError` — log-level-only heuristic with documented rationale; keep.
- D4 startedAt anchor "duplication" — reset semantics genuinely differ; helper would carry only a ternary.
- D7 main claim — `isMithrilPartialSyncBlockingNodeStart` alias is the deliberate T24 outcome with real callers.
- D10 `'darwin-arm'` Platform widening — upstream commit `1e0f1cf17` riding the branch; not mithril-authored.
- E7 store-side progress-item normalization — infeasible: digest synthesis is view-timer state; items come from two backends.
- D3 nine-observables → `.ref` snapshot refactor — refuted: `_updateStatus` is a single `@action`, MobX batches the writes, no torn state exists; only the typing fix survives (CAT-D).
- F7 typed-field variant ("delete the anchor concept") — ripples into both main-process emitters + IPC types; export-constant variant chosen instead.
- B5 spec-size split; stale PR base / upstream ride-alongs (Electron 41, treefmt, deps) — process items for the PR author, not code tasks.

## Wave-wide verification gates (per CAT and final)

1. `yarn lint` and `yarn compile` — Node v24: regenerate `.scss.d.ts` via `typed-scss-modules`
   first; apply the jest `identity-obj-proxy` sidecar before treating jest failures as regressions.
2. Scoped jest over every touched spec file — comment/deletion edits must not change pass/fail
   counts except where a plan doc explicitly deletes spec blocks with their dead subjects.
3. `yarn prettier:check` on touched files; classify failures against pre-existing HEAD drift first
   (`git show HEAD:<f> | prettier --stdin-filepath <f>`); never reformat
   `toHaveBeenCalledWith('str', {obj})` call shapes (prettier 2.1.2 oscillation).
4. Behavior gate: no IPC channel names, no emitted status shapes (beyond the typed-only D6/D9
   tightenings), no user-visible copy (beyond the sanctioned D12 correction in CAT-E, which changes
   only which existing message is selected, never message text), and no locked-boundary behavior
   may change.
5. CAT-H additionally runs its own density + grep gates (see its doc).

## Review-log paths

- `task-ux-704-plan-review.md` (plan validation pass)
- `task-ux-704-impl-review.md` (per-CAT implementation reviews, when built)
