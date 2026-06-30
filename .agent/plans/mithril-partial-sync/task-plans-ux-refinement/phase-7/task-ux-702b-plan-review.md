# task-ux-702b — Plan Review Log

> Append-only Planner/Critiquer transcript for task-ux-702b. Newest entries at the end.

---

## Critiquer — 2026-06-30T00:00:00Z — orchestrated grill-with-docs + grounding re-verification

**Method.** One design-level grill pass (decisions D-702b-0…10 vs the 11 locked safety boundaries, terminology, edge cases) + four parallel grounding-verification passes (one per category pair) that re-checked every `file:line` citation and control-flow claim against live code on `feat/mithril-partial-sync-ux-refinement`.

**Grounding result.** Citations are accurate within ±2 lines throughout; no load-bearing claim was wrong. The central CAT-H claim — `getPartialSyncBehindness` forks `checkDiskSpace` per probe via `getManagedChainPath → getConfig → getDefaultStorageConfig` (NOT the `start()` preflight `_assertSufficientDiskSpace`) — was confirmed by live trace. All new symbols confirmed absent from the tree. Minor nits (corrected in-plan): prompt consumer fallback range `:146-156` (key `promptBodyUnknown`); `startPartialSync` optimistic call is `_updateStatus({status: START_PENDING_STATUS, …})` (object literal; true first statement is `let startError`); `DaedalusDiagnosticsDialog` Mithril prop is `isMithrilPartialSyncSignificantlyBehind`; full paths `source/main/utils/chainStorageManagerConfig.ts` and `source/renderer/app/api/network/types.ts:48-78`.

**Findings (resolved in this revision):**
1. (MAJOR) D-702b-4/CAT-D "keep the overlay up on failure" is defeated by its own mechanism — the backend resets to `idle` before the failed `fs.remove`, so `syncStatus()` pulls `idle` and the overlay hides anyway; the staging/`.lock` leak is renderer-unreachable. Reframed: the two genuine deliverables are no-unhandled-rejection + no-premature-optimistic-hide-on-success; the leak fix is deferred to the out-of-scope backend reorder. Added a failure-path store-spec case (status→idle ⇒ `shouldShowOverlay===false`).
2. (MAJOR) #16's headline early-sync fix can ship silently inert if `extractCertifiedEpoch`'s paths miss the real beacon key, with no failing test. Made the operator beacon-key check a hard checkpoint before CAT-H and required a known-good-shape fixture test.
3. (MINOR) CAT-H's 5-min input cache lags the offer signal during NORMAL sync (no lifecycle transition) — conservative over-offer, prompt protected by the renderer near-tip hide, Diagnostics not. Documented as an accepted tradeoff in D-702b-9.
4. (NIT) "ticks up ~1 epoch" → "by the certified-frontier lag (commonly ~1, occasionally more); never down."
5. (NIT) CAT-H epoch exposure pinned to a single approach: sibling `_getCachedCertifiedEpoch()`.
6. (NIT) §Sequencing store-serialization now reads B → A → D → E (CAT-B is the first store editor).
7. (NIT) CAT-C reorder guardrailed as a stable sort (not a filter) to honor locked invariant #2.
8. (NIT) CAT-H must add `certifiedEpoch` to the probe result conditionally (mirror `behindByImmutables?`) to keep the four `toEqual` cases green; CAT-F must not touch the unrelated `NotificationActions.primaryButton`.

**Invariant compliance.** All 11 locked safety boundaries hold. The two at-risk ones — #4 (backend owns the behind-ness offer; the renderer `certifiedKnown`/`computeBehindByEpochs` can only SUPPRESS an offer or set anti-flash known-ness, never CREATE an offer) and #11 (CAT-C restores the empty-chain bootstrap default; pending operator visual confirmation) — both hold as designed.

**Behavior-shift flagged (kept, not blocked).** #16 intentionally moves the proactive prompt earlier (post-verifying early/mid sync) than the original ISSUE-7 manual assessment scoped it. This is the correct direction (Mithril helps most when far behind) and is the locked intent of D-702b-10; retained. Operator may override if near-end-of-sync timing was intended.

Decision: requires_changes → all applied this revision (see Planner entry).

## Planner — 2026-06-30T00:00:00Z — applied review fixes
Applied findings 1–8 to `task-ux-702b.md` and `task-ux-702b-decisions.md` (D-702b-4 reframe + correction bullet; D-702b-9 normal-sync tradeoff; D-702b-10 §2 tick-up wording; CAT-D acceptance/tests; CAT-H pre-checkpoint + conditional-result + single-getter + path fixes; CAT-A/CAT-B nits; CAT-C guardrail; CAT-F warning; sequencing B→A→D→E; Risks pre-CAT-H beacon-key checkpoint). Plan subsequently decomposed into per-category `task-ux-702b-cat-{a..h}.md` docs. Decision: approved.

---

## Critiquer (external grounding pass) — 2026-06-30T00:00:00Z — CAT plan-corpus consistency & validity re-verification

**Source.** A third external review of all 10 task-ux-702b docs (master `task-ux-702b.md`, `-decisions.md`, `-plan-review.md`, `cat-a`…`cat-h`), with ~65 load-bearing `file:line` / behavioral claims re-verified against live code on `feat/mithril-partial-sync-ux-refinement` via three parallel grounding passes, plus the tasks-JSON and cross-doc citations. Consolidated here per the append-only log convention.

**Verdict.** The corpus is solid. Sequencing (B→A→C→D→E→H→F→G), finding→category traceability (#1–#16), decision IDs (D-702b-0…10), the store-editor serialization (B→A→D→E), the overlay serialization (C→D→G), and the locked-invariant citations are internally consistent across all 10 files. Per the reviewer, ~95% of grounding claims are accurate to ±2 lines, including subtle ones (the `:98-110` anti-flash test logic, the `checkDiskSpace`-via-`getManagedChainPath→getConfig` chain, the `node_tip` vs `network_tip` epoch-nullability asymmetry, the ISSUE-1 comment, all "new symbol absent" claims). The issues are concentrated in **CAT-H** (the most complex category) plus a few stale references; **none invalidate the plan** — they are precision / scope-tracking / citation fixes.

**Findings (ranked) — all RESOLVED this revision (see Planner entry); the load-bearing ones re-grounded against live code before applying:**
1. **(Medium) CAT-H step 2 ↔ step 5 contradiction on the return objects.** `cat-h.md` step 2 (and the master "switch" step) call the success + `gap<=0` returns "byte-for-byte unchanged", but step 5 adds `certifiedEpoch` to both; `cat-h.md` step 5's example also rewrote `behindByImmutables` into a conditional spread. Live code (verified `MithrilPartialSyncService.ts:686-689`) emits a plain literal `{ isSignificantlyBehind, behindByImmutables: gap }` (`gap` always `>0` there). **Resolution (operator-confirmed: minimal append):** append only `...(certifiedEpoch != null ? { certifiedEpoch } : {})`; keep `behindByImmutables: gap` as the literal; scope "byte-for-byte unchanged" to the gap/threshold *logic* and note step 5 supersedes it for the two return literals.
2. **(Low-Med) CAT-H mis-describes the existing return + the four `toEqual` fixtures.** "mirroring `behindByImmutables?`" conflated the type-optional field (`:670-672`) with the unconditional runtime emit. Only two of the four cases (`:1228`/`:1241`) carry `behindByImmutables`; `:1254`/`:1266` are `{ isSignificantlyBehind: false }` (verified). The real reason the `!= null` guard is mandatory: `_getCachedCertifiedEpoch()` returns **`null`**, and Jest `toEqual` ignores `undefined` but **not** `null`. All three wording points corrected in `cat-h.md` + master.
3. **(Low) CAT-H #16 extractor fixture example used the wrong key.** Doc said `beacon: { epoch: 320, immutable_file_number: 25 }`; the real fixture at `mithrilSnapshotMetadata.spec.ts:30-32` is `cardano_db_beacon: { immutable_file_number: '25' }` (key `cardano_db_beacon`, string value — verified). Corrected the "mirror the existing case" example in `cat-h.md` + master to the real key/shape.
4. **(Low) Tasks-JSON was in the pre-#16 state** (`estimatedHours: 8`, `summary.estimatedTotalHours: 98`, "14 + 1 = 15", `targetPaths` missing the five #16 files), self-flagged "Pending re-apply" at master `:1181-1186` — a real scope-drop risk if the runner drives scope from the JSON. **Resolution (operator-confirmed: full #16 re-apply):** bumped 8→10h / 98→100h, added the 6 #16 paths, and extended description/notes/tests/acceptance for #16; the master's pending note is now flipped to "applied".
5. **(Low/cosmetic) Stale cross-references in the master "Numbering note."** Cited `decisions.md:11-14` as still saying "13 findings"/"the 14th" (those lines already read "14 actionable findings (#1–#14)") and cited the category map at `:182-190` (it is at `:370-396`, table `:372-381` — verified). Conclusion (standardize on 14+1) unchanged; citations corrected.

**Informational (disclosed, not defects).**
- #16 shifts the proactive prompt earlier than the original ISSUE-7 manual-assessment scope (now appears post-verifying during early/mid sync) — the single biggest behavioral change in the set, but the intentional, locked direction of D-702b-10 with operator override available (see Critiquer `:25` above). Re-confirmed retained.
- The decisions-doc category-map "Primary files" columns (`:374-375`) are non-exhaustive summaries; the authoritative file lists live in each cat doc's "Exact files" + the master. Not a contradiction.

Decision: requires_changes (precision / scope-tracking only) → all applied this revision.

## Planner — 2026-06-30T00:00:00Z — applied external-grounding fixes (findings 1–5)
Operator chose **minimal append** (finding 1) and **full #16 tasks-JSON re-apply** (finding 4) via grill-with-docs. Applied:
- **`task-ux-702b-cat-h.md`** — step 2: scoped "byte-for-byte unchanged" to the gap/threshold logic + a supersede note (1); step 5: rewrote the result example to a literal `behindByImmutables: gap` + a conditional `certifiedEpoch` spread for both the success and `gap<=0` returns, with the null-vs-undefined `toEqual` rationale and the two-of-four-cases correction (1, 2); Tests: corrected the four-`toEqual`-cases description (2) and the extractor fixture example to `cardano_db_beacon: { … }` (3).
- **`task-ux-702b.md`** (master) — CAT-H "switch" step scope note (1); CAT-H #16 step return-shape + `behindByImmutables?` type-vs-runtime + `toEqual` rationale (1, 2); extractor fixture example `cardano_db_beacon` (3); Numbering note stale citations → `decisions.md:11-14` (already-corrected) + category map `:370-396` (5); Tasks-JSON section: hours 8→10 / totals 98→100 and the pending-#16 note flipped to "applied" (4).
- **`mithril-partial-sync-ux-refinement-tasks.json`** — `estimatedHours 8→10`, `summary.estimatedTotalHours 98→100`, +6 #16 `targetPaths` (`mithrilSnapshotMetadata.ts`/`.spec.ts`, `MithrilController.ts`, `mithrilPartialSyncChannel.spec.ts`, `mithril-partial-sync.types.ts`, `DaedalusDiagnosticsDialog.tsx`), `description` 15→16 + `D-702b-0..10` + a #16 paragraph, and one #16 entry each in `implementationNotes`/`testCases`/`acceptance` (4). JSON re-validated.

No decision was re-opened; findings were precision, scope-tracking, and citation fixes only. Decision: approved.
