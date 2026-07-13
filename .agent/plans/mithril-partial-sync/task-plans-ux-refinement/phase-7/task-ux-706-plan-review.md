# task-ux-706 — Plan review (master + CAT-A…G)

> **✅ Resolved 2026-07-10 (all findings dispositioned — see master DD-706-9).** R-1 → option **(a)**
> downgrade (C1+C3 ship; size/jitter → Tier-3 follow-up with corrected root cause). R-2 → fix relocated
> to the top-level `beforeEach` as prescribed. Native-dialog copy → CAT-A Step A5 (casing-only). JA
> Help-menu label → B1 extended to the main-process locale (round-2 flag). All remaining corrections
> applied as written to the master + CAT docs. This document is the review record; the plan docs are
> the source of truth for the build.

> Plan-validation pass over `task-ux-706.md` and the seven per-CAT docs, run 2026-07-10 against the
> working tree of `feat/mithril-partial-sync-ux-refinement` (HEAD `01a58dcba`). Method: every concrete
> code reference (file/line/key/quoted string) and every feasibility-bearing claim was checked against
> the codebase by validation agents — six of the eight areas by two independent agents; disagreements
> were adjudicated against the source directly. `MithrilPartialSyncService.spec.ts` and
> `partialSyncErrorCopy.spec.ts` were actually executed (jest, Node v24 — main-process specs, no
> renderer sidecar needed). No repo files were modified by the review.

## Verdict at a glance

| CAT | Verdict | Gating items |
| --- | --- | --- |
| A | ✅ implement with corrections | key list 29→30; unowned main-process dialog copy; 1 descriptor name |
| B | ✅ implement with corrections | Help-menu JA label gap (decision needed); prefer `environment.isMacOS` |
| C | ⚠️ **C2 needs re-plan** (C1/C3 fine) | DD-706-4 mechanism unsound as specced — see R-1 |
| D | ✅ implement with corrections | 2 files missing from threading list; acceptance contradicts locked copy |
| E | ✅ implement with corrections | decline button in unlisted file; invert primary/fallback CSS technique |
| F | ✅ implement with corrections | F2 fix location can't turn the spec green (9 red, not 4); generated-artifact procedure |
| G | ✅ implement with corrections | G2 trace wrong: download spawns **outside** the mutation lock |
| Master | ✅ sound | S2 attribution nit; one locale edit lands outside Blocks A/B; one unlisted spec seam |

Overall: the plans are unusually accurate at the line level (the large majority of the ~200 checked
citations are exact), the DD-706-3 dead-code gate re-verified clean, and the DD-706-6 design is
confirmed feasible end-to-end. Two findings gate implementation: **R-1** (CAT-C C2) and **R-2**
(CAT-F F2). Everything else is a plan-text correction that prevents a mis-implementation, not a
redesign.

---

## R-1 · BLOCKER — CAT-C C2 / DD-706-4: the size-plumbing fix will not smooth the bar, and the size line it adds is misleading

Confirmed independently by both CAT-C validators and re-verified directly in
`MithrilStepIndicator.tsx:143-197`. Three compounding problems:

1. **Wrong byte figure.** `_latestSnapshot.size` is the **whole-DB** snapshot size, not the
   partial-range delta. The service itself treats it that way
   (`MithrilPartialSyncService.ts:1066-1074` — disk calc subtracts what's already on disk;
   `mithrilSnapshotMetadata.ts:16-22` — `raw.total_db_size_uncompressed` fallback chain), and partial
   sync downloads only `--start/--end` chunks (`MithrilPartialSyncService.ts:569-572`). In the
   weighted branch (`MithrilStepIndicator.tsx:183-188`),
   `snapshotWeight = size / (size + ancillaryBytesTotal) × 100` with a whole-snapshot numerator gives
   ≈97-99% on mainnet — **at or above the 95% fallback it replaces**. With `filesTotal = 2`, each file
   still swings the combined bar ~47 pts. The jitter is unchanged (marginally worse).
2. **Ancillary term is pinned at 0.** The weighted branch consumes `ancillaryPercent`
   (`MithrilStepIndicator.tsx:146,163`), sourced from the `ancillaryProgress` prop
   (`:483: typeof ancillaryProgress === 'number' ? ancillaryProgress : 0`). The bootstrap path
   computes and passes it (`MithrilBootstrapStore.ts:78-88` → `MithrilBootstrapPage.tsx:149`); the
   partial-sync path has no such field — `MithrilPartialSyncStore` doesn't hold one, `App.tsx:100-111`
   doesn't build one, `MithrilPartialSyncOverlay.tsx:212` doesn't pass one. The plan's step 4 threads
   only `snapshotSizeBytes`, so ancillary download progress would still contribute nothing to the bar.
3. **The "≈ {totalSize} total" line overstates the download.** On partial sync the rendered value
   (`MithrilStepIndicator.tsx:225-233`) would be the full snapshot size (e.g. "≈ 150 GB total") on a
   run fetching a 2-chunk delta plus ancillary — the opposite of Extra #6's "catch-up size" intent.

Also latent: the weighted branch additionally requires `ancillaryBytesTotal > 0`
(`:180-181`), which the backend only emits once the mithril client reports `bytesTotal`
(`MithrilPartialSyncService.ts:611-613`) — whether that arrives concurrently with the chunk counts is
a runtime unknown. And if `snapshotSizeBytes` is added to `transferProgress`, note the payload is
reset to `{}` at `MithrilPartialSyncService.ts:187, 345, 401, 457, 798` — deriving it from
`this._latestSnapshot?.size` inside `_updateStatus` survives the resets; a set-once field does not.

**Required before build — pick one and update CAT-C/DD-706-4:**
- **(a) Downgrade.** Ship C1 (relabel) + C3 only; move the jitter fix and the size line to the
  already-provisioned Tier-3 follow-up, with the corrected root cause recorded (fallback weights are
  not the problem; the missing *delta* size and missing ancillary percent are).
- **(b) Re-scope the plumbing.** Thread a **ranged-delta** byte size (does not exist today —
  `MithrilSnapshotItem.size` is the only size field, `mithril-bootstrap.types.ts:49-55`; it would
  have to be derived, e.g. from the download child's reported totals) **and** an `ancillaryProgress`
  percent for the partial-sync path, mirroring the bootstrap store's computation. Bigger than the
  planned change; the "≈ total" label semantics still need a partial-sync-specific message or value.
- **(c) Fallback reweight only.** Accept the fallback branch as the partial-sync reality and smooth
  there (e.g. incorporate ancillary bytes into the fallback), which DD-706-4 currently forbids
  ("do not silently reweight") — so this needs an explicit decision reversal, not a silent switch.

The mechanically-verified parts of C2 stay valid whichever way this goes: the type seam
(`mithril-partial-sync.types.ts:61-67`), the store sites (`MithrilPartialSyncStore.ts:44-48,
180-184`), the emit path (unfiltered `{...this._status}` → IPC → store `_applyStatus`; no field
whitelisting), and the fact that `MithrilProgressView` already declares/forwards `snapshotSizeBytes`
(`:25`, `:200`) so only App.tsx + overlay + store + type need edits.

## R-2 · BLOCKER — CAT-F F2: the prescribed fix cannot turn the spec green

Evidence from two independent live jest runs: `MithrilPartialSyncService.spec.ts` fails **9 of 76**
tests today, not the 4 the plan names.

- The plan's set — `:414` (primary) and `:2168/:2181/:2194` — all fail as diagnosed. ✔
- Four more behindness-adjacent tests fail on the same falsy-`pathExists` root cause and *would* be
  incidentally fixed by the plan's helper-local fix: `:2339` (concurrent aggregator/local read) and
  the certified-epoch trio (`:2377/:2391/:2404`).
- **One failing test escapes both proposed fix sites**: the slot-clobber regression
  `an untracked behind-ness probe mid-download … cannot clobber the durable download slot`
  (assertion at `:1565`). It lives in the `'cancel correctness'` describe, does **not** call
  `stubLocalImmutableNumber`, and inlines its own `getManagedChainPath`/`readdirMock` stubs
  (`:1561-1564`). Fixing inside the helper or a behindness-scoped `beforeEach` leaves it red,
  violating F2's own acceptance ("scoped spec green … no new red").

**Required correction:** put the default in the top-level `beforeEach` (`spec.ts:113-114`) as
`pathExists.mockResolvedValue(true)`. Safe: `fs.pathExists` has exactly one production consumer in
this spec's reach (`MithrilPartialSyncService.ts:916`), and the marker module is separately
`jest.mock`ed (`spec.ts:6-10`). This one line fixes all 8 secondary failures including `:1565`.

Also under-scoped (not a blocker, but plan-text should say it): the F2-primary rewrite is not an
assertion swap. `start()` resolves snapshot metadata **twice** (`:194` preparing, `:235` drift
re-resolve), so the current `mockResolvedValueOnce` must become `mockResolvedValue` or the second
call trips the drift throw at `:242`; and asserting the ledger-only contract means stubbing the
download path far enough to check `_runCommand` receives `'--start', '25', '--end', '25'`
(`MithrilPartialSyncService.ts:569-572`) — the sibling test at `:428` (and the `--start 12 --end 25`
assertion at `:220`) is the setup pattern. Confirmed there is **no existing equivalent assertion**
anywhere (no `mithrilPartialSyncPreflight` spec exists), so the "delete if duplicate" branch of F2
does not apply — rewrite is mandatory.

---

## Per-CAT corrections (must-fix plan edits, then implement)

### CAT-A — vocabulary sweep

1. **A1 list is 30 keys, not 29 (31 occurrences).** All 29 listed ja-JP lines check out; **line 378**
   (`loading.mithrilPartialSync.progress.nodeStartingDetail`) also carries `Mithril同期` and is missing
   from the enumerated list (the spot-check bullet partially covers it via A3). Line **355 contains the
   substring twice**. Fix the count/list so a strict executor doesn't leave stragglers; the acceptance
   grep ("no `Mithril同期` remains") is the real gate.
2. **Unowned surface: main-process native dialog.**
   `source/main/mithril/mithrilPartialSyncNodeStartup.ts:103` (`'Wipe chain and full Mithril sync'`)
   and `:109` (`…a full Mithril sync can run again.`) are user-facing (Electron `showMessageBox`
   before the renderer exists), lowercase, non-i18n, and **owned by no CAT** (grepped master + all
   seven CAT docs). Either add them to CAT-A's scope or scope the acceptance criterion to
   i18n/renderer strings and log the dialog as a follow-up — currently the criterion "EN 'Mithril
   sync' (lowercase) no longer appears in user-facing strings" fails after CAT-A completes.
3. **Descriptor name inaccuracy:** the A3 source def is `partialSyncNodeStartingDetail`
   (`MithrilBootstrap.messages.ts:305`, defaultMessage `:308`), not
   `partialSyncProgressNodeStartingDetail`. (Plan's "confirm by id, not line" hedge already covers
   this; correcting anyway.) The "line 19 (title area)" annotation is likewise the `description`
   descriptor's default, not a title.
4. **Spec inventory (add to acceptance):** the sweep breaks hardcoded old-copy assertions in
   `SyncingConnectingMithrilPrompt.spec.tsx:54,93,146`, `MithrilProactivePromptContainer.spec.tsx:68`,
   `MithrilPartialSyncOverlay.spec.tsx:99,292,307`, `MithrilBootstrap.spec.tsx:168,239,243,263`.
5. Verified clean, for the record: A2's three lowercase-EN i18n hits are exhaustive; A3/A4 "now"
   quotes are byte-exact in both locales and their completeness greps found no missed occurrence;
   out-of-scope claims (title 345 / accept 297) exact; `Mithril同期` has zero hits in `en-US.json` /
   `defaultMessages.json`.

### CAT-B — JA consistency + shortcut

1. **The "last user-facing 「診断」" claim misses the Help menu itself.**
   `source/main/locales/ja-JP.json:36` — `"menu.helpSupport.daedalusDiagnostics": "Daedalus診断"` —
   is the menu item the B1 handoff note points users at (`osx.ts:204` / `win-linux.ts:223` use this
   label). After B1, JA copy says "Daedalus Diagnostics … under the Help menu" while the menu reads
   「Daedalus診断」. **Decision needed:** extend B1 to the main-process locale
   (`Daedalus診断` → `Daedalus Diagnostics`, consistent with the "full page name in English" direction)
   or explicitly scope B1 to the renderer and log the menu label for round 2. Adjudicated against
   source directly (one validator missed it by grepping only the renderer locale).
2. **B4: use the precomputed boolean.** The renderer idiom is `global.environment.isMacOS`
   (`environment.types.ts:33`, populated via `checkIsMacOS` in `source/main/environment.ts`;
   precedents: `crypto.ts:116`, `AppUpdateContainer.tsx:18`, `StakePoolsSettings.tsx:152`) — one hop
   simpler than the plan's `checkIsMacOS(...)` wiring. Option 1 (placeholder) is confirmed feasible:
   the same file already uses `intl.formatMessage(messages.promptBody, { epochs })`
   (`SyncingConnectingMithrilPrompt.tsx:164-166`), so `{shortcut}` values are directly supported.
3. Nit: B1's expected post-CAT-A reading "…から Mithril Sync を開始できます" shows spaces around the
   token that CAT-A's in-place rule won't produce (`…からMithril Syncを開始できます`). Align the
   verify string so a literal check doesn't false-fail.
4. Verified clean: all five locale line cites exact in both files; 「シャットダウン」 and 「先端」
   occurrence sets are complete as claimed (renderer + main locales); accelerators `Command+D` /
   `Ctrl+D` at the cited lines; cheat-sheet macOS note present at `…smoke-test-cheat-sheet.md:194-195`.

### CAT-C — beyond R-1

1. **C1 is implement-ready** — locale/messages/defaultMessages cites all exact; no component hardcodes
   the label (i18n-only render path via `InlineProgressBar`).
2. **Add the spec updates to C1's plan:** `MithrilStepIndicator.spec.tsx:125,163,184,227,255,293,318`
   and `MithrilPartialSyncOverlay.spec.tsx:271` assert the old "fast sync" strings and will fail after
   the relabel. Do **not** touch `/fast sync with mithril/i` assertions
   (`MithrilProgressView.spec.tsx:116,170`, `MithrilBootstrap.spec.tsx`) — that's the feature title,
   CAT-A-out-of-scope.
3. C3 verified: snapshot side renders file counts (`:214-219`); no surface converts immutable chunk
   counts to bytes.
4. Nit: the ~47-pt swing is the *combined bar* (50 × 0.95); `snapshotPercent` itself swings 50.

### CAT-D — variant split + caution

1. **Threading list is missing two required files.** The dialog container does not render the section:
   `DaedalusDiagnosticsDialog.tsx:136-139` passes the flags to the presentational
   **`DaedalusDiagnostics.tsx`**, which declares them as props (`:411-414`), destructures (`:525-528`),
   computes `behindByEpochs` (`:571-575`), and renders `MithrilPartialSyncSection` (`:718-731`). The
   new `isAtOrPastSnapshot` prop must also be added to **`MithrilPartialSyncAvailability`**
   (`source/common/types/mithril-partial-sync.types.ts:89-101`) — the typed IPC payload the store
   consumes. Followed literally, the plan's file list compiles nothing through the middle hop and the
   new variant silently never fires. (Confirmed by both validators.)
2. **Acceptance contradicts the locked copy.** Acceptance bullet 1 still requires the surfaces to
   state "…that **Mithril Sync isn't needed**…" — exactly the flat claim the 2026-07-09 resolution
   dropped ("The copy drops the flat 'you don't need Mithril Sync'"; the LOCKED D1 strings contain no
   such sentence). Rewrite the bullet to match the locked strings, or a verifier will fail correct
   copy / an implementer may "fix" the copy to match the stale bullet. (The "What at/past means"
   intent paragraph carries the same pre-resolution phrasing; fine as history, but the acceptance
   checklist must match what ships.)
3. **Spec-plan corrections:** there is no `MithrilPartialSyncRecommendation.spec.tsx` (variant→copy is
   tested in `MithrilPartialSyncSection.spec.tsx:240,251`); conversely, adding `isAtOrPastSnapshot`
   to the probe's return breaks exact-shape `toEqual` assertions the plan doesn't name —
   `MithrilPartialSyncService.spec.ts:1565` and the behindness describe (`:2153-2250`), plus
   `MithrilPartialSyncStore.spec.ts` `_applyAvailability` coverage (`:656+`). See also the D↔F seam
   note below.
4. **D2 is a new element, not a message swap:** `renderSubItem` (`MithrilStepIndicator.tsx:560-591`)
   renders only icon + label today — the caution needs a new DOM element + SCSS class. Feasible;
   plan wording ("detail/subtext of the sub-item") slightly understates it.
5. Design notes confirmed in the plan's favor: the preferred explicit `isAtOrPastSnapshot: gap <= 0`
   return field is the right call — the `behindByImmutables`-absence fallback would misclassify
   probe-failed (and disabled/working short-circuits, `MithrilController.ts:151-160`, return no flag
   at all, so the store default must deliberately be `false`). "Behind by 0 epochs" cannot render
   (`computeBehindByEpochs` returns `undefined` for diff ≤ 0, `mithrilBehindness.ts:38-39`) — at
   `gap <= 0` the misleading message is `behindUnknown`, as the plan's "most likely" hedge said.
6. Line drifts (cosmetic): `getPartialSyncBehindness` is `:955-992` (gap branches `:969-981`);
   section ladder `:130-135`; confirmation select `:89-93`. Option-c rejection substrate fully
   confirmed (`handleCheckBlockReplayProgress.ts:15,53,59,65`; `NetworkStatusStore.ts:177-181`;
   gate `SyncingConnectingStatus.tsx:124-127`).

### CAT-E — button overflow

1. **Redirect the decline-button audit item:** it renders in
   `MithrilDecisionView.tsx:101-114`, styled by **`MithrilDecisionView.scss`** (`.actions:72-76`,
   `.secondaryAction:92-104`) — not "the `MithrilBootstrap` action row" (that file has no action row;
   its `failed` state reuses `MithrilErrorView`, which the main fix therefore covers for free).
2. **Invert primary/fallback technique.** `.primaryAction`/`.root` are equal specificity (0,1,0) on
   the same element (`ButtonSkin` composes both), so a class-level `height: auto` wins only by
   injection order — statically indeterminable. Setting the **custom properties on the module rule**
   (`--rp-button-height: auto; --rp-button-width: auto;`) wins deterministically over the
   `documentElement`-inherited theme values (`ThemeManager.tsx:24`). Make that the primary recipe;
   note the plan's step-1 example repeats `--rp-button-height` where the **width** var was meant.
3. **Surface inventory:** four scss modules independently restyle the same fixed-geometry skin with no
   height/wrap control — `MithrilErrorView.scss`, `MithrilDecisionView.scss`,
   `SyncingConnectingMithrilPrompt.scss:62-97` (adds `min-width: 180px`), and the confirmation's
   buttons, which render via the **`Dialog` widget's `actions` array**
   (`MithrilPartialSyncConfirmation.tsx:72-85`; `Dialog.scss:83-94` `width: 50%` + margins,
   `DaedalusDiagnostics.scss:466-472` `min-width: 180px`) — a different fix shape than the flex-row
   recipe. The plan's "only touch rows that actually overflow" stance is fine; the inventory above is
   what "visually verify" has to cover, with the `min-width`s interacting with any wrap recipe.
4. Nits: `.actions` already has `gap: 12px` (`MithrilErrorView.scss:75-79`) — add only `flex-wrap`;
   overrides also set background/border (which is the evidence the module CSS is injected after the
   theme, for what it's worth). Every cited line in the Diagnosis section is exact.

### CAT-F — beyond R-2

1. **F1: use the generated-artifact pipeline, and the list has a fifth JSON.**
   `translations/messages.json:2890-2899` carries the same dead descriptors and is absent from the
   removal list. Both it and `defaultMessages.json` are **generated** (`i18n:extract` /
   `i18n:check`, package.json:52-54; the repo rule says regenerate via `yarn i18n:manage`, don't
   hand-edit). Correct F1 procedure: remove the source descriptors
   (`MithrilBootstrap.messages.ts:392-403` — span ends at 403, not 402), the union member, the copy
   const `partialSyncErrorCopy.ts:15-18` + map entry `:56`, the spec tuple `:8-11`; then run
   `yarn i18n:manage` and let it drop the four JSON artifacts' entries. **Residual to verify at
   build:** whether the manager auto-deletes obsolete `en-US.json`/`ja-JP.json` keys or only reports
   them (702a's impl-review says it dropped removed ids; not re-executed here) — if report-only,
   delete `:366-367` in both by hand.
2. **F1 gate independently re-confirmed — DELETE is correct.** Repo-wide grep (source, storybook,
   tests) finds only the enumerated sites; no throw/`errorCode =`/dynamic construction;
   `derivePartialSyncRange` returns `{start: latest, end: latest}` (`mithrilPartialSyncPreflight.ts:134-143`);
   the live no-snapshot path throws `PARTIAL_SYNC_METADATA_UNAVAILABLE` (`MithrilPartialSyncService.ts:860-865`).
   Type-system note: the union member and `COPY_BY_CODE` entry are bound by
   `Record<MithrilPartialSyncErrorCode, …>` exhaustiveness — remove atomically or `yarn compile` fails.
   Wording nit: `derivePartialSyncRange` contains **no** throw at all; the
   `IMMUTABLE_POSITION_UNAVAILABLE` throw is in `resolveLocalImmutableNumber` (`:120-124`).
3. **F3 confirmed as planned**, two nits: keep the `!!!` prefix in the descriptor (locale values are
   stored stripped); inline placement matches the file's ~12 sibling `loading.screen.*` inline
   messages (the loading tree is mixed — `SyncingProgress.messages.ts` exists — so either home is
   conventional). `i18n:extract` alone writes `translations/messages.json`; `defaultMessages.json` /
   `en-US.json` come from `i18n:check` — i.e. `yarn i18n:manage`, then add the JA by hand.
4. **Acceptance names a nonexistent spec:** there is no `SyncingConnectingStatus` spec (only
   `SyncingConnectingMithrilPrompt.spec.tsx`, which doesn't reference the key). Drop or re-point that
   acceptance line.

### CAT-G — separator + race

1. **G2 trace correction (gates the guard placement).** The download spawn is **outside** the mutation
   lock: the lock closure returns at `chainStorageCoordinator.ts:260` and `handlers.start()` runs at
   `:264` in a separate post-lock IIFE. As worded, Work item 2 ("guard before `handlers.start()` …
   keep it inside the existing mutation lock") is self-contradictory. The workable placement is inside
   the lock closure, in/after `_ensureNodeStoppedForPartialSync` (the `:456-458` early-return on a
   click-time `null`/`STOPPED` snapshot is confirmed — that is the unguarded path; a RUNNING/STOPPING
   click *is* serialized via `await nodeStopHandler()` at `:469`, though only to process-exit).
2. **The "minimal defensive re-query" needs plumbing and won't itself close the race.** The
   coordinator has no node-state getter — `PartialSyncDependencies` (`:35-39`) would need a new
   `getNodeState` callback (live getter exists on the controller: `MithrilController.ts:170-172`).
   And a re-queried state already reads `STOPPED` while handles release (`CardanoNode._isDead`,
   `:935-936`, checks IPC-disconnect + process-exit only — confirmed) — so the re-query is
   race-window narrowing, not a fix. This matches the locked scope (reproduce-first, minimal guard,
   deep fix deferred), but the plan text should say the re-query is a placeholder so it isn't
   mistaken for the real guard when the follow-up is triaged.
3. **G1: add the second caller + second spec.** `MithrilDecisionView.tsx:52` also calls
   `getManagedChainDisplayPath` (display-only, benefits from the fix), covered by
   `MithrilBootstrap.spec.tsx:126,170` asserting `'/mnt/current-chain/chain'` — run that spec too,
   not just `ChainStorageLocationPicker.spec.tsx:76`. The full function (`chainStorageUtils.ts:98-110`)
   has a default-path fallback branch the fix must not touch.
4. G1 root cause confirmed (nit: `webpack.config.js:115` is `resolve.fallback`, not an alias —
   functionally identical); main-side resolver confirmed correct and untouched; coordinator spec
   exists with `startPartialSync` coverage incl. `nodeState: 'running'/'stopping'/'stopped'`
   (`chainStorageCoordinator.spec.ts:429,451`) — item 3's spec extension is feasible there.

### Master doc

1. **S2 attribution:** the "new optional caution id" belongs to **CAT-D** (`moveCaution`), not CAT-C
   (C reuses the existing `progressSnapshotSizeContext`). Disjointness verdict unaffected.
2. **S1 addendum:** CAT-F's extracted `loading.screen.mithrilSyncInterrupt` inserts **outside both
   blocks** (~line 391, between `loading.screen.loadingWalletData` and `loading.screen.pushingLedger`)
   — a fifth locale region; keep the EN↔JA insertion line-parallel. (Whole-file alignment holds
   today: both files 1516 lines, key-identical per line; block boundaries 156/178/297/386 are tight;
   `defaultMessages.json` confirmed not line-aligned.)
3. **Unlisted seam (D↔F): `MithrilPartialSyncService.spec.ts` behindness block.** CAT-D's probe-return
   change reshapes the same `toEqual` assertions CAT-F must turn green, and D's own new variant tests
   hit the same falsy-`pathExists` default F fixes. With A→G order, D lands before F — land R-2's
   top-level `pathExists.mockResolvedValue(true)` **with or before CAT-D's spec additions**, and let F
   verify rather than re-fix. (Also minor: CAT-E and CAT-G both touch `MithrilDecisionView` — scss vs
   a util call site; no real collision.)
4. Verified clean: S4/S5 regions disjoint as claimed; S6 path exists hop-for-hop; smoke-test cheat
   sheet has **no** replay-interrupt-button step and **all six** stale vocabulary terms at the cited
   lines; both JA copy docs predate the five newer keys (all absent there, all present in
   `en-US.json` today); `getPartialSyncBehindness` clean spans: method `:955-992`, `gap <= 0` branch
   `:970-976`, `behindByImmutables: gap` at `:979`.

---

## Not verifiable from the repo (unchanged from the plans' own flags)

- JA round-2 items (「台帳状態:」, the two D1 strings, the moveCaution JA, DD-706-1 term review) —
  external translator/product dependency.
- The G2 race reproduction and C2's runtime byte magnitudes / `bytesTotal` timing — runtime-only;
  both plans already prescribe the runtime step.
- Whether `react-intl-translations-manager` auto-deletes obsolete locale keys (see CAT-F item 1).
- Task-705 history cross-references (e.g. "commit 5") — mechanism confirmed in code; the historical
  attribution not re-checked.

## Suggested doc updates before build

1. CAT-C: rewrite C2 per **R-1** (decision: a/b/c) — this is the only redesign-level item.
2. CAT-F: swap the F2 fix location per **R-2**; add `translations/messages.json` + the
   `yarn i18n:manage` procedure to F1; fix the two acceptance nits.
3. CAT-D: add `DaedalusDiagnostics.tsx` + `MithrilPartialSyncAvailability` to the D1 file list; fix
   acceptance bullet 1 to the locked hedge copy; correct the spec inventory.
4. CAT-A: 30-key list; decide ownership of `mithrilPartialSyncNodeStartup.ts` dialog copy; fix the
   descriptor name; add the four spec files to acceptance.
5. CAT-B: decide the `menu.helpSupport.daedalusDiagnostics` JA label (extend B1 or log follow-up);
   prefer `global.environment.isMacOS`.
6. CAT-E: redirect the decline item to `MithrilDecisionView`; make the CSS-var override the primary
   technique; fix the width-var typo.
7. CAT-G: correct the G2 trace (spawn outside the lock) and guard placement; add the
   `MithrilDecisionView`/`MithrilBootstrap.spec` blast radius to G1.
8. Master: S2 attribution; S1 fifth-region note; add the D↔F spec seam with the ordering rule from
   item 3 above.
