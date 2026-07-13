# task-ux-402 — Plan Review Log

Append-only. Newest entries at EOF.

---

**Planner:** 2026-06-26T10:42:35Z

Authored `task-ux-402.md` (planning status `in_review`, build `in_progress`, interaction mode `autonomous`) to the small-model-implementable bar: exact files, ordered mechanical steps, LIVE line anchors, exact prop/type/i18n-key names, inline locked invariants, and per-testCase verification commands. Mirrors the structure of the shipped `task-ux-401.md`.

**Committed implementation approach (4 affordances, all reusing proven seams):**
1. **Rotating active step** — swap the static `.activeCircle` for the existing `.iconSpinner`/`loading-spin` in `MithrilStepIndicator.TopLevelIcon`. Pure TSX swap; reduced-motion already covered.
2. **Ticking elapsed** — add `@observable startedAt` to `MithrilPartialSyncStore`, anchored byte-for-byte like `MithrilBootstrapStore.bootstrapStartedAt` (anchor on first working frame, honor backend `elapsedSeconds`, release on `idle`, keep through terminal so elapsed freezes). Thread store→`App.tsx`→overlay→`MithrilProgressView` as `bootstrapStartedAt`; drop the frozen `elapsedSeconds` pass-through at the overlay. Renderer clock only — no IPC, no synthetic figure (locked #8/#18).
3. **Long-phase reassurance** — `LONG_RUNNING_STATUSES` {verifying,unpacking,converting,installing,finalizing} → one `aria-live` line under the timer + `.reassurance` style in `MithrilProgressView.scss`.
4. **Populated stopping-node frame** — map `stopping-node → preparing` in `STATUS_TO_STEP` (active spinning step, not greyed) + an `isStoppingNode` block mirroring `isStartingNode`.

**Anchor reconciliations (every TASK_CONTEXT/PRD anchor drifted; components grew during task-ux-401):**
- `.activeCircle` active branch: JSON `386-388` → **LIVE 403-405**.
- spec lock: JSON `59-60` → **LIVE 61-62** (test 38-64); flipped to expect `.iconSpinner` not-null / `.activeCircle` null.
- bootstrap ticking effect: `97-109` → **LIVE 93-111** (confirmed the proven path).
- `isStartingNode` block: `142-163` → **LIVE 145-166**.
- frozen `elapsedSeconds` prop: `MithrilPartialSyncOverlay.tsx:93` → **CONFIRMED exact**.
- **Material scope reconciliation:** the JSON listed `MithrilStepIndicator.scss` as a target, but `.iconSpinner` (scss L71-77) already exists AND the reduced-motion block (scss L306-311) already names it — so **no scss edit to MithrilStepIndicator.scss** is needed; the spinner is a pure TSX swap. The only scss edit is a new `.reassurance` rule in `MithrilProgressView.scss` (not in the JSON list; rationale documented, mirroring task-ux-401 touching `MithrilBootstrapPage.tsx`/storybook beyond the JSON list when the seam required it). `App.tsx` is likewise added (overlay mount point).

**i18n keys chosen (first-class EN+JA, no `!!!` in runtime catalogs):**
- `loading.mithrilBootstrap.progress.longPhaseReassurance` — EN "This can take several minutes — Daedalus is still working." / JA "数分かかることがあります。Daedalusは引き続き処理しています。"
- `loading.mithrilBootstrap.progress.nodeStoppingTitle` (+ partial-sync `loading.mithrilPartialSync.progress.nodeStoppingTitle`) — EN "Stopping Cardano node..." / JA "Cardanoノードを停止しています..."
- `loading.mithrilBootstrap.progress.nodeStoppingDetail` — EN "Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes." / JA "Daedalusは検証済みのチェーンデータを復元するために、Cardanoノードを停止しています。数分かかることがあります。"
- `loading.mithrilPartialSync.progress.nodeStoppingDetail` — EN "Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes." / JA "Daedalusは検証済みのMithrilチェーンデータを復元する前に、Cardanoノードを停止しています。数分かかることがあります。"
Live i18n mechanism confirmed: `defineMessages` in `MithrilBootstrap.messages.ts` (`!!!` defaults) → `yarn i18n:extract` writes only `translations/messages.json` → hand-edit `en-US.json`/`ja-JP.json` (key-sorted) → `yarn i18n:check`.

**Locked invariants stated inline:** #11 (no bootstrap regression — verified inert for `stopping-node`/`completed`/optional props + re-run guards), #8/#18 (renderer clock, no IPC/synthetic), reduced-motion (uses the already-guarded `.iconSpinner`), #4 (subtitle untouched).

**Residual open questions (non-blocking):** (a) the big `completionSpinner` is pre-existing-unguarded for reduced-motion — flagged for 601/502, out of scope; (b) two bootstrap-scoped `nodeStopping*` defaults are likely dead (bootstrap never stops a node) — kept for `isStartingNode` symmetry, 501 may prune; (c) the tick test depends on Jest 27 modern fake timers mocking `Date.now()` (confirmed present in repo).

---

**Critiquer:** 2026-06-26T10:53:20Z

One consolidated pass against the live tree, tasks JSON, PRD D4/D8#19, and the locked invariants. Verdict: **requires_changes**. The four-affordance approach is the right smallest-truthful change and reuses the proven seams; nearly every anchor in the live-seam table re-verifies (`.activeCircle` TopLevelIcon L403-405; spec lock L61-62; `isStartingNode` block L145-166; overlay `elapsedSeconds=` L93; ticking effect L93-111; store `_updateStatus` L146-166; `bootstrapStartedAt` anchor mirror in `MithrilBootstrapStore` L129-137; `stopping-node` is a working status so the store anchor fires from 0:00 on the optimistic frame; `isMithrilPartialSyncRestoreCompleteStatus` already forces all top-level steps to `completed` during `completed`/`starting-node`, so the spinner swap never lights those). i18n mechanism confirmed (`defineMessages` → `i18n:extract` writes only `messages.json` → hand-edit `en-US`/`ja-JP`, runner manages those two languages). Reduced-motion guard for `.iconSpinner` is real (scss L306-311), so the pure-TSX swap is correct and `MithrilStepIndicator.scss` truly needs no edit. **But two factual errors in the plan's regression/test analysis are blockers — both touch the SHARED component and one is locked invariant #11, the task's stated top risk.**

**BLOCKER 1 — `MithrilPartialSyncOverlay.spec.tsx` WILL break; plan lists it as "no edits expected" (Step 10d) and runs it in its own verification command.** That spec's first test (`MithrilPartialSyncOverlay.spec.tsx:42-49`) renders `transferProgress={{ …, elapsedSeconds: 65 }}` and asserts `screen.getByText('1:05')` (L48). The plan deletes the overlay's `elapsedSeconds={transferProgress?.elapsedSeconds}` pass-through (Step 4c) and the test passes no `startedAt`, so `MithrilProgressView` gets `elapsedSecondsProp=undefined` + `bootstrapStartedAt=undefined` → renders `0:00`, not `1:05` → the assertion fails. The plan's claim ("sets elapsedSeconds: 65 … but never asserts the timer; the unused field is benign") is wrong. Fix: add `MithrilPartialSyncOverlay.spec.tsx` to the edited-files set and update that test (pass `startedAt: Date.now() - 65_000` and keep `1:05`, or drop the timer assertion). Location: `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx:42-49` + plan §Step 10d / §Verification.

**BLOCKER 2 — adding `'completed'` to `TERMINAL_STATUSES` (Step 5a) is NOT inert for bootstrap; the #11 safety rationale is false.** Bootstrap renders the SHARED `MithrilProgressView` with `status==='completed'`: `MithrilBootstrap.tsx:67-76` includes `'completed'` in `WORKING_STATUSES` and L190-204 renders `MithrilProgressView` for it (corroborated by `MithrilProgressView.spec.tsx:84,106` which exercise `status:'completed'`). `MithrilBootstrapStore` keeps `bootstrapStartedAt` non-null through `completed` (it's only cleared on decision-cycle or `preparing`-after-terminal), so TODAY the elapsed timer ticks on the bootstrap completed screen; after Step 5a it FREEZES. The plan's "bootstrap never sits on a completed MithrilProgressView … inert for bootstrap (#11)" is factually wrong. The actual change is plausibly benign/desirable (completed is transient → `starting-node` resumes ticking), but #11 is the task's explicit top risk and cannot rest on a false premise. Fix: correct the rationale to state the freeze DOES apply to bootstrap's completed frame, explicitly accept it (or scope the freeze to partial-sync only), and make the `MithrilBootstrap.spec.tsx` re-run actually cover the completed-screen timer. Location: `MithrilProgressView.tsx:43-45`, `MithrilBootstrap.tsx:67-76,190-204`, plan §Step 5a / §Risks ("`completed` … inert for bootstrap").

**MINOR a — generated i18n artifacts not staged.** `yarn i18n:check`/`i18n:manage` runs `react-intl-translations-manager`, which regenerates `source/renderer/app/i18n/locales/defaultMessages.json` (and may touch `whitelist_en-US.json`/`whitelist_ja-JP.json`) with the 5 new ids. The plan edits only `messages.json` + `en-US`/`ja-JP` and never mentions committing the regenerated `defaultMessages.json` (task-ux-304 listed it as an explicit target). Add it to the staged set / verification.

**MINOR b — `App.spec.tsx` store mock.** The plan's "no edits" claim holds here (overlay is mocked at `App.spec.tsx:109-112`; prop assertion uses `expect.objectContaining`, L236-246, so a new `startedAt` is tolerated). Optional polish: add `startedAt` to the `mithrilPartialSync` store mock (L144/L201) for fidelity; non-blocking.

**Coverage/altitude:** D4 UX6/UX7 + D8#19 and gaps #5/#19/#36/#37/#38 are fully addressed; the `MithrilStepIndicator.spec.tsx:61-62` lock-flip is planned; no synthetic throughput/ETA/overall-% and no new IPC (#8/#18 preserved — elapsed is a renderer clock off a store timestamp); `disabled={isStartingNode}`/`hideAction` left to task-ux-404; #4 subtitle untouched. Detail is concrete and small-model-implementable. Resolve the two blockers (and ideally the two minors) and this is ready.

Decision: requires_changes

---

**Planner:** 2026-06-26T10:59:31Z

Single fix pass (cap = one). All four critique items resolved in `task-ux-402.md`; verified each against the live tree before editing. The four-affordance approach, seams, anchors, and i18n keys are unchanged — only the regression/test analysis and rationale were corrected (smallest truthful change preserved).

**BLOCKER 1 (overlay spec WILL break) — FIXED.** Verified against live `MithrilPartialSyncOverlay.spec.tsx:18-49`: the default `transferProgress` carries `elapsedSeconds: 65` (L21) and the first test asserts `getByText('1:05')` (L48); Step 4c deletes the overlay pass-through, so without a `startedAt` the view renders `0:00` and the assertion fails. Replaced Step 10d: promoted `MithrilPartialSyncOverlay.spec.tsx` from "re-run, no edits expected" to **EDIT REQUIRED**. The fix adds `startedAt={Date.now() - 65_000}` to the `renderComponent` default props and drops the now-dead `elapsedSeconds: 65` from the default `transferProgress`, keeping `1:05` — which now proves the new overlay→`MithrilProgressView` `startedAt`→`bootstrapStartedAt` threading end-to-end (real timers fine; the synchronous `tick()` computes 65 on mount). Added a code snippet and an inert-for-other-tests note. The spec was already in the `yarn test:jest` verification list.

**BLOCKER 2 (`'completed'`→`TERMINAL_STATUSES` not inert for bootstrap; #11 rationale false) — FIXED.** Verified live: `completed` is in `MithrilBootstrap.tsx:67-76` `WORKING_STATUSES` and L190-204 renders the shared `MithrilProgressView` for it with `bootstrapStartedAt`; `MithrilBootstrapStore.ts:119-148` keeps `bootstrapStartedAt` non-null through `completed` (cleared only on decision-cycle or `preparing`-after-terminal); `MithrilProgressView.tsx:99` freezes when `TERMINAL_STATUSES.has(status)`. So the bootstrap completed frame ticks today and WILL freeze. Rewrote Step 5a's rationale to state this truthfully and **explicitly accept** the freeze (success frame should not climb; `completed` is transient → `starting-node` resumes the live tick from the same anchor). Corrected both Risks bullets ("Bootstrap regression (#11)" and "`completed` keeps ticking?") which previously claimed bootstrap "never sits on a completed `MithrilProgressView`". Added a Step 10b `MithrilProgressView.spec.tsx` completed-freeze test (renders the exact shared component with `status:'completed'` + `bootstrapStartedAt`, advances fake timers 5s, asserts elapsed stays `0:00` and never reaches `0:05`) as the bootstrap completed-frame timer proof; noted `MithrilBootstrap.spec.tsx:207-213` stays the integration regression (passes no anchor → `0:00`).

**MINOR a (regenerated defaultMessages.json not staged) — FIXED.** Added Step 9d: `yarn i18n:check`/`i18n:manage` (react-intl-translations-manager) regenerates `source/renderer/app/i18n/locales/defaultMessages.json` (and may touch the empty `whitelist_en-US.json`/`whitelist_ja-JP.json`) with the 5 new ids — commit it alongside `messages.json`+`en-US`+`ja-JP`, do not hand-edit. Added two verification commands (`git status --porcelain …/defaultMessages.json` and a `grep -c` that expects 5 — confirmed against the live catalog's `"id": "…"` line format, matching the existing 4 nodeStarting + elapsedLabel pattern).

**MINOR b (App.spec.tsx store mock fidelity) — ADDRESSED (optional).** Confirmed the "no edits" claim holds (overlay mocked `App.spec.tsx:109-112`; `expect.objectContaining` L236-246 tolerates a new `startedAt`). Added an optional-fidelity note in Step 10e to add `startedAt` to the idle (L144-162) and failed (L201-219) `mithrilPartialSync` mocks; marked non-blocking.

No scope/seam/anchor changes; locked invariants #8/#18/#11/#4 and reduced-motion handling unchanged. Plan remains small-model-implementable.
