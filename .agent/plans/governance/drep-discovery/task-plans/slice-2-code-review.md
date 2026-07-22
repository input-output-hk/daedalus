# Slice-2 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:` entry
> (required review pass over the PRD + guide), and per-task `Code Review:` entries.
> Companion docs: [slice-2-PRD.md](./slice-2-PRD.md) ·
> [slice-2-implementation-guide.md](./slice-2-implementation-guide.md)

---

## Planner: 2026-07-22 — slice-2 planning complete (status: in_review)

**Scope planned.** Three tasks, forced order 112 → 113 → 114, all classified
`autonomous` (none is in the locked non-autonomous set, and no blocking decision
survived planning). task-112 wires the in-app "Browse DReps" affordance into
`VotingPowerDelegation` (replacing the external gov.tools label link), adds the row-level
"Select for delegation" CTA to `DRepCard`, and carries the whole handoff through React
Router `location.state` via a new typed picker module
(`containers/governance/delegationFormState.ts`) consumed by the `withRouter`-wrapped
`VotingGovernancePage` and `DRepDirectoryPage`. task-113 widens the confirmation dialog
with `drepIdentity: DRepIdentity | null` and renders the full raw ID for drep targets.
task-114 is the in-slice verification: end-to-end byte-equality Jest, Storybook coverage
(global locale toggle), `!!!` copy audit, walkthrough de-GovTool-ing, compile/lint gates,
and the inherited sanitization floor (17/17).

**Orchestrator decisions applied.**
- **D1:** No Detail route/surface/CTA in production; task-112 AC-5 is met by production
  forwarding helpers (`pickDelegationFormReturnState`, reused by slice-4 later) plus a
  `DetailRouteStub` registered only inside
  `containers/voting/VotingGovernancePage.spec.tsx` with the path literal defined in the
  spec. `routes-config.ts`/`Routes.tsx` stay byte-identical. Planning confirmed this is
  implementable — no escalation needed.
- **D2:** gov.tools label link replaced with the in-app affordance; dead keys
  `drepInputLabelLinkUrl`/`…Preview` removed, and — planner extension P-1 — the equally
  dead `drepInputLabelLinkText` and `drepInputLabelPreprod` (plus the
  `environment.isPreprod`/`isMainnet` branches) removed too, since the in-app directory is
  network-agnostic. Walkthrough sweep scoped to this flow: 02 (:60/:62/:78/:84 — the
  brief's :88-90 anchor was stale, re-verified to :84), 04 (:17/:57/:140), 05 flow-routing
  lines only (:23/:204/:252; Lace note :33 untouched).
- **D3:** Dialog renders `drepIdentity.raw` only — full, monospaced, untruncated
  (`DRepIdDisplay` deliberately not reused: confirmation is a security surface and it
  truncates). No CIP-105 line, no signed-payload line, no source label, no name slot; the
  container's `credentialType` fill is a syntactic prefix check that never touches the
  rendered/submitted bytes. Recorded in the PRD as required.
- **D4:** `nix` unavailable → `node_modules/.bin/prettier --write` on changed
  `.ts/.tsx/.scss/.md` only; deviation goes into findings + PRD Final Outcome.

**Risks called out for the critique/build phases.**
1. **jsdom render friction** is the main execution risk: `VotingPowerDelegation` uses
   skinless react-polymorph `Input`/`Button`/`Link`, so the new specs wrap in the exact
   `ThemeProvider` from `StoryDecorator.tsx:25-29`, mock the two heavy dropdowns
   (per the `Governance.spec.tsx` mocking precedent), and locate the passphrase input
   structurally. If the Dialog portal still misbehaves, that is an implementation-time
   adaptation, not a plan change.
2. **Walkthrough gitignore deviation (P-5):** `.vscode/` is gitignored and absent from
   the worktree; task-114 copies the governance walkthroughs in, edits the copies, and the
   sync-back to the main checkout is recorded as a manual close-out step (worktree
   isolation forbids editing the main checkout).
3. **Un-synced return hop** lands on `VotingUnavailable` with the pre-fill parked in
   `location.state` — pinned as expected behavior for `ux-refinement`; tests cover the
   synced path only (brief §G-4).
4. **Copy quality:** ja-JP values are preliminary by design (`!!!`); the release-end
   review is user-owned and untouched.

**Verification contract:** `node_modules/.bin/tsc --noEmit` zero errors after every task
(clean baseline verified in this worktree); focused Jest via
`node_modules/.bin/jest --testPathPattern="…" --no-coverage --runInBand`; sanitization
suite 17/17 after every task; `yarn i18n:manage` whenever copy changes; one subject-only
commit per task.

Planning status set to `in_review` — awaiting the required Critiquer pass.

---

## Critiquer: 2026-07-22 — review pass over slice-2 PRD + implementation guide

**Scope reviewed.** `slice-2-PRD.md`, `slice-2-implementation-guide.md`, the
task-112/113/114 `acceptanceCriteria` in `governance-drep-discovery-plan-tasks.json`, and
the orchestrator grounding brief. Line anchors spot-checked against the live worktree
(and, read-only, against the main-checkout walkthrough originals) on 2026-07-22.

**What holds up (verified, not assumed).**
- AC mapping is complete: every acceptance criterion of task-112 (5), task-113 (3), and
  task-114 (8) traces to a concrete numbered guide step with quoted code and an explicit
  per-task acceptance checklist. task-112 AC-5 is satisfied per D1 (production
  `pickDelegationFormReturnState` + harness-only `DetailRouteStub`; `routes-config.ts` /
  `Routes.tsx` untouched; no "View details" CTA). task-113 follows D3 exactly (raw-ID-only,
  `drepIdentity: DRepIdentity | null`, sentinel labels kept, no-name regression test).
  task-114 covers the byte-equal payload end-to-end plus the `!!!` audit, walkthrough
  sweep, and gates.
- Invariants #1/#2/#4/#10/#11/#13 are stated inline at the exact steps that touch them;
  no query params, no store-backed pending state, no new logger/analytics calls anywhere
  in the quoted code; `VotingStore`/`GovernanceStore` byte-identical is a cross-cutting
  acceptance item.
- No hidden manual checkpoints: the one manual step (P-5 walkthrough sync-back to the
  gitignored main-checkout `.vscode/`) is explicit, justified by worktree isolation, and
  recorded in findings + Final Outcome requirements. The release-end `!!!` review stays
  user-owned and untouched.
- Anchor spot-checks all pass: `VotingPowerDelegation.tsx` (:85-93, :95-103, :104,
  :133-136, :233, :248-275, `environment` global :250/:264), `VotingGovernancePage.tsx`
  (:9, :25-33, :36-75, :80), `DRepCard.tsx` (:10-16, :18-21, :36-54, scss ends :41),
  `DRepDirectoryList.tsx` (:29-32, :34, :64-66), `DRepDirectory.tsx` (:48-55, :136),
  `DRepDirectoryPage.tsx` (:9-11, :32-47, :50), ConfirmationDialog props :53-68 /
  `mapVoteToIntlMessage` :30-39 / messages `vote` :9-13, en-US.json :878-892 + :284 +
  :880, ja-JP.json same keys :888-892 + :284, `DRepDirectory.spec.tsx` 25-per-page test
  :215-221 (`[class*="card"]` claim holds under jest-css-modules-transform local names),
  `Governance.spec.tsx` :41-59, `Governance.tsx` :4/:65 withRouter pattern, stories
  anchors (:54-55, :198-229, :262-306, :308, :392, :395, :429, :460; DRepDirectory
  stories :123-135), `StoryDecorator.tsx` ThemeProvider, `.gitignore` :135/:141, and all
  ten walkthrough line anchors including the guide's :84 correction of the brief's stale
  :88-90. The `VALID_DREP_ID` fixture matches the proven-valid stories constant, the
  dialog's password input is `type={'password'}`, `HwDeviceStatuses.READY` exists, and
  every JSX call site of the components gaining required props is in the guide's file
  lists (grep-verified — none missed).

**Blockers.**

1. **task-113 Step 4 — wrong quoted import path (compile-breaking).** The guide instructs
   adding, at the top of `source/renderer/app/containers/voting/VotingGovernancePage.tsx`:
   `import type { DRepIdentity } from '../../../common/types/governance.types';`
   Three `..` from `containers/voting/` resolves to `source/renderer/common/…`, which does
   not exist (`ls source/renderer/common` → no such directory). Repo precedent in the same
   containers tree uses four levels: `containers/status/DaedalusDiagnosticsDialog.tsx:4-6`
   and `containers/MenuUpdater/useMenuUpdater.ts:3` both import
   `'../../../../common/…'`. The quoted line must be
   `import type { DRepIdentity } from '../../../../common/types/governance.types';`
   As written, a builder following the guide literally cannot reach the mandated
   zero-error `tsc --noEmit` gate without deviating from quoted code — a contradiction
   the small-model bar does not allow. (The guide's other cross-boundary imports were
   checked and are correct: the dialog's five-level path matches the
   `DRepStatusBadge.tsx:4` precedent, and the storybook `'../../../source/common/…'` path
   resolves correctly.)

**Non-blocking notes (no plan change required; planner may fold into the same revision).**
- N-1: task-112 Step 3 (en-US.json) gives literal line numbers (change :888, delete
  :889-892) that are valid only before Step 3.1's insertion at :879. The preamble's
  "re-verify an anchor if the file was touched by an earlier step" rule plus the named
  keys disambiguate, but key-anchoring the en-US edits (as the ja-JP steps already are)
  or reordering (change/delete first, insert last) would remove the trap entirely.
- N-2: the dialog "vote paragraph block (:147-153)" range includes the
  `styles.content` div opener at :147; the actual replaced lines are :148-153. The quoted
  old-code block is exact, so the edit is unambiguous.
- N-3: Step 2's "keep the object keys of `messages` alphabetically ordered" — the file is
  not alphabetically ordered today; the operative instruction ("insert before
  `drepInputError`") is concrete and correct.
- N-4: directory-first entry (Select with no inherited wallet state): the pre-filled ID
  is silently wiped by `WalletsDropdown` onChange's `...initialState` reset when the user
  then picks a wallet. Outside slice-2's ACs (the round trip starts at the form), but
  worth a line in `slice-2-findings.md` for `ux-refinement`, alongside the un-synced
  return note.

Decision: requires_changes

---

## Planner: 2026-07-22 — fix pass (blocker resolution; status: approved)

**Blocker 1 (task-113 Step 4 wrong import path) — resolved.** The quoted import in
`slice-2-implementation-guide.md` Step 4 now reads
`import type { DRepIdentity } from '../../../../common/types/governance.types';`
(four levels, matching `containers/status/DaedalusDiagnosticsDialog.tsx:4-6` and
`containers/MenuUpdater/useMenuUpdater.ts:3`), with a level annotation added in the same
style as Step 2's, and the step prose reordered so the top-of-file import comes before
the callback-body edit. Verified against the live tree: from `containers/voting/`,
`../../../../common/types/governance.types.ts` resolves and the three-level form does
not. The guide's other two `DRepIdentity` imports (dialog five-level, storybook
`'../../../source/common/…'`) were re-checked and remain correct as the Critiquer found.

**Non-blocking notes N-1..N-4:** no plan text changed (per the Critiquer, none requires
a change). N-1's trap is covered by the preamble's re-verify rule plus key anchors; N-4
and the un-synced return hop remain slated for `slice-2-findings.md` at build time.

Planning status set to `approved` in `slice-2-PRD.md`. Slice-2 is cleared for build.

---

## Code Review: task-112 — round 1 — 2026-07-22

Reviewed the uncommitted worktree state against `slice-2-implementation-guide.md`
(task-112 section), the task-112 acceptance criteria in
`governance-drep-discovery-plan-tasks.json`, and the locked invariants. All gates were
re-run independently.

**Acceptance criteria**

- AC-1 (no second delegation backend): PASS. `VotingStore.ts`, `GovernanceStore.ts`,
  `routes-config.ts`, and `Routes.tsx` are byte-identical to base (`git diff` empty).
- AC-2 (VotingStore does not read GovernanceStore): PASS. No store file touched; no new
  imports of either store anywhere in the diff.
- AC-3 (list-row selection, no detail view): PASS. `DRepCard.tsx` gains only the
  "Select for delegation" `Button` (real `<button>` per shared-tokens §10); no
  "View details" CTA, no `DREP_DETAIL` literal in production (D1 honored).
- AC-4 (location.state-only handoff): PASS. `grep -rn selectedDRepId source/` hits only
  `delegationFormState.ts`, `DRepDirectoryPage.tsx`, `VotingPowerDelegation.tsx`, and the
  spec. No query params (`?…=` grep clean), no store-backed pending form state.
- AC-5 (two-hop coverage): PASS. `VotingGovernancePage.spec.tsx` registers the
  harness-only `DetailRouteStub`; both the Directory return hop and the
  Form → Directory → Detail → Form sequence use the production
  `pickDelegationFormReturnState` picker; wallet + vote type restored and ID pre-filled,
  asserted end-to-end.

**Invariants**

- #2 sanitization floor: no new `logger.*` / `analytics.*` / electron-store call in the
  diff (grep clean); sanitization suite re-run: 17/17 green.
- #10 byte-equality: `selectedDRepId` seeds `drepInputState.value` verbatim in the lazy
  `useState` initializer; `chosenOption` derivation untouched; no trim/normalization.
- #11 copy: 3 new/changed strings in BOTH locales, all `!!!`-prefixed
  (`browseDRepsLink`, `card.select`, changed `drepInputLabel`); `git diff` shows zero
  removed `!!!` lines in either locale; the four deleted `drepInputLabel*` keys are
  whole-key removals sanctioned by D2, with no stale references left
  (`gov.tools` / `environment` / removed-key greps all clean).
- #13 sentinels: Browse affordance lives inside the DRep input label, which renders only
  when `selectedWallet && selectedVoteType === 'drep'` (VotingPowerDelegation.tsx:260);
  directory rows are DReps only.

**Gates (run by reviewer)**

- `node_modules/.bin/tsc --noEmit` → exit 0, zero errors.
- `eslint` on all 13 touched files → 0 errors, 25 warnings (all pre-existing repo
  patterns: decorator-import false positives, unused-vars on function-type parameter
  names, inherited `@ts-ignore`).
- `jest --testPathPattern="VotingGovernancePage|DRepDirectory"` → 15/15 pass.
- `jest --testPathPattern="voting/Governance"` → 2/2 pass (no regression from the
  `withRouter` wrapping).
- `jest --testPathPattern="governance-sanitization"` → 17/17 pass.
- `yarn i18n:manage` → exit 0 and idempotent (regenerated files stable).

**Non-blocking observations**

1. prettier 2.1.2 cannot parse the inline `import { withRouter, type RouteComponentProps }`
   syntax in `DRepDirectoryPage.tsx`, `VotingGovernancePage.tsx`, and the new spec
   (exit 2, SyntaxError). This is a pre-existing condition: the committed slice-1
   `containers/voting/Governance.tsx` fails identically at base, and the guide prescribes
   copying that exact pattern. tsc/eslint/babel-jest all handle it. Consequence: the D4
   `prettier --write` substitute step cannot format these three files — record in
   `slice-2-findings.md` so the pre-merge `nix fmt` run covers them.
2. `waitFor` is omitted from the spec's testing-library import — explicitly sanctioned by
   the guide's parenthetical; task-114 must add it with the appended payload test.
3. prettier 2.1.2 reformat drift rode along in `Governance.stories.tsx`
   (`(STAKE_POOLS as unknown) as …`, `Record<…>` reflow) and `VotingPowerDelegation.tsx`
   (`(typeof messages)[…]` → `typeof messages[…]`, semantically identical) — expected
   under D4 given the known HEAD-drift situation; acceptable to ride with the commit.
4. `translations/messages.json` regeneration re-added unrelated
   `daedalus.diagnostics.dialog.*` descriptor hunks — tool-managed output of
   `yarn i18n:manage`, rides with the task commit per convention.
5. Directory-first entry edge (Select with no inherited wallet state → `WalletsDropdown`
   onChange resets to `initialState`, wiping the pre-filled ID) was already logged by the
   Critiquer as N-4 for `ux-refinement`; unchanged by this implementation and outside
   task-112's ACs.

No blockers found. Implementation matches the guide step-for-step, including the D1/D2
scope boundaries.

Decision: approved

---

## Code Review: task-113 — round 1 — 2026-07-22

Reviewed the uncommitted worktree state against `slice-2-implementation-guide.md`
(task-113 section), the task-113 acceptance criteria in
`governance-drep-discovery-plan-tasks.json`, and the locked invariants. All gates were
re-run independently.

**Acceptance criteria**

- AC-1 (prop contract widens to `DRepIdentity`): PASS.
  `VotingPowerDelegationConfirmationDialogProps` gains `drepIdentity: DRepIdentity | null`
  with the type imported from `common/types/governance.types` via the corrected
  five-level path (matches the guide's post-fix quoted code; container uses the
  four-level path per the Planner fix pass — both resolve, tsc exit 0).
- AC-2 (renders the selected DRep ID instead of the generic label): PASS. For a non-null
  `drepIdentity` the dialog renders `messages.drepId` + `<code>{drepIdentity.raw}</code>`
  (full, monospaced, `word-break: break-all` — deliberately not the truncating
  `DRepIdDisplay`, per D3). The spec's negative assertion uses the exact finalized
  literal `Delegate to DRep (default)` (verified against en-US.json:889 — not vacuous).
- AC-3 (name slot reserved for anchor-2): PASS. Only `drepIdentity.raw` is read from the
  identity; the "never renders a name field" regression test injects a `givenName` and
  asserts it does not render.

**Invariants**

- #10 byte-equality: the container builds `raw: chosenOption` verbatim (sentinels → null;
  `credentialType` is a prefix classification that never touches the string); the dialog
  renders `drepIdentity.raw` with no trim/normalization; `onSubmit` still passes the same
  `chosenOption` into `voting.delegateVotes`. `VotingStore.ts` byte-identical to base.
- #13 sentinels: `abstain` / `no_confidence` map to `drepIdentity: null` and keep their
  label rendering via the untouched `mapVoteToIntlMessage`; both pinned by passing tests.
- #2 sanitization floor: no new `logger.*` / `analytics.*` / electron-store call anywhere
  in the diff (grep clean); suite re-run 17/17 green.
- #4 handoff: this task adds no navigation, no query params, no store reads;
  `GovernanceStore.ts`, `routes-config.ts`, `Routes.tsx` byte-identical to base.
- #11 copy: one new key `voting.governance.confirmationDialog.drepId` = `!!!DRep ID` in
  BOTH locales, correctly alphabetized after `button.confirm`; zero removed `!!!` lines
  in the diff; `defaultMessages.json` + `translations/messages.json` regenerated
  consistently (5-line insertions each).

**Gates (run by reviewer)**

- `node_modules/.bin/tsc --noEmit` → exit 0, zero errors.
- `eslint` on the five touched .ts/.tsx files → 0 errors, 12 warnings (pre-existing repo
  patterns plus the guide's own `as any` spec fixtures).
- `jest --testPathPattern="VotingPowerDelegationConfirmationDialog"` → 4/4 pass.
- `jest --testPathPattern="VotingGovernancePage|DRepDirectory"` → 15/15 pass (no
  regression from the container callback-body change).
- `jest --testPathPattern="governance-sanitization"` → 17/17 pass.
- `prettier --check` on all touched .ts/.tsx/.scss → clean (the task-112 round's
  inline-`type`-import parse failure is gone: the container now uses separate
  `import type` statements, which prettier 2.1.2 parses).
- `yarn i18n:manage` → exit 0 and idempotent (no further tree changes).

**Non-blocking observations**

1. prettier 2.1.2 reformat drift rode along in the dialog
   (`(typeof messages)[…]` → `typeof messages[…]`, `useState<…>` generic reflow) —
   semantically identical, expected under D4.
2. The walkthrough claim "the specific DRep ID is not displayed in the dialog"
   (02-voting-power-delegation.md) is now false after this task, but its rewrite is
   explicitly task-114 Step 3 scope — recorded here so it is not lost, not a task-113
   defect.
3. The dialog itself does not assert `drepIdentity.raw === chosenOption`; the guarantee
   lives at the single production call site (container builds `raw: chosenOption`
   verbatim) and the task-114 end-to-end payload test pins the full path — consistent
   with the guide's design.

No blockers found. Implementation matches the guide step-for-step, including the D3
scope boundary (raw-ID-only, no CIP-105 line, no signed-payload line, no source label).

Decision: approved
