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
