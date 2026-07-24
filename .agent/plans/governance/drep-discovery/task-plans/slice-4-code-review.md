# Slice-4 Code Review Log

> Append-only. Entries are added in chronological order: Planner (planning),
> Critiquer (planning review), Code Review (per-task implementation review).
> Never rewrite or delete an earlier entry.

---

## Planner: slice-4 planning — 2026-07-24

**Scope planned.** Two autonomous tasks. task-116 builds the on-chain-only DRep detail:
the `components/governance/drep-detail/` tree (`DRepDetail`,
`DRepDetailOnchainSection`, `DRepDetailAnchorSection`, `DRepDetailActions`), a
`DRepDetailPage` container replicating the directory's refresh contract (refresh on
Idle/Failed + refetch-at-tip reaction), two shared-component extensions
(`DRepSourceLabel` gains the `on-chain-anchor-reference` variant; `DRepIdDisplay` gains
an opt-in aria-live copied confirmation), 21 new `!!!` keys per locale, an 11-test
container spec, and five Storybook stories under the global locale toggle. task-117
wires production: `GOVERNANCE.DREP_DETAIL` literal, `Routes.tsx` detail `TrackedRoute`
plus the mandatory `exact` on the directory route, the card's "View details" CTA
threaded to a directory push that forwards `pickDelegationFormReturnState` state,
`maskAnalyticsRoute` applied inside `MatomoClient.getAnalyticsURL`, a 3-test extension
of the sanitization floor suite (20 → 23), the `DetailRouteStub` → production migration
in `VotingGovernancePage.spec.tsx` (nine permitted edits, slice-2/3 pins otherwise
byte-identical), and `research/slice-4-findings.md`.

**Decisions recorded.** The twelve pre-resolved orchestrator decisions (D1–D12) are in
the PRD with grounding — notably: vote positions deferred with a graceful unavailable
state (plan-internal `gov-state` conflict recorded); the analytics URL leak the new
id-bearing route would create is closed at the single `getAnalyticsURL` boundary; the
favorite toggle and "Registered: epoch N" row do not ship; CIP-129-only ID display;
anchor URL + hash as non-interactive text under "On-chain anchor reference". Planner
resolutions P-1…P-11 close the remaining judgment calls: the un-pre-assigned
anchor-reference label id (`governance.drepDetail.sourceLabel.anchorReference`), the
timer-free inline copy confirmation, single provenance treatment per section, the
detail render-state mapping, the duplicate-message-id reuse of `card.select`
(repo precedent), explicit `LinkSkin` on the back link, state-preserving back
navigation, the pure mask module + regex shape, the inactive-DRep expiry rule, the
spec-local detail path literal in task-116, and the deferred focus-management nicety.

**Live-code verification.** All anchors re-verified against worktree base `d5e3a03f2`
on 2026-07-24. Drift found and folded into the docs: the harness routes sit at
`VotingGovernancePage.spec.tsx:165-175` (not 169-174); the directory refresh seam spans
`DRepDirectoryPage.tsx:24-49`; `pickDelegationFormReturnState` at
`delegationFormState.ts:50-62`; the sanitization suite is already 20 tests (slice-2
docs said 17; slice-3 added 3 — re-run green 20/20 during planning); shared-design-tokens
§9 pre-assigns no id for the "On-chain anchor reference" label.

**Known risks.** (1) The stub-migration diff is the most delicate surface — the guide
whitelists exactly nine edits and requires the two-hop test's final assertions to stay
byte-identical; reviewer should diff-check that file first. (2) Shared-component edits
(`DRepIdDisplay`, `DRepSourceLabel`) touch every directory card — the confirmation prop
defaults off and the `'on-chain'` variant is unchanged, with the directory suites
re-run in task-116's gate. (3) The Matomo boundary test depends on module-mock
mechanics (`matomo-tracker` constructor mock + jsdom hash); the guide pins the exact
setup and why it needs no further scaffolding. (4) Storybook cannot be launched in this
devcontainer — stories are verified at tsc/eslint level only; eyeball both locales via
the global toggle before merge (standing deviation, slice-2 precedent). (5) prettier
2.1.2 may rewrap pre-existing drift hunks in touched files (slice-3 I-4) — expected,
formatting-only.

**Status:** PRD + implementation guide written to the small-model bar; planning status
`in_review`, awaiting the Critiquer pass.

---

## Critiquer: slice-4 planning review — 2026-07-24

**Pass scope.** One broad pass over `slice-4-PRD.md` + `slice-4-implementation-guide.md`
against `prompt.md` (locked invariants, doc requirements, small-model bar), the tasks
JSON (task-116/117 acceptance + dependencies), the README binding scope, both design
docs, and the live worktree code. Every quoted seam was spot-checked against the files;
the sanitization floor suite and `DRepDirectory.spec.tsx` were re-run during review.

**What holds up.** Coverage is complete: all six task-116 acceptance criteria and the
task-117 state-forwarding AC map to concrete steps and named tests; the analytics URL
mask + 3-test floor extension (D2/P-8), the mandatory `exact` fix (D8), the
nine-edit stub migration with slice-2/3 pins whitelisted (D10), anchor-as-inert-text
(D7), the graceful deferrals (D1 vote positions, D3 favorites, D4 registration epoch),
`!!!` markers on all 22 new strings (21 + `card.viewDetails`), and the global-toggle
Storybook are all specified mechanically with full file bodies. Invariant handling is
sound — notably the D2 mask closes a leak the new route would otherwise create, and
`MatomoClient.getAnalyticsURL` is verified as the single URL-embedding boundary
(`TrackedRoute` uses `window.location.hash` only for `matchPath`, nothing sent).
Anchors verified accurate: `routes-config.ts:39-42`, `Routes.tsx:42/:226-239/:233-237`,
`MatomoClient.ts:61-63/:39/:57`, `DRepIdDisplay.tsx` seams (:1/:10-21/:23-26/:38/
:47-52/:64-70), `DRepCard.tsx` seams (:19-23/:36-41/:90/:92-98), stub at
`VotingGovernancePage.spec.tsx:74-99` + stub route at :174, `delegationFormState.ts`
picker, `Governance.tsx:48-51`, en-US/ja-JP `card.select` both at :284 with correct
alphabetical insertion, jest `globals.environment` at jest.config.js:63-67, story
seams :136-153/:352-366. Deleting `act`/`RouteComponentProps`/picker imports in Step 12
is safe (all remaining uses live in the deleted stub/replaced test — grep-verified).
The 20/20 floor baseline claim re-verified green during this review. No hidden manual
checkpoints beyond the already-acknowledged Storybook eyeball deviation.

**Blockers (3).**

- **B-1 — Guide (task-116 Steps 4/7/11): duplicate status badge breaks the guide's own
  first test.** `DRepDetail.tsx` renders `DRepStatusBadge` in the header AND
  `DRepDetailOnchainSection` renders it again in the Status field row;
  `DRepStatusBadge` emits a visible `!!!Active` label each time, so
  `screen.getByText('!!!Active')` in the "renders the on-chain fields" test throws
  "Found multiple elements". The detail wireframe (`drep-discovery-design.md:84-94`)
  shows status only inside the On-chain box, not in the header. Fix: render the badge
  once — drop it from the `DRepDetail` header (keep the Status row) and align PRD FR-6,
  the architecture diagram line, and the Step 7 body; the P-3 single-provenance
  rationale extends naturally to the status treatment.
- **B-2 — Both docs: every `npx` verification command fails in this devcontainer.**
  npm 11.13.0 rejects the repo's string-form `package.json` `devEngines`
  (`"node": ">=v22.0.0"`), so `npx tsc/eslint/jest/prettier` all exit with
  `npm error Invalid property "devEngines.node"` before the tool runs (reproduced
  during this review). Verified working substitutes: `node_modules/.bin/tsc` (4.9.5),
  `node_modules/.bin/eslint` (8.13.0), `node_modules/.bin/jest` (27.5.1 — floor suite
  20/20 green via this path), `node_modules/.bin/prettier` (2.1.2), and `yarn <tool>`.
  Fix: swap the invocations in the guide's cross-cutting verification block and Steps
  13/15 (and the PRD's NFR-5/NFR-6 + DoD wording) to the `node_modules/.bin/` or
  `yarn` forms; record the stale `npx` convention in `slice-4-findings.md` for future
  slices.
- **B-3 — Guide (task-117 Step 15): wrong expected test count for
  `DRepDirectory.spec.tsx`.** The live spec has 19 tests (re-run during review: 19
  passed), so the post-edit expectation is **20 (19 + 1)**, not "21 (20 + 1)". A
  literal implementer would hunt for a phantom missing test at the gate.

**Minor notes (no fix required to approve).**

- `DRepDetail.tsx` imports `AppDRepDirectoryEntry` in a value-import list; repo
  precedent (`DRepCard.tsx:9`) uses a separate `import type`. tsc/jest tolerate the
  mixed form; suggest `import type` when applying B-1's edit to the same file.
- Trivial off-by-one anchors, harmless because the exact code is quoted: `buildStores`
  spans :106-142 (guide says :107-143); `DRepSourceLabel.tsx` is 28 lines (guide says
  29).
- The copy-confirmation spec resolves `setCopied` in a microtask; `findByText` handles
  it but an act() warning may appear in the log (known F-6 noise) — not a failure.

**Decision: requires_changes** — three mechanical fixes (B-1 component/test/PRD
alignment, B-2 command substitution, B-3 count correction); no scope, invariant, or
sequencing changes needed. One Planner fix pass should clear all three.

---

## Planner: slice-4 fix pass — 2026-07-24

Applied all three Critiquer blockers; no scope or sequencing change.

- **B-1 (duplicate status badge):** `DRepStatusBadge` now renders exactly once, in
  `DRepDetailOnchainSection`'s Status row (the wireframe's placement,
  `drep-discovery-design.md:84-94`). Guide Step 7's `DRepDetail.tsx` body drops the
  header badge and its import; Steps 4/7/11 notes state the single-instance rule so
  `screen.getByText('!!!Active')` stays unique; PRD FR-6 and the architecture-diagram
  header line updated to match. Per the minor note, `AppDRepDirectoryEntry` in Step 7
  moved to a separate `import type` (precedent `DRepCard.tsx:9`).
- **B-2 (broken `npx`):** every `npx` invocation in the guide (cross-cutting
  verification block, Steps 13 and 15, both AC lists, cross-cutting acceptance) and the
  PRD (NFR-5, NFR-6, DoD) swapped to the verified `node_modules/.bin/<tool>` forms
  (tsc 4.9.5, eslint 8.13.0, jest 27.5.1, prettier 2.1.2) or `yarn <tool>`; the
  cross-cutting block explains the `devEngines` failure so an implementer never
  misreads it as a code error; Step 14 now requires the findings note to record the
  stale `npx` convention for later slices.
- **B-3 (wrong count):** Step 15 expected counts corrected to
  `DRepDirectory` 20 (19 + 1) — live spec re-verified at 19 tests.

**Status:** PRD planning status set to `approved`; slice-4 ready for implementation.

---

## Code Review: task-116 pass 1 — 2026-07-24

**Scope.** One broad pass over the staged (uncommitted) task-116 diff against the
implementation guide and PRD: 8 new files (four `drep-detail/` components +
`DRepDetail.scss`, `DRepDetailPage` container + spec, `DRepDetail.stories.tsx`),
3 shared-file edits (`DRepSourceLabel`, `DRepIdDisplay` + its `.scss`), both locale
JSONs, and the tool-managed `defaultMessages.json` / `translations/messages.json`
regeneration.

**Verified during review (re-run, not taken on trust).**

- `node_modules/.bin/tsc --noEmit` — exit 0, zero errors.
- Scoped eslint on all touched `.ts/.tsx` — 0 errors, 9 warnings, every warning in a
  pre-existing baseline class (decorator `observer`/`inject` false positives, harness
  `as any`, spec non-null assertion, callback param name).
- `DRepDetailPage.spec.tsx` 11/11 green; sanitization floor 20/20 green (suite file
  byte-identical to base); directory regression suites 22/22 green (covers both edited
  shared components).
- AC-1 grep for `onExternalLinkClick|<a |href=` over `drep-detail/` — empty; the
  container spec additionally pins `.closest('a')` null on the anchor URL.
- No `logger.*`/`analytics.*` call anywhere in the new code; the only logger lines are
  the pre-existing `drepIdLength` ones in `DRepIdDisplay`, re-indented only.
- Protected files byte-identical to base: `routes-config.ts`, `Routes.tsx`,
  `VotingStore.ts`, `GovernanceStore.ts`, `DRepCard.tsx`, `MatomoClient.ts`,
  `VotingGovernancePage.spec.tsx`, the sanitization suite — so every slice-2/3 pin
  survives untouched, and the route/CTA/analytics-mask surface is cleanly left to
  task-117.
- i18n: 20 `governance.drepDetail.*` + `governance.drepDirectory.backToDirectory` = 21
  keys per locale, all `!!!`-prefixed, correctly sorted before
  `governance.drepDirectory.card.select`; the message-JSON regeneration adds exactly the
  new descriptors (the `drepDirectory.votingPower` hunks are a reorder — counts match
  HEAD).

**Invariant check.** Local-first holds (container reads `drepIndex` only; no IPC/CLI);
anchor floor holds (inert monospace text under the "On-chain anchor reference" label,
D7); delegation handoff is `location.state`-only with the route param forwarded
untransformed (`selectedDRepId` byte-equal, D10/#10); CIP-129-only display via the
shared `DRepIdDisplay` (D5); `DRepStatusBadge` renders exactly once (on-chain Status
row, B-1 fix honored) and stays active/inactive-only; copied confirmation defaults off
so directory cards are behaviorally unchanged.

**Deviations accepted.** Guide-sanctioned only: prettier 2.1.2 reflows of three
guide-verbatim snippets (formatting-only, diff-checked); `node_modules/.bin/<tool>`
invocation per the B-2 environment note; Storybook verified at tsc/eslint level (no
display in this devcontainer — slice-2 precedent; eyeball via the global locale toggle
before merge). Staging also carries the three pre-existing planning docs under
`task-plans/` — pre-existing untracked content riding with the slice, not task-116
work product.

**Blockers:** none.

**Decision: approved** — task-116 may be committed as staged
(`feat(gov): task-116 build DRep detail view with on-chain fields and anchor presence`).

---

## Code Review: task-117 pass 1 — 2026-07-24

**Scope.** One broad pass over the staged (uncommitted) task-117 diff against the
implementation guide and PRD: route literal + `Routes.tsx` wiring, card "View details"
CTA threaded Directory → List → Card, `DRepDirectoryPage.handleViewDetails` push, the
new `maskAnalyticsRoute` helper + `MatomoClient.getAnalyticsURL` masking, the
sanitization-suite extension, the `DetailRouteStub` → production migration in
`VotingGovernancePage.spec.tsx`, one locale key per language, Storybook prop fixes,
the F-7 findings append, and the tool-managed message-JSON regeneration.

**Verified during review (re-run, not taken on trust).**

- `node_modules/.bin/tsc --noEmit` — exit 0, zero errors.
- Scoped eslint on the guide's 11-path list — 0 errors, 17 warnings, all in the
  pre-existing baseline classes (`no-unused-vars` on type positions, harness
  `no-explicit-any`).
- Focused Jest — 42/42 green with the exact expected split: `VotingGovernancePage` 8,
  `DRepDirectory` 20, `DRepDetailPage` 11, `DRepDirectoryPage` 3.
- Sanitization floor — 23/23 green; the diff of the suite is append-only (matomo
  mock + three imports + one new describe); the inherited 20 tests are byte-identical.
- The tracked-URL boundary test pins `http://daedalus/governance/dreps/:drepId` and
  `not.toContain(CIP129_DREP)` against the real `MatomoClient.sendEvent` through the
  mocked `matomo-tracker` — the analytics-URL regression the slice required.
- Pins byte-identical to base across the whole slice: `VotingStore.ts`,
  `GovernanceStore.ts`, `delegationFormState.ts` (checked `d5e3a03f2..staged`); no
  `.scss.d.ts` anywhere under `source/`.
- `VotingGovernancePage.spec.tsx` diff contains exactly the nine permitted Step-12
  edits; the browse-push, single-hop, payload, and all three HW tests are untouched,
  and the migrated two-hop test keeps its three final assertions identical.
- i18n: `governance.drepDirectory.card.viewDetails` present in BOTH locales,
  `!!!`-prefixed, correctly sorted after `card.select`; `defaultMessages.json` /
  `translations/messages.json` carry only the one new descriptor (tool-managed).

**Invariant check.** Sanitization floor holds and is strengthened: the D2 mask closes
the hash-URL leak at the single `getAnalyticsURL` boundary (feeding both
`sendPageNavigationEvent` and `sendEvent`), `pageTitle` strings are static, and the
new renderer code makes zero logger/analytics/storage calls (grep-verified).
Local-first holds (no new IPC/CLI; the detail still reads `drepIndex` only).
Delegation handoff is `location.state`-only: `handleViewDetails` pushes
`${ROUTES.GOVERNANCE.DREPS}/${drepId}` (raw, byte-equal) with only the picked
`{ from, selectedWalletId, voteType }`, and the new forwarding-state test pins
`selectedDRepId` absent on the outbound hop. D8 `exact` is on the directory route, so
no double render (the two-hop test would fail loudly). Anchor floor and CIP-129-only
display are untouched by this diff and re-covered by the still-green detail spec.

**Deviations accepted.** Both implementer-reported deviations are sound: (1) Step 14
said CREATE `slice-4-findings.md`, but task-116 had already created it with F-1..F-6
covering D1/D4/D2/D10/npx — the F-7 append supplies the remaining required
planning-drift items (harness-route anchor, 20-not-17 floor count, missing §9 anchor
label id), so all Step 14 content exists; (2) `node_modules/.bin/<tool>` invocation
per the guide's own npx correction. Tracker/PRD status updates ride at orchestrator
commit time (task-116 precedent: its statusReason embeds post-review outcomes).

**Blockers:** none.

**Decision: approved** — task-117 may be committed as staged
(`feat(gov): task-117 wire DRep detail route and mask drep id from analytics urls`).

---

## Planner: slice-4 close-out

Slice-4 is closed. Both tasks completed and committed on `job/slice-4`: task-116
(`34296ec16`, on-chain-only DRep detail — component tree, `DRepDetailPage` container
with the directory refresh contract, shared `DRepSourceLabel`/`DRepIdDisplay`
extensions, 21 locale keys, 11-test spec, five global-toggle stories) and task-117
(`f4fec59c9`, detail route + directory `exact`, View-details CTA with picked-state
push, `maskAnalyticsRoute` in `getAnalyticsURL`, stub → production harness migration,
floor suite 20 → 23).

**Review iterations.** Planning: one Critiquer pass returned `requires_changes` with
three blockers (B-1 duplicate status badge, B-2 broken `npx`, B-3 wrong
`DRepDirectory` count); a single Planner fix pass cleared all three and planning was
approved. Implementation: one code-review pass per task, both approved with zero
blockers and no unsanctioned deviations.

**Invariant checks run.** Sanitization floor green at every gate (20/20 after
task-116, 23/23 after task-117, inherited 20 tests byte-identical), with the D2 mask
closing the detail-route hash-URL leak at the single `getAnalyticsURL` boundary and
grep confirming zero logger/analytics/storage calls in new code. Anchor floor: inert
anchor URL + hash under "On-chain anchor reference", interactivity grep empty,
`.closest('a')` pinned null. Local-first: detail reads the existing `drepIndex` only,
no new IPC/CLI. Delegation handoff: `location.state`-only, DRep id byte-equal and
untransformed, proven by the migrated two-hop test with slice-2/3 assertions
unchanged.

**Deferrals.** Vote positions → future `gov-state` slice (D1, graceful unavailable
state shipped); favorites → slice-7 (D3); dual-ID display → cv-1 (D5, CIP-129-only
here); "Registered: epoch N" dropped (D4, no data source). Durable findings F-1…F-7
recorded in `research/slice-4-findings.md`.
