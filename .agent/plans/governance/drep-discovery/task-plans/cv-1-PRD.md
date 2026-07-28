# CV-1 PRD: Current-Vote Plumbing + CurrentVoteSummary Core States

> **Planning Status:** approved | **Slice Status:** closed 2026-07-28 (all 12 tasks complete — see Final Outcome) | **Date:** 2026-07-27 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `cv-1` — "Current-vote 1 - Plumbing + CurrentVoteSummary core" (riskLevel: medium)
> **Tasks:** task-126 … task-135, plus task-170 / task-171 added post-approval (12 tasks; cross-phase deps task-109 complete, task-110 verified)
> **Findings:** [research/cv-1-findings.md](../research/cv-1-findings.md)
> **Implementation guide:** cv-1-implementation-guide.md (authored after PRD review)

---

## Executive Summary

cv-1 opens Track V (current-vote display). It commits authored cardano-wallet
fixtures for the four voting-delegation shapes, fixes the latent
`'voting_and_delegating'` → `'delegating_and_voting'` wire-literal bug (constant
export name preserved), widens the renderer delegation types with a
`voting?: WalletVotingTarget` field, adds the pure `normalizeDRepIdentity`
helper (HRP- and credential-type-aware), maps `delegation.active.voting` through
`_createWalletFromServerData` into a new `Wallet.votingTarget` observable with
`currentVote` / `isVoting` computeds, and renders the `CurrentVoteSummary`
component's four CORE states only: `noDelegation` (warning + CTA), DRep-ID-only,
`abstain`, `no_confidence`. Storybook (4 core knobs), Jest (mapper + computeds +
snapshots), and core i18n keys land in-slice. The live status badge and every
other enrichment is cv-2.

Two rows were added to the phase after this PRD was approved and land in the
same slice: task-170 redacts the raw wallet payloads at the
`AdaApi::getWallets` / `AdaApi::getWallet` log sites — a live invariant-2 breach
that task-128's `voting` field widened (`api.ts:379-383`, `:458-460`) — and
task-171 restores the leading `!!!` marker on the nineteen feature-introduced
ja-JP strings and adds the Jest guard that keeps the gap closed (invariant 11).

Everything is renderer-only: no new IPC channel, no new cardano-wallet endpoint,
no signing-path change, no WalletsStore polling change
(designs/current-vote-display-design.md:14).

## Problem Statement — Why Now

- Daedalus cannot show a wallet's current governance delegation at all. Users who
  have delegated to a DRep, Abstain, or No Confidence see nothing, and users who
  have NOT delegated get no CIP-1694 reward-withdrawal warning — a real funds-UX
  gap (invariant 9 below).
- The renderer carries a latent parsing bug today: the wire value is
  `delegating_and_voting` but the code compares against `'voting_and_delegating'`
  (`source/renderer/app/domains/Wallet.ts:42`,
  `source/renderer/app/api/wallets/types.ts:84`), so real
  pool-and-DRep-delegating wallets are misclassified. cv-1 is the phase the plan
  assigns this fix to (governance-drep-discovery-plan.md:152, reconciled to
  task-127 — see F-1 in cv-1-findings.md).
- Slices 1-7 and ux-refinement are complete/verified; the shared display
  components `DRepIdDisplay` / `DRepSourceLabel` (task-109/task-110) that
  task-132 reuses are verified. cv-2 (badge, VotingPowerDelegation pre-fill,
  same-vote prevention) is blocked on cv-1's plumbing; starting cv-1 now keeps
  Track V on the critical path.

## Per-Task Contract (interaction mode, scope, dependencies)

No cv-1 task is in the locked non-autonomous set (task-125, task-166 remainder,
task-158, release-end `!!!` copy review). Planning surfaced no blocking decision,
so all twelve tasks are classified `autonomous` — including task-171, which only
restores `!!!` markers and never strips one, so it is not the release-end copy
review. Decisions D-1 … D-10 below close every question planning raised.

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-126** — Commit cardano-wallet voting/delegating fixtures | `autonomous` | Author small `GET /v2/wallets/{id}` JSON fixtures under `tests/mocks/wallets/` for: `status=voting` + `active.voting=drep1…`; `status=delegating_and_voting` + pool target + `voting=drep_vkh1…`; `status=voting` + `voting=abstain`; `status=voting` + `voting=no_confidence`. Authored from the pinned v2026-05-11 swagger shape (D-5); `delegation.next` modeled as an ARRAY (D-9); bech32 vectors checksum-verified (D-8) | No live capture (no wallet/network in this devcontainer); no real wallet ids; no fixing of the singular `next` type (out of cv-1) | — |
| **task-127** — Fix latent `delegating_and_voting` literal mismatch | `autonomous` | Change the underlying string in the `DelegationStatus` union (`api/wallets/types.ts:84`) and `WalletDelegationStatuses.VOTING_AND_DELEGATING` (`domains/Wallet.ts:42`) to `'delegating_and_voting'`; grep-and-update every comparison against the old literal; unit test pins constant === wire literal | No rename of the `VOTING_AND_DELEGATING` export; no behavioral change beyond the literal | task-126 |
| **task-128** — Widen `WalletDelegation`/`WalletNextDelegation` with `voting` field | `autonomous` | Add `voting?: WalletVotingTarget` (discriminated on `kind: 'drep' \| 'abstain' \| 'no_confidence'`) to both types in `api/wallets/types.ts`; DRep variant carries `DRepIdentity` imported from `source/common/types/governance.types.ts:20-31` (D-6 — reuse, never redefine) | No `givenName`/`anchorUrl` on `DRepIdentity` (anchor display is drepIndex-owned, design :99); no runtime code | task-127 |
| **task-129** — `normalizeDRepIdentity` helper | `autonomous` | New pure function `source/renderer/app/utils/governance/normalizeDRepIdentity.ts`: bech32 decode preserving HRP; classify `drep` (CIP-129, header byte 0x22 key / 0x23 script) / `drep_vkh` / `drep_script` (CIP-105) / `abstain` / `no_confidence` sentinels; fill missing canonical forms; reuse an existing renderer bech32 helper | No new package.json dependency; no logging of raw ids on failure (unknown HRP → null + sanitized HRP-only warning path is task-130, design :110) | task-128 |
| **task-130** — Mapper in `_createWalletFromServerData` + collision rules | `autonomous` | `api.ts`: `parseVoting` + 4-way status switch — `voting` → `delegatedStakePoolId = null`, `votingTarget = parsed(active.voting)`; `delegating_and_voting` → both populated; `delegating` / `not_delegating` → `votingTarget = null`, pool mapping byte-identical; unknown HRP → parser null, sanitized warning (HRP only, never the raw DRep id), treat as `voting === undefined`; pass `votingTarget` into the Wallet constructor | No anchor-derived hydration; no lovelace parsing; no WalletsStore polling change | task-128, task-129 |
| **task-131** — Wallet domain `votingTarget`/`currentVote`/`isVoting` incl. `update()` pick list | `autonomous` | `domains/Wallet.ts`: `@observable votingTarget`; `@computed currentVote(): WalletVotingTarget \| null`; `@computed isVoting`; extend `WalletProps` AND the explicit `update()` pick list at `Wallet.ts:177-200` (D-7 — the constructor is `Object.assign` but `update()` picks; missing entry = stale value on poll) | No historical vote-target fields in v1; no `pendingVote` computed (D-10) | task-130 |
| **task-132** — `CurrentVoteSummary` CORE states, no live badge | `autonomous` | New `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx` + `.scss` module; props `{ currentVote: WalletVotingTarget \| null }`; four render rules (design :182-185); reuse `DRepIdDisplay`; status labels via a component-local react-intl renderer because `DRepSourceLabelVariant = 'on-chain' \| 'on-chain-anchor-reference'` cannot express them (D-4); `noDelegation` shows reward-withdrawal warning + CTA, never hides the panel | NO live status badge (cv-2 task-136, invariant 14); no cardano-cli spawn or fallback IPC lookup (design :187); no `givenName`/anchor URL (anchor-1/2); no view-details link (cv-2, gated on task-116) | task-131, task-109 ✔, task-110 ✔ |
| **task-133** — Storybook entry, 4 core knobs | `autonomous` | `storybook/stories/governance/CurrentVoteSummary.stories.tsx` with knob ids `noDelegation \| drepUnverified \| abstain \| noConfidence`; global English/Japanese toggle only (D-3); follow existing `storybook/stories/governance/` conventions; pure wallet factory shape per plan :156 | No `drepVerified` knob and no 5-value knob (cv-2); no local `IntlProvider`, no per-locale story variants; no `GOVERNANCE_WALLETS` mutation | task-132 |
| **task-134** — Jest: mapper + Wallet computeds + core snapshots | `autonomous` | Specs under `tests/jest/` using the live `.spec.ts(x)` convention (F-3): mapper cases drep / abstain / no_confidence / delegating_and_voting / pending, voting-only asserts `delegatedStakePoolId === null`; `currentVote`/`isVoting` per kind plus null; snapshots for the 4 core states; re-assert `tests/jest/security/governance-sanitization.spec.ts` green (invariant 2) | No `.test.ts` naming (stale design-doc names, F-3); no cv-2 same-vote regression tests (task-147/148) | task-130, task-131 |
| **task-171** — Restore the `!!!` marker on ja-JP DRep Discovery copy and guard it | `autonomous` | Restore the leading `!!!` on the nineteen feature-introduced ja-JP keys in `ja-JP.json` (seventeen `governance.drepDirectory.*` plus `governance.tabs.directory` and `sidebar.categoryTooltip.governance`); add a Jest guard asserting every key whose en-US value starts with `!!!` is also marked in ja-JP, with an allow-list holding only the pre-existing `wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement` | Never strips a marker — removal stays the user-owned release-end copy review (README :16, :18); no en-US edit and no `defaultMessages.json` / `translations/messages.json` change (the `!!!` lives in each component's source `defaultMessage`) | — (no deps of its own; it is a dependency of task-135 and of cv-2's task-146, so the guard precedes every remaining governance copy mint) |
| **task-135** — i18n core keys `voting.governance.currentVote.*` | `autonomous` | `CurrentVoteSummary.messages.ts` with the core-state keys (inventory below); `yarn i18n:manage` populates `en-US.json` / `ja-JP.json`; every new string keeps the leading `!!!` (invariant 11); ja-JP reviewed for overflow | No `sameVoteHint`, no `status.expiring/.inactive/.unavailable` (cv-2 task-146); `confirmationDialog.previousVote/.newVote` reserved-not-wired (ux :168); no `!!!` removal | task-132, task-171 |
| **task-170** — Redact raw wallet payloads at the AdaApi wallet-list log sites | `autonomous` | Wrap `wallets` / `legacyWallets` at `AdaApi::getWallets success` (`api.ts:379-383`) and the single `wallet` at `AdaApi::getWallet success` (`:458-460`) in `filterLogData`, or reduce them to non-identifying fields; audit the remaining whole-payload `logger.*` sites in `api.ts` that can carry `delegation.*.voting`; add a `getWallets` call-boundary case to `tests/jest/security/governance-sanitization.spec.ts` | No change to the existing `hwLocalData` filtering or to non-governance log shapes; the `[HW-DEBUG]` error-message surface (slice-3-findings :71-74) is a substring-in-message class `filterLogData` cannot reach and stays out | task-130, task-109 (complete) |

## Acceptance Criteria (verbatim from governance-drep-discovery-plan-tasks.json)

### task-126 — Commit cardano-wallet voting/delegating fixtures (json :811-832)

- "Fixtures cover voting / delegating_and_voting / abstain / no_confidence."
- "All fixtures conform to ApiWallet shape from cardano-wallet v2026-05-11 swagger."
- "Each fixture is small (only the fields needed for mapper tests) and contains no real wallet ids."

### task-127 — Fix latent delegating_and_voting literal mismatch (json :835-859)

- "DelegationStatus union contains 'delegating_and_voting', not 'voting_and_delegating'."
- "WalletDelegationStatuses.VOTING_AND_DELEGATING === 'delegating_and_voting' (constant name preserved)."
- "Unit test asserts the constant equals the wire literal."
- "No remaining 'voting_and_delegating' string literal in the renderer codebase (except possibly in changelog/migration notes)."

### task-128 — Widen WalletDelegation and WalletNextDelegation with voting field (json :862-885)

- "WalletDelegation and WalletNextDelegation expose voting?: WalletVotingTarget."
- "WalletVotingTarget discriminator is 'kind' with values 'drep' | 'abstain' | 'no_confidence'."
- "DRepIdentity contains raw (required) plus optional cip129/cip105/credentialHex."
- "DRepIdentity carries `credentialType` to prevent key/script collision in the same-vote comparator."

### task-129 — Implement normalizeDRepIdentity helper (json :888-912)

- "Pure function with no side effects."
- "Round-trips drep1 → cip105 → drep1 losslessly."
- "Rejects invalid bech32 input by returning null or throwing a typed error consistent with the rest of the renderer."
- "Unit tests cover all three prefixes plus invalid input."
- "Uses `bech32.decode` (or equivalent) that preserves the HRP — must distinguish `drep`, `drep_vkh`, `drep_script` even when the 28-byte payload is identical."
- "Returns `DRepIdentity` carrying `credentialType: 'key' | 'script'` so the same-vote comparator does not falsely equate a key DRep and a script DRep with the same hash."
- "Reuses an existing bech32 helper from the renderer bundle (e.g. `@cardano-sdk/core` or the existing `bech32` dependency) — no new direct dependency added to package.json."

### task-130 — Extract current vote in _createWalletFromServerData with explicit collision rules (json :915-944)

- "For status === 'voting' fixture, delegatedStakePoolId === null and votingTarget is populated."
- "For status === 'delegating_and_voting' fixture, both delegatedStakePoolId and votingTarget are populated."
- "Existing delegating / not_delegating mappings are byte-identical to today's behaviour."
- "When status === 'voting' and active.voting is absent, votingTarget is null — never parses active.target (which is a pool id)."
- "`DRepIdentity` shape does NOT include `givenName` or `anchorUrl`. These fields are removed. The mapper is not responsible for hydrating anchor-derived display values."

### task-131 — Extend Wallet domain model with currentVote / isVoting (json :947-967)

- "Wallet.currentVote returns the parsed WalletVotingTarget or null."
- "isVoting === true iff currentVote !== null."
- "Unit tests cover all four delegation statuses plus pending."
- "Wallet.update() pick list explicitly includes `votingTarget` (and the new computeds' dependencies) so polled wallet refreshes propagate the new vote target — preventing the well-known \"stuck stale value\" Daedalus pitfall."
- "Verification runs the unfiltered `yarn test:jest --runInBand` — 82 suites at HEAD — not the `tests/jest` path filter, which is a testPathPattern matching only 7; the statusReason reports the suite and test counts from that unfiltered run."
- "The four Wallet-importing specs the filter excludes — source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx, source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx, source/renderer/app/components/wallet/WalletSendForm.spec.tsx, tests/wallets/unit/wallet-utils.spec.ts — are green after the Wallet.ts edit, as is tests/jest/security/governance-sanitization.spec.ts."
- "cv-1-implementation-guide.md:1331 (this task's verify block) and :1740 (task-132's) drop the `tests/jest` argument, so neither trailing \"suites stay green\" comment describes a run that skipped 75 of 82 suites."
- "The `(cv-1, task-129)` parenthetical is removed from source/common/types/governance.types.ts:18 with the \"Populated by normalizeDRepIdentity\" sentence left intact, after which `grep -rn 'task-1[0-9][0-9]' source/ tests/ storybook/` returns zero hits."
- "That comment edit rides this task's single commit, and nothing this task adds introduces a task id, review label, or change history into a code comment."

### task-132 — Implement CurrentVoteSummary core states (no live status badge) (json :970-989)

- "Renders DRep id (via DRepIdDisplay) + source label (via DRepSourceLabel)."
- "When the wallet has no governance delegation, renders the reward-withdrawal warning and CTA instead of hiding the panel."
- "DRep state renders the DRep id ONLY in this slice; verified body.givenName and the external anchor URL link are explicitly OUT of scope here and render only after the hardened/verified anchor pipeline lands (anchor-1/anchor-2), preventing display of unverified anchor-derived identity. The in-app view-details link is gated on slice-4 (task-116) and added in cv-2."
- "Handles abstain and no_confidence kinds with no DRep id rendered."
- "Live active/inactive/expiring status badge is explicitly out of scope here and lands in cv-2 (task-136)."
- "If DRepSourceLabel cannot localize the new abstain / noConfidence / delegatedToDRep labels with its existing prop contract, wrap it in a CurrentVoteSummary-local renderer that consumes react-intl directly. Do NOT silently fall back to English literals."

### task-133 — Add CurrentVoteSummary Storybook entry (core knobs) (json :992-1005)

- "Four core knob values (noDelegation | drepUnverified | abstain | noConfidence) render without console errors."
- "Story renders in both en-US and ja-JP locales without overflow."

### task-134 — Unit tests: mapper, Wallet computeds, and CurrentVoteSummary core snapshots (json :1008-1030)

- "All five mapping cases pass."
- "voting-only fixture asserts delegatedStakePoolId === null."
- "Wallet computeds covered for every WalletVotingTarget kind plus null."
- "CurrentVoteSummary core-state snapshots cover noDelegation / drepUnverified / abstain / noConfidence."
- "tests/jest/governance/normalizeDRepIdentity.spec.ts gains a wrong-length `drep_vkh` (or `drep_script`) vector exercising the CIP-105 length guard at normalizeDRepIdentity.ts:47-49, and a coverage run over that module reports no uncovered lines."
- "The added vector is checksum-valid and decodes under the repo's bech32 version — a 29-byte payload under a CIP-105 HRP — so it reaches the length guard rather than the decode catch."
- "The accepted-target mapper cases (voting-only DRep, delegating_and_voting, abstain, no_confidence) assert `expect(mockedWarn).not.toHaveBeenCalled()`, pinning the never-logs floor on the accepted-id path and not only on the rejection paths."
- "The existing eight normalizeDRepIdentity cases and tests/jest/security/governance-sanitization.spec.ts stay green."
- "This task edits tests/jest/governance/normalizeDRepIdentity.spec.ts alongside the new files under its tests/jest/api/ targetPath, and the cv-1 guide's task-134 file list records that spec as MODIFIED rather than created."

### task-171 — Restore the !!! preliminary marker on ja-JP DRep Discovery copy and guard it (json :1033-1047)

- "All nineteen feature-introduced keys carry the leading `!!!` in ja-JP.json: the eighteen governance.\* keys plus sidebar.categoryTooltip.governance."
- "A Jest guard asserts that for every key present in both locale files whose en-US value starts with `!!!`, the ja-JP value also starts with `!!!`, with a documented allow-list containing only the one pre-existing non-feature exception (wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement)."
- "The guard demonstrably fails when a newly marked en-US key has an unmarked ja-JP counterpart, so task-135, task-146 and anchor-2 copy cannot silently reopen the gap."
- "`yarn i18n:manage` runs clean and defaultMessages.json / translations/messages.json are unchanged by the restoration (the `!!!` lives in each component's source defaultMessage; only the ja-JP translation file is edited)."
- "The task restores markers only and never strips one: removal remains the release-end manual copy review, user-owned per README.md:16 and :18."

### task-135 — Add i18n keys for CurrentVoteSummary core states (json :1050-1064)

- "Core keys present in en-US.json and ja-JP.json: headerCurrent, statusDelegatedToDRep, statusAbstain, statusNoConfidence, noDelegation title/warning/subline/cta, DRep link labels, abstain/noConfidence captions."
- "Preliminary ja-JP copy is reviewed for length / layout overflow while retaining the leading !!! marker."
- "yarn i18n:manage runs clean."

### task-170 — Redact raw wallet payloads at the AdaApi wallet-list log sites (json :1067-1084)

- "`wallets` and `legacyWallets` at the `AdaApi::getWallets success` call site are wrapped in `filterLogData`, or reduced to non-identifying fields; `hwLocalData` filtering unchanged."
- "The `AdaApi::getWallet success` call site (:458-460) receives the same treatment on the single `wallet` object."
- "Every remaining whole-payload `logger.*` call site in api.ts that can carry `delegation.*.voting` is audited; those that can are wrapped, and the audit list is recorded in the task evidence."
- "tests/jest/security/governance-sanitization.spec.ts gains a `getWallets` call-boundary case driving a voting-wallet fixture through `AdaApi.getWallets`, asserting no CIP-129/CIP-105 bech32 string and no abstain / no_confidence literal reaches the emitted logger payload."
- "INHERITED sanitization floor: the full governance-sanitization suite is green with the new case; non-governance log shapes for the wallet-list flow are otherwise unchanged."

## Planning Decisions (binding, as applied)

- **D-1 — Wire-literal fix owned by task-127.** The plan's Key Decisions row
  attributed the fix to task-128; the tasks JSON assigns it to task-127 (json
  :826-828). JSON is authoritative; governance-drep-discovery-plan.md:152 was
  reconciled to task-127 (smallest edit, nothing else touched). Recorded as F-1.
- **D-2 — Task-count drift record-only.** Plan :265 says "58 tasks across 13
  phases"; JSON metadata says 69 tasks / 14 phases (json :16, :1729-1730). The
  plan sentence itself names the JSON the source of truth; no plan edit made.
  Recorded as F-2.
- **D-3 — Live test/storybook conventions over stale doc names.** Design §12
  names `.test.ts` files and a `voting-sanitization.test.ts` suite (design
  :248, :250, :253); the live tree has zero `.test.ts` under `tests/`, uses
  `.spec.ts(x)`, and the sanitization floor is
  `tests/jest/security/governance-sanitization.spec.ts`. Live repo wins.
  Storybook uses the global English/Japanese locale toggle — no local
  `IntlProvider`, no per-locale story variants — matching the existing
  `storybook/stories/governance/` conventions. Recorded as F-3.
- **D-4 — task-132 label fallback resolved up front: local intl renderer.**
  `DRepSourceLabelVariant = 'on-chain' | 'on-chain-anchor-reference'`
  (`DRepSourceLabel.tsx:18`) cannot express the new
  `delegatedToDRep` / `abstain` / `noConfidence` status labels. Per the
  task-132 acceptance criterion, `CurrentVoteSummary` therefore renders status
  labels through a component-local renderer that consumes react-intl directly,
  and reuses `DRepSourceLabel` unchanged only for the on-chain source label on
  the DRep state. English string literals are never used as fallback, and
  `DRepSourceLabel`'s existing prop contract is not widened in cv-1.
- **D-5 — Fixtures are authored, not captured.** No running cardano-wallet and
  no network exist in this devcontainer. task-126 fixtures are authored from the
  pinned cardano-wallet v2026-05-11 swagger (commit
  c642e0779676d2567e3d5fa1e2db9f029b6398e1; plan :174, :196) plus the live
  `ApiWallet` consumption in `api.ts`. The task title's "Capture" is satisfied
  by shape-conformant authored fixtures; AC-2 ("conform to ApiWallet shape …
  swagger") is the binding check.
- **D-6 — Reuse the existing `DRepIdentity`.** The interface already exists at
  `source/common/types/governance.types.ts:20-31` with exactly the shape
  task-128 specifies (`raw`, `cip129?`, `cip105?`, `credentialHex?`,
  `credentialType: 'key' | 'script'`). cv-1 imports it; it is never redefined.
- **D-7 — `update()` pick list is in-scope, explicitly.** The `Wallet`
  constructor is `Object.assign` (`Wallet.ts:172-174`) but `update()` picks an
  explicit field list (`Wallet.ts:177-200`). task-131 must extend BOTH
  `WalletProps` and the pick list, otherwise polled refreshes silently drop
  `votingTarget`.
- **D-8 — No hand-invented bech32.** Every `drep1…` / `drep_vkh1…` /
  `drep_script1…` / `pool1…` string in fixtures and specs is either sourced from
  existing repo docs/fixtures or synthesized with the `bech32` library, and each
  is checksum-verified by decoding before commit.
- **D-9 — `delegation.next` modeled as an array.** The `AdaWallet` type declares
  `next` singular but the consumer treats it as an array (`last(next)`);
  fixtures follow the consumer (array). Correcting the type itself is out of
  cv-1 scope.
- **D-10 — task-134's description mention of `Wallet.pendingVote` is treated as
  description drift.** task-131 adds only `votingTarget` / `currentVote` /
  `isVoting` and its ACs forbid historical vote-target fields; no task-134
  acceptance criterion requires `pendingVote`. cv-1 tests cover the AC list;
  "plus pending" in task-131's ACs is covered by a wallet fixture carrying a
  pending `next` delegation, not by a new computed.

## User Stories

- **US-CV1.1 — See my current governance delegation.** As a wallet owner who has
  delegated voting power, I open the governance screen and see, above
  VotingPowerDelegation, my current target: my DRep's id (with on-chain source
  label), or Abstain, or No Confidence — with the id row always visible as the
  identity-equality safety anchor (ux :83).
- **US-CV1.2 — Understand why my rewards are stuck.** As a wallet owner with no
  governance delegation, I see the CIP-1694 reward-withdrawal warning, the
  "Daedalus will not pick for you" subline, and a CTA to choose a delegation —
  the panel never hides (invariant 9).
- **US-CV1.3 — Dual delegation shown truthfully.** As an owner of a wallet that
  delegates to a pool AND a DRep (`delegating_and_voting`), both my stake-pool
  delegation and my vote target parse correctly — the current literal bug makes
  this state invisible today.

## Non-Functional Requirements

- **Renderer-only:** no new IPC channel, no new cardano-wallet endpoint, no
  signing-path change, no WalletsStore polling change (design :14). cv-1 parses
  no lovelace and adds no IPC (invariant 5 untouched).
- **Sanitization floor (invariant 2, inlined):** no DRep id, no
  abstain/no_confidence literal, no CIP-129/CIP-105 bech32 string in any logger,
  analytics, or electron-store payload. The unknown-HRP warning logs the HRP
  only, never the raw id (design :110). The task-111 spy suite
  `tests/jest/security/governance-sanitization.spec.ts` is re-asserted green in
  this slice. Test fixtures and docs MAY contain DRep ids — the floor binds
  runtime logging/analytics/store paths only.
- **Byte-equality (invariant 10, inlined):** CIP-129, CIP-105, and the signed
  payload `vote.id` remain byte-equal through every identity-display change;
  `normalizeDRepIdentity` round-trips losslessly and is credential-type-aware
  (key vs script never conflated even with identical 28-byte payloads).
- **i18n (invariant 11, inlined):** every new en-US and ja-JP string keeps the
  leading `!!!` marker; removing `!!!` is a release-end manual review, never a
  per-slice task.
- **Accessibility:** glyph+text state indication, aria labels, focus order per
  ux :188-197.

## Architecture: Data Flow (cv-1 delta)

```
cardano-wallet GET /v2/wallets/{id}            (unchanged endpoint, pinned v2026-05-11)
  └─ delegation.active.voting  ── authoritative current state (plan :154, CIP-1694:
                                  newest on-chain delegation IS current; no waiting period)
        │
        ▼
api.ts _createWalletFromServerData             (task-130)
  parseVoting(raw) ─ normalizeDRepIdentity     (task-129, pure, HRP-preserving)
  4-way status switch over DelegationStatus    (task-127 fixes the wire literal;
                                                task-128 widens the types)
  voting-only status ⇒ delegatedStakePoolId = null
        │  votingTarget: WalletVotingTarget | null
        ▼
Wallet domain                                  (task-131)
  @observable votingTarget → @computed currentVote / isVoting
  WalletProps + update() pick list BOTH extended
        │  wallet.currentVote
        ▼
CurrentVoteSummary                             (task-132; props { currentVote })
  above VotingPowerDelegation, between wallet picker and vote-type controls,
  never modal/collapsible (ux :31); reads ONLY wallet.currentVote
  states: noDelegation | drep (ID-only) | abstain | no_confidence
        │
        ├─ Storybook: 4 core knobs             (task-133)
        ├─ Jest: mapper/computeds/snapshots    (task-134)
        └─ i18n: voting.governance.currentVote.* core keys (task-135)
```

`WalletVotingTarget` (design :74-93):
`{ kind: "drep"; drep: DRepIdentity; source: "verified" | "unverified" | "onchain" } | { kind: "abstain" } | { kind: "no_confidence" }`.
HRP discrimination (plan :153, design :95): `abstain` / `no_confidence`
sentinels; `drep1…` = CIP-129 (header byte 0x22 key / 0x23 script);
`drep_vkh1…` / `drep_script1…` = CIP-105 (key / script). Abstain and No
Confidence are form-only sentinels, never DRep directory entries (invariant 13;
research :49-53).

## What cv-1 Deliberately Does NOT Include (cv-1 vs cv-2 boundary)

The design and UX docs describe the full cv-2 card; the tasks JSON scopes cv-1.
Explicitly out:

- **No live status badge.** The active/inactive/expiring badge is cv-2
  (task-136) and depends on the directory `drepIndex`; the cv-1 core states do
  not (plan :284, :295). Inlined invariant 14: canonical on-chain DRep status is
  `active | inactive`; `expiring` is renderer-derived display state; cv-1 must
  NOT render the badge. `CurrentVoteSummary` must not spawn a cardano-cli
  invocation or fallback IPC lookup to compensate (design :187).
- **No VotingPowerDelegation changes.** Pre-fill from the current on-chain
  delegation, same-vote submit disabling, and the confirmation-dialog
  current-target work are cv-2. cv-1 only mounts a sibling panel above it
  (ux :31).
- **No `sameVoteHint` and no `status.*` keys.** Deferred to cv-2 (task-146);
  `confirmationDialog.previousVote` / `.newVote` are reserved-not-wired
  (ux :168). The Previous→New comparison story is deferred beyond v1 (ux :25).
- **No anchor-derived display.** `givenName` / `anchorUrl` are NOT on
  `DRepIdentity` (design :99); name display waits for the verified anchor
  pipeline (anchor-1/anchor-2; plan :158 — ID-only until verified). cv-1 renders
  none of the CIP-119 fields (research :57-65).
- **No view-details link.** Gated on slice-4 (task-116), added in cv-2.
- **No second delegation backend (invariant 4, inlined).** Selection supplies a
  DRep ID to the existing `delegateVotes`/`VotingStore` signing path via React
  Router `location.state` only; `VotingStore` never reads `GovernanceStore`.
  sw/hw submission is unchanged (plan :253-259: confirmation shows DRep ID
  only).
- **No historical governance views**, no `drepVerified` Storybook knob, no
  5-value knob (cv-2), no fix to the singular `AdaWallet.delegation.next` type
  (D-9).
- **No logging sweep beyond `api.ts`.** task-170 widens cv-1's `api.ts`
  footprint past task-130's mapper fence to the two wallet-payload log sites,
  but only for key-redactable payloads `filterLogData` can reach. The
  `HardwareWalletsStore` `[HW-DEBUG]` surface recorded at slice-3-findings
  :71-74 is a substring-in-error-message class the key filter cannot fix, and it
  stays out of cv-1.

## i18n Core-Key Inventory (task-135; ux :154-186 — all keep leading `!!!`)

Namespace `voting.governance.currentVote.*`:
`headerCurrent` "Current delegation"; `statusDelegatedToDRep` "Delegated to
DRep"; `statusAbstain` "Abstain"; `statusNoConfidence` "No Confidence";
`noDelegation.title` "No governance delegation"; `noDelegation.warning` "Your
staking rewards cannot be withdrawn until you delegate this wallet's voting
power to a DRep, Abstain, or No Confidence."; `noDelegation.subline` "Daedalus
will not pick a DRep for you — choose how you want your voting power to
participate in Cardano governance."; `noDelegation.cta` "Choose a delegation";
`drep.viewDetails` "View details"; `drep.anchorMetadata` "Anchor metadata ↗";
`abstain.caption` "Your stake is recorded on chain as not participating in
governance. Rewards can be withdrawn."; `noConfidence.caption` "Your stake
counts as Yes on every motion of no-confidence. Rewards can be withdrawn."

## Docs / Designs / Research / Workflows / Skills Consulted

- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md` —
  Track V definition :284/:295; Key Decisions :152-159; delegation integration
  :253-259; fixture pin :174/:196; sequencing note :265.
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  — cv-1 phase :804-995 (authoritative task contracts, quoted verbatim above).
- `.agent/plans/governance/drep-discovery/designs/current-vote-display-design.md`
  — renderer-only :14; TS shapes :74-99; mapper :110/:122-146; normalizer
  :151-166; component :170-187; testing table :246-253 (stale names, F-3).
- `.agent/plans/governance/drep-discovery/designs/current-vote-display-ux.md` —
  IA :31; ASCII states :54-105; id-row anchor :83; i18n inventory :154-186;
  reserved keys :168; accessibility :188-197; Storybook :199-211; deferral :25.
- `.agent/plans/governance/drep-discovery/research/external-research.md` —
  sentinels :49-53; key-or-script DReps :52; CIP-119 fields :57-65.
- `.agent/plans/governance/drep-discovery/research/cv-1-findings.md` — F-1…F-3
  (this planning pass).
- Precedent PRDs: `task-plans/slice-7-PRD.md`, `task-plans/ux-refinement-PRD.md`
  (heading/contract conventions).
- Live code verified during planning: `source/renderer/app/domains/Wallet.ts`
  (:42 literal, :172-174 constructor, :177-200 pick list),
  `source/renderer/app/api/wallets/types.ts:84`,
  `source/common/types/governance.types.ts:20-31`,
  `source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx:18`,
  `tests/jest/security/governance-sanitization.spec.ts` (exists),
  `tests/jest/governance/*.spec.ts` (naming convention).
- Skills flagged for implementation (not invoked at planning):
  `bech32-encoding-decoding` (fixture vectors, D-8), `storybook-creation`
  (task-133), `i18n-messaging` (task-135).

## Locked Invariants Touched (inlined)

- **(2) Sanitization floor** — touched by task-130 (warning path) and task-170
  (the `AdaApi::getWallets` / `AdaApi::getWallet` payload log sites, a live
  breach at HEAD), re-asserted by task-134 and by task-170's new `getWallets`
  call-boundary case. No DRep id, no abstain/no_confidence literal, no
  CIP-129/CIP-105 bech32 string in any logger, analytics, or electron-store
  payload; the task-111 spy suite
  `tests/jest/security/governance-sanitization.spec.ts` must be re-asserted
  green in this slice. Fixtures/docs MAY contain DRep ids — the floor binds
  runtime logging/analytics/store paths only.
- **(9) No auto-delegation** — touched by task-132/135. The `noDelegation` state
  shows the CIP-1694 reward-withdrawal warning + CTA; Daedalus never picks a
  delegation.
- **(10) Byte-equality** — touched by task-129/130. CIP-129, CIP-105, and the
  signed payload `vote.id` remain byte-equal through every identity-display
  change; `normalizeDRepIdentity` must round-trip losslessly.
- **(11) `!!!` markers** — touched by task-135 and task-171. Every new en-US and
  ja-JP string keeps the leading `!!!`; removal is release-end manual review
  only. task-171 restores the marker on the nineteen ja-JP keys that lost it and
  adds the Jest guard that fails when an en-US-marked key has an unmarked ja-JP
  counterpart; it strips nothing.
- **(13) Sentinels** — touched by task-128/129/132. Abstain / No Confidence are
  form-only sentinels, never DRep directory entries.
- **(14) Status badge boundary** — respected by task-132/133. Canonical on-chain
  status is `active | inactive`; `expiring` is renderer-derived; the live badge
  is cv-2 (task-136) — cv-1 must NOT render it.
- **(4) No second delegation backend** — not modified, guarded: cv-1 adds no
  submission path; selection continues to supply a DRep ID to the existing
  `delegateVotes`/`VotingStore` signing path via React Router `location.state`
  only; `VotingStore` never reads `GovernanceStore`.
- Context only (untouched): (1) local-first, no hosted governance APIs; (5)
  lovelace losslessness (json-bigint → decimal-string IPC → renderer BigNumber)
  — cv-1 adds no IPC and parses no lovelace.

## Dependencies

- In-phase chain: 126 → 127 → 128 → 129 → 130 → 131 → 132 → {133, 135}; 134
  depends on {129, 130, 131}; 130 also depends on 129; 132 also depends on
  task-109 ✔ and task-110 ✔ (`DRepIdDisplay` / `DRepSourceLabel` exist under
  `source/renderer/app/components/governance/_shared/`; task-109 is `complete`
  and task-110 `verified`).
- The two post-approval rows sit outside that chain: task-171 has no
  dependencies of its own and is a dependency of task-135, so the `!!!` guard
  lands before cv-1's own copy mint (the same edge is added to cv-2's task-146
  — listing order is not authoritative, `dependencies` is); task-170 depends on
  task-130 (complete) and cross-phase task-109 (complete — its AC-2 is the
  residual gap task-170 closes).
- Slices 1-7 and ux-refinement are complete/verified; no cv-1 task depends on
  cv-2, anchor-1/2, or standing work.
- Environment: pinned cardano-wallet v2026-05-11 swagger (commit
  c642e0779676d2567e3d5fa1e2db9f029b6398e1) is the fixture source of truth; no
  running wallet, no network (D-5).

## Risks / Open Questions

- **R-1 (medium) — Literal fix blast radius.** task-127 changes a string every
  status comparison may touch; `Wallet.ts:245` already consumes the constant.
  Mitigation: the constant name is preserved, a unit test pins constant ===
  wire literal, and the grep AC requires zero remaining old literals.
- **R-2 (medium) — Silent stale `votingTarget` on poll.** The `update()` pick
  list trap (D-7) fails invisibly at runtime. Mitigation: task-131 AC pins the
  pick-list entry and task-134 covers refresh behavior.
- **R-3 (low) — Bech32 fixture validity.** An invalid checksum makes the
  normalizer reject a fixture and can mask mapper bugs as parse failures.
  Mitigation: D-8 verification of every vector before commit.
- **R-4 (low) — Node v24 gate flakiness.** `yarn compile` has previously failed
  for environment reasons under Node v24.16.0. Mitigation: capture the exact
  error, then gate on `node_modules/.bin/tsc --noEmit` (plus
  `typed-scss-modules` where scss types are needed); an env failure is never
  treated as a code failure without checking.
- **R-5 (low) — ja-JP overflow.** Preliminary `!!!` ja-JP copy may overflow the
  panel. Mitigation: task-135 AC requires the overflow review; task-133 AC
  requires both locales rendering without overflow via the global toggle.
- **Resolved (not open): `pendingVote`.** task-134's description names a
  `Wallet.pendingVote` computed that no AC requires and task-131 forbids
  expanding into; resolved as description drift (D-10).
- No open questions block cv-1; nothing in this slice requires an
  `interactive_decision`, `interactive_validation`, or `manual_execution` task.

## Definition of Done

- All 62 verbatim acceptance criteria above pass (42 at PRD approval, plus the
  five each that task-131 and task-134 gained and the five each on task-170 and
  task-171).
- Gates (from the worktree root): `yarn compile` — zero TS errors (Node v24
  fallback per R-4: `node_modules/.bin/tsc --noEmit`, plus `typed-scss-modules`
  for new scss modules); `yarn lint`; `yarn prettier:check` (devcontainer
  substitute: `node_modules/.bin/prettier` on files this slice created only —
  never reformat pre-existing files); `yarn test:jest` including focused runs of
  the new suites `--runInBand`; `yarn i18n:manage` runs clean after task-135.
- `tests/jest/security/governance-sanitization.spec.ts` re-asserted green
  (invariant 2).
- Storybook: the four core knobs render in en-US and ja-JP via the global locale
  toggle without console errors or overflow.
- No `voting_and_delegating` literal remains in the renderer (task-127 AC).
- Findings F-1…F-3 recorded in `research/cv-1-findings.md`; plan :152 reads
  task-127.
- Code-review log appended per the slice-2 convention (Planner open/close,
  Critiquer, per-task Code Review entries).

## Final Outcome

**Closed 2026-07-28.** All twelve cv-1 tasks are `complete` in
`governance-drep-discovery-plan-tasks.json`, each with its own commit, each
code-reviewed and **approved on iteration 1 with zero blockers** — no task
needed a second review round anywhere in the slice.

### What shipped, task by task

| Task | Commit | Outcome |
| --- | --- | --- |
| task-126 | `35a8a57d0` | Four authored cardano-wallet voting fixtures committed (`tests/mocks/wallets/wallet-voting-*.json`). Approved. |
| task-127 | `f948845a5` | Latent `'voting_and_delegating'` → `'delegating_and_voting'` wire-literal bug fixed, constant export name preserved, zero stale literals remain. Approved. |
| task-128 | `83edc15fa` | `WalletDelegation` / `WalletNextDelegation` widened with `voting?: WalletVotingTarget`. Approved. |
| task-129 | `40bcd990a` | Pure `normalizeDRepIdentity` bech32 helper (HRP- and credential-type-aware); no new dependency. Approved. |
| task-130 | `1d33baa2c` | `delegation.active.voting` mapped through `_createWalletFromServerData` with a sanitized warning on rejection; ships no test of its own by design (pinned by task-134). Approved. |
| task-131 | `2baed760c` | `Wallet.votingTarget` observable plus `currentVote` / `isVoting` computeds, with the `update()` pick-list entry that R-2 called for. Approved. |
| task-132 | `23f443b76` | `CurrentVoteSummary` four CORE states only — `noDelegation` (CIP-1694 warning + CTA), DRep-ID-only, `abstain`, `no_confidence`. `CurrentVoteSummary.messages.ts` was created **here**, not at task-135. No badge, no store, no anchor coupling. Approved. |
| task-133 | `051567976` | Storybook "Governance / Current Vote Summary → Core states" (4 knobs). Approved, plus a Planner verification addendum that replaced the inferred `yarn storybook:build` failure with a measurement: it is red at clean HEAD for an unrelated manager-side JSX loader gap (**F-20**), while `yarn storybook`'s preview compile is clean and is the real automated floor. |
| task-134 | `d8f71319c` | Mapper cases, Wallet computeds, four `CurrentVoteSummary` snapshots, sanitized-warning cases, and the ninth `normalizeDRepIdentity` vector. A follow-up docs commit `a3e352841` reconciled the guide's Step-1 block and AC-7 wording (two → four `not.toHaveBeenCalled()` assertions). Approved. |
| task-171 | `523141760` | Nineteen feature-introduced ja-JP values regained the leading `!!!`; `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` added as the durable guard, proved to bite. Approved. |
| task-135 | `d3729994a` | Twelve `voting.governance.currentVote.*` keys seeded into all four catalogs, ja-JP hand-authored, 24/24 values byte-equal to the guide. `yarn i18n:manage` clean from this row onward, proved by sha256 non-mutation. Approved. |
| task-170 | `fb4f07f6c` | Six Shelley `AdaWallet` `logger.debug` success sites in `api.ts` wrapped in `filterLogData`; a `getWallets` call-boundary case added to the floor suite (23 → 24 tests) and proved non-vacuous by a revert probe. Approved. |

### Gates at close (measured, not asserted)

- Whole-tree `node_modules/.bin/jest --runInBand`, no path argument — **86 test
  suites (1 skipped, 85 passed), 1073 tests (12 skipped, 1061 passed), 6
  snapshots, exit 0**. Against the pre-slice baseline that is +1 suite (the new
  i18n guard) and the slice's new cases. The guide's "82 suites" figure was
  authored pre-slice and has been reconciled to 86 throughout.
- `yarn compile` exit 0 on Node v24.16.0 at every row; **R-4 is closed rather
  than merely mitigated** (F-23) — the fallback was never needed.
- `yarn lint` exit 0 at exactly 5591 warnings, zero delta, at every row.
- `yarn i18n:manage` clean from task-135 onward, re-confirmed at task-170.
- `tests/jest/security/governance-sanitization.spec.ts` green at every row —
  23 tests through task-135, 24 from task-170.

### Deviations from this PRD and its guide

- **Implementation order.** The guide's "Implementation Order" section lists
  task-170 → task-171 → task-135; the tracker's own listing order and the
  orchestration ran **task-171 → task-135 → task-170**. Both are valid
  topological orders of the same dependency graph (task-170 depends only on
  task-130 and task-109). Recorded deviation, no content change.
- **task-171 touched a third file** — `DRepDirectory.spec.tsx` (3/3), whose
  exact-text ja-JP `getByText` assertions cannot match once the marker is
  restored. Necessary consequential fix; the guide's "Files touched" list has
  been reconciled.
- **task-171 AC-4's first clause** (`yarn i18n:manage` runs clean) was
  unsatisfiable at that position by the guide's own ordering — the twelve
  currentVote keys were already missing at HEAD and are task-135's deliverable.
  **task-135 discharged it**, with sha256 non-mutation proof, and task-170 gate 7
  re-confirmed it. It is settled, not carried forward.
- **task-135 does not create `CurrentVoteSummary.messages.ts`** — task-132 did.
  The tracker's task-135 description still says otherwise; the guide (the spec)
  is correct in two places and the delivered diff follows the guide.
- **No `pendingVote` computed** was added, per the resolved D-10 description
  drift.

### Cross-phase bookkeeping triggered by task-170

- **task-109** (slice-1, "Redact governance vote targets in filterLogData") was
  **re-promoted from `complete` back to `verified`**: its AC-2 gap — the raw
  `wallets` / `legacyWallets` arrays reaching the log file on the 5 s
  `WalletsStore` poll — is exactly what task-170 closed, with proof from a
  different module at a call boundary task-109 never touched.
- **task-111** (slice-1, the sanitization spy suite) had its recorded caveat
  **cleared**: the suite's call-boundary cases previously covered `delegateVotes`
  and the two hardware-wallet paths only; task-170 AC-4 added the missing
  `getWallets` case, and task-111's reusable module-scope `jest.mock` harness
  pattern held for a fourth time.

### OWED at close (nothing here is faked green)

1. **Human visual / overflow pass in a running app** — one browser session in the
   main checkout, no row needed, owner is whoever runs the cv-1 visual pass.
   There is no browser in this devcontainer. It covers two open acceptance
   halves: **task-133 AC-1** (all four knobs render in en-US AND ja-JP via the
   global locale toggle, no console errors, no missing-message warnings) and
   **task-135 AC-2's overflow half** (`Governance / Current Vote Summary → Core
   states`, locale switched to Japanese, all four knob values, every string
   rendering fully with no clipping — `noDelegation.subline` at 58 characters is
   the likeliest clipper). The marker half of AC-2 is met and machine-verified.
2. **`nix fmt` pre-merge pass (F-5)** — `nix` is not installed in this
   devcontainer. The substitute used throughout was
   `node_modules/.bin/prettier --check` on explicit paths, and only on files this
   slice created. Owed before merge.
3. **Release-end `!!!` copy review** — user-owned by invariant 11 and never a
   per-slice task. cv-1 enlarged its surface: the strip now touches 12 more
   values than previously counted, and each is pinned by a committed snapshot and
   by exact-text matchers, so the strip is a code change, not a copy edit.
4. **Guide/tracker prose drift left unreconciled by design** — the task-135
   tracker description still credits it with creating
   `CurrentVoteSummary.messages.ts`; the task-171 acceptance bullet still states
   an `i18n:manage`-clean condition its own ordering makes unreachable at that
   row (discharged at task-135); the guide's task-170 Step 3 anchors are
   pre-change (`api.ts` is 8 lines shorter) and its audit grep is under-inclusive
   in two ways. Rewording an approved acceptance criterion is a phase-owner
   judgment call, so these were recorded rather than edited.
5. **`yarn check:all` and `yarn storybook:build` were never run as gates** — both
   are red at HEAD for the unrelated storybook manager-side JSX loader reason
   (F-20). Not valid cv-1 gates.
6. **No `auditSummary`** was added to the cv-1 phase object: only `slice-1`
   carries that field in this tracker, so cv-1 matches the convention of the ten
   other phases. Verified, not assumed.

### Residual gaps a later phase inherits

- **F-15 — task-170 explicitly does NOT close it.** `filterLogData`'s
  `sensitiveData` list is keyed to the WIRE shape (`drepId`, `dRepId`, `vote`,
  `voting`). The renderer-side domain names `votingTarget` and `currentVote` are
  absent from it, so the domain shape is unguarded the moment a consumer logs
  one. **Owned by the first cv-2 consumer.**
- **F-26 (new, recorded at task-170)** — `api.ts:1995` `importWalletFromKey` and
  `:2025` `importWalletFromFile` log `{ importedWallet }` unwrapped and are
  Shelley-`AdaWallet`-typed. Pre-existing and byte-identical at HEAD, type-level
  rather than demonstrated (both POST Byron/V0 legacy import endpoints, neither
  is on a poll), and structurally invisible to the guide's audit grep. For the
  reviewer to accept or schedule.
- **`HardwareWalletsStore`'s `[HW-DEBUG]` raw `{ error }` sink** — the
  message-SUBSTRING leak class, which a key-based redactor structurally cannot
  reach. Still open, fenced out of task-170 by both the guide and the tracker.
- **i18n key-DELETION blind spot (F-25)** — the `!!!` guard filters on `key in
  ja`, react-intl silently falls back to the `defaultMessage`, and
  `jest.config.js` promotes no console error to a failure, so a locale-only
  deletion ships silently between `i18n:manage` runs. Self-healing but unowned;
  the cheap fix is a key-set symmetry assertion inside the existing
  `preliminaryCopyMarkers` suite, not a new file.
- **Guard asymmetry (task-171, by design)** — a key minted with no `!!!` in
  en-US at all is never flagged. Low risk today because en-US markers derive from
  each component's source `defaultMessage`. cv-2's task-146 and anchor-2's copy
  authors should know the direction the guard does not cover.
- **cv-2's copy mint procedure is now fixed by precedent** (F-25): define
  messages in source first, let the runner seed both catalogs, replace only the
  ja-JP *values* by hand, keep every `!!!`, and never hand-edit an en-US value
  away from its `defaultMessage` while a snapshot bakes the fallback in.
- **What cv-2 builds on:** `Wallet.currentVote` / `isVoting`, the
  `WalletVotingTarget` shape, `normalizeDRepIdentity`, and `CurrentVoteSummary`'s
  four core states — all landed and green. The live status badge,
  `VotingPowerDelegation` pre-fill, and same-vote prevention remain cv-2, exactly
  as scoped here.

## References

- Parent plan: `governance-drep-discovery-plan.md` (:152 Key Decisions
  wire-literal row — reconciled; :153-158 sibling decisions; :174/:196 fixture
  pin; :253-259 delegation integration; :265 sequencing note; :284/:295 Track V).
- Task tracker: `governance-drep-discovery-plan-tasks.json` :804-995 (cv-1
  phase), :16/:1729-1730 (metadata).
- Design: `designs/current-vote-display-design.md`; UX:
  `designs/current-vote-display-ux.md`; Research: `research/external-research.md`.
- Findings: `research/cv-1-findings.md` (F-1 attribution, F-2 counts, F-3 test
  naming).
- Live-code anchors: `source/renderer/app/domains/Wallet.ts:42,172-200,245`;
  `source/renderer/app/api/wallets/types.ts:84`;
  `source/common/types/governance.types.ts:20-31`;
  `source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx:18`.
