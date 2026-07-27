# CV-1 PRD: Current-Vote Plumbing + CurrentVoteSummary Core States

> **Planning Status:** approved | **Date:** 2026-07-27 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `cv-1` — "Current-vote 1 - Plumbing + CurrentVoteSummary core" (riskLevel: medium)
> **Tasks:** task-126 … task-135 (10 tasks, all `pending`; cross-phase deps task-109/task-110 verified)
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
so all ten tasks are classified `autonomous`. Decisions D-1 … D-10 below close
every question planning raised.

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
| **task-135** — i18n core keys `voting.governance.currentVote.*` | `autonomous` | `CurrentVoteSummary.messages.ts` with the core-state keys (inventory below); `yarn i18n:manage` populates `en-US.json` / `ja-JP.json`; every new string keeps the leading `!!!` (invariant 11); ja-JP reviewed for overflow | No `sameVoteHint`, no `status.expiring/.inactive/.unavailable` (cv-2 task-146); `confirmationDialog.previousVote/.newVote` reserved-not-wired (ux :168); no `!!!` removal | task-132 |

## Acceptance Criteria (verbatim from governance-drep-discovery-plan-tasks.json)

### task-126 — Commit cardano-wallet voting/delegating fixtures (json :811-823)

- "Fixtures cover voting / delegating_and_voting / abstain / no_confidence."
- "All fixtures conform to ApiWallet shape from cardano-wallet v2026-05-11 swagger."
- "Each fixture is small (only the fields needed for mapper tests) and contains no real wallet ids."

### task-127 — Fix latent delegating_and_voting literal mismatch (json :826-841)

- "DelegationStatus union contains 'delegating_and_voting', not 'voting_and_delegating'."
- "WalletDelegationStatuses.VOTING_AND_DELEGATING === 'delegating_and_voting' (constant name preserved)."
- "Unit test asserts the constant equals the wire literal."
- "No remaining 'voting_and_delegating' string literal in the renderer codebase (except possibly in changelog/migration notes)."

### task-128 — Widen WalletDelegation and WalletNextDelegation with voting field (json :844-859)

- "WalletDelegation and WalletNextDelegation expose voting?: WalletVotingTarget."
- "WalletVotingTarget discriminator is 'kind' with values 'drep' | 'abstain' | 'no_confidence'."
- "DRepIdentity contains raw (required) plus optional cip129/cip105/credentialHex."
- "DRepIdentity carries `credentialType` to prevent key/script collision in the same-vote comparator."

### task-129 — Implement normalizeDRepIdentity helper (json :862-880)

- "Pure function with no side effects."
- "Round-trips drep1 → cip105 → drep1 losslessly."
- "Rejects invalid bech32 input by returning null or throwing a typed error consistent with the rest of the renderer."
- "Unit tests cover all three prefixes plus invalid input."
- "Uses `bech32.decode` (or equivalent) that preserves the HRP — must distinguish `drep`, `drep_vkh`, `drep_script` even when the 28-byte payload is identical."
- "Returns `DRepIdentity` carrying `credentialType: 'key' | 'script'` so the same-vote comparator does not falsely equate a key DRep and a script DRep with the same hash."
- "Reuses an existing bech32 helper from the renderer bundle (e.g. `@cardano-sdk/core` or the existing `bech32` dependency) — no new direct dependency added to package.json."

### task-130 — Extract current vote in _createWalletFromServerData with explicit collision rules (json :883-900)

- "For status === 'voting' fixture, delegatedStakePoolId === null and votingTarget is populated."
- "For status === 'delegating_and_voting' fixture, both delegatedStakePoolId and votingTarget are populated."
- "Existing delegating / not_delegating mappings are byte-identical to today's behaviour."
- "When status === 'voting' and active.voting is absent, votingTarget is null — never parses active.target (which is a pool id)."
- "`DRepIdentity` shape does NOT include `givenName` or `anchorUrl`. These fields are removed. The mapper is not responsible for hydrating anchor-derived display values."

### task-131 — Extend Wallet domain model with currentVote / isVoting (json :903-918)

- "Wallet.currentVote returns the parsed WalletVotingTarget or null."
- "isVoting === true iff currentVote !== null."
- "Unit tests cover all four delegation statuses plus pending."
- "Wallet.update() pick list explicitly includes `votingTarget` (and the new computeds' dependencies) so polled wallet refreshes propagate the new vote target — preventing the well-known \"stuck stale value\" Daedalus pitfall."

### task-132 — Implement CurrentVoteSummary core states (no live status badge) (json :921-940)

- "Renders DRep id (via DRepIdDisplay) + source label (via DRepSourceLabel)."
- "When the wallet has no governance delegation, renders the reward-withdrawal warning and CTA instead of hiding the panel."
- "DRep state renders the DRep id ONLY in this slice; verified body.givenName and the external anchor URL link are explicitly OUT of scope here and render only after the hardened/verified anchor pipeline lands (anchor-1/anchor-2), preventing display of unverified anchor-derived identity. The in-app view-details link is gated on slice-4 (task-116) and added in cv-2."
- "Handles abstain and no_confidence kinds with no DRep id rendered."
- "Live active/inactive/expiring status badge is explicitly out of scope here and lands in cv-2 (task-136)."
- "If DRepSourceLabel cannot localize the new abstain / noConfidence / delegatedToDRep labels with its existing prop contract, wrap it in a CurrentVoteSummary-local renderer that consumes react-intl directly. Do NOT silently fall back to English literals."

### task-133 — Add CurrentVoteSummary Storybook entry (core knobs) (json :943-956)

- "Four core knob values (noDelegation | drepUnverified | abstain | noConfidence) render without console errors."
- "Story renders in both en-US and ja-JP locales without overflow."

### task-134 — Unit tests: mapper, Wallet computeds, and CurrentVoteSummary core snapshots (json :959-975)

- "All five mapping cases pass."
- "voting-only fixture asserts delegatedStakePoolId === null."
- "Wallet computeds covered for every WalletVotingTarget kind plus null."
- "CurrentVoteSummary core-state snapshots cover noDelegation / drepUnverified / abstain / noConfidence."

### task-135 — Add i18n keys for CurrentVoteSummary core states (json :978-992)

- "Core keys present in en-US.json and ja-JP.json: headerCurrent, statusDelegatedToDRep, statusAbstain, statusNoConfidence, noDelegation title/warning/subline/cta, DRep link labels, abstain/noConfidence captions."
- "Preliminary ja-JP copy is reviewed for length / layout overflow while retaining the leading !!! marker."
- "yarn i18n:manage runs clean."

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

- **(2) Sanitization floor** — touched by task-130 (warning path) and re-asserted
  by task-134. No DRep id, no abstain/no_confidence literal, no CIP-129/CIP-105
  bech32 string in any logger, analytics, or electron-store payload; the
  task-111 spy suite `tests/jest/security/governance-sanitization.spec.ts` must
  be re-asserted green in this slice. Fixtures/docs MAY contain DRep ids — the
  floor binds runtime logging/analytics/store paths only.
- **(9) No auto-delegation** — touched by task-132/135. The `noDelegation` state
  shows the CIP-1694 reward-withdrawal warning + CTA; Daedalus never picks a
  delegation.
- **(10) Byte-equality** — touched by task-129/130. CIP-129, CIP-105, and the
  signed payload `vote.id` remain byte-equal through every identity-display
  change; `normalizeDRepIdentity` must round-trip losslessly.
- **(11) `!!!` markers** — touched by task-135. Every new en-US and ja-JP string
  keeps the leading `!!!`; removal is release-end manual review only.
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
  depends on {130, 131}; 130 also depends on 129; 132 also depends on task-109 ✔
  and task-110 ✔ (both verified — `DRepIdDisplay` / `DRepSourceLabel` exist under
  `source/renderer/app/components/governance/_shared/`).
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

- All 42 verbatim acceptance criteria above pass.
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

_Placeholder — filled at slice close (task outcomes, review verdicts, deviations
from this PRD, and the closing planner entry)._

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
