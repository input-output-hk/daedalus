# Governance — Current Vote Display UX

**Plan:** [../../../plans/governance/governance-drep-discovery-plan.md](../../../plans/governance/governance-drep-discovery-plan.md) (`cv-1` / `cv-2`)
**Tasks:** [../../../plans/governance/governance-drep-discovery-plan-tasks.json](../../../plans/governance/governance-drep-discovery-plan-tasks.json) (`task-126`..`task-148`, plus slice-1 sanitization floor `task-109`..`task-111`)
**Design:** [current-vote-display-design.md](./current-vote-display-design.md)
**Scope:** renderer-only • no new IPC • no hardware-wallet signing changes
**Status:** draft, pending architecture + product review

---

## 1. Overview

This document specifies the UX for surfacing the selected wallet's current governance delegation: no delegation, DRep, Abstain, and No Confidence. The feature consumes data already returned by cardano-wallet's `delegation.active.voting` and uses CIP-1694 liquid-democracy semantics: the newest on-chain vote delegation is current.

The UI reuses the existing presentational primitives under [source/renderer/app/components/governance/_shared/](../../../../source/renderer/app/components/governance/_shared/) (`DRepIdDisplay`, `DRepSourceLabel`, `DRepStatusBadge`) and adds one presentational component, `CurrentVoteSummary`, plus Storybook coverage across the production and dedicated-Governance-section layouts.

## 2. User stories addressed

| Story | Title | UX intent |
|---|---|---|
| 3.1 | See current DRep delegation | Render a clearly labelled "Current delegation" panel above the form showing DRep name, source label, id, and anchor links as soon as the wallet is selected. |
| 3.2 | See current Abstain delegation | Render Abstain as a first-class state with an explicit icon + label, no id, and caption confirming rewards can be withdrawn. |
| 3.3 | See current No Confidence delegation | Mirror 3.2 with No Confidence framing and copy; semantically distinct from Abstain. |
| 3.4 | Prevent re-submitting the same delegation | Disable submit + show a tooltip explaining the wallet already delegates this way; keep the server `same_vote` error as the silent fallback. |
| 3.5 | Switch delegation — show "Previous -> New" (deferred) | **Deferred beyond v1.** `task-141` / `task-142` lock the v1 confirmation dialog to the current target only. The Previous / New comparison is captured here as a future enhancement, not a v1 deliverable. |
| 3.6 | Storybook five-value `currentVote` knob | Make every state reviewable in Storybook across all four governance layouts. |
| 3.8 | Hardware-wallet rendering without device | Render the summary from cached wallet state with no device required; hardware-wallet prompts only when the user attempts to change the delegation. |

## 3. Information architecture

`CurrentVoteSummary` sits above the `VotingPowerDelegation` form in every layout, between the wallet picker and the vote-type controls. It is never modal, never collapsible in Phase 1, and reads only from the selected `Wallet` instance (`currentVote`).

### 3.1 Production layout — `VotingPowerDelegation`

```text
┌─────────────────────────────────────────────────────────────┐
│  Governance                                                 │
├─────────────────────────────────────────────────────────────┤
│  [ Wallets dropdown v ]                                     │
│                                                             │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  CurrentVoteSummary                                   │  │
│  └───────────────────────────────────────────────────────┘  │
│                                                             │
│  ◉ DRep    ○ Abstain    ○ No Confidence                     │
│  [ DRep id input ....................................... ]  │
│                                                             │
│                                       [  Delegate vote  ]   │
└─────────────────────────────────────────────────────────────┘
```

## 4. No-delegation state

```text
┌─ No governance delegation ─────────────────────────────────┐
│  ! Your staking rewards cannot be withdrawn until you       │
│    delegate this wallet's voting power to a DRep, Abstain,  │
│    or No Confidence.                                       │
│                                                            │
│  Daedalus will not pick a DRep for you — choose how you     │
│  want your voting power to participate in Cardano           │
│  governance.                                                │
│                                                            │
│  [ Choose a delegation ]                                    │
└────────────────────────────────────────────────────────────┘
```

The panel always renders in this state. It warns about the CIP-1694 reward-withdrawal gate and makes the user action explicit. The CTA is a button in the mock; production can wire it to focus the vote-type controls or open the DRep selector.

## 5. Current delegation states

### DRep

```text
┌─ Current delegation ───────────────────────────────────────┐
│  [● Delegated to DRep]  SIPO  [Verified]  [● Active]       │
│  drep120m...7zu7l / drep_vkh1...                           │
│                                                            │
│  View details     Anchor metadata ↗                         │
└────────────────────────────────────────────────────────────┘
```

`body.givenName` is rendered prominently when verified. The id row remains visible because identity equality is still the safety anchor for delegation and hardware-wallet review.

The trailing badge on the header row is the DRep's live status badge (from shared-design-tokens §1): `Active`, `Inactive`, `Expiring in {n} epochs`, or `Retired`. It is required so the user can tell at a glance whether their existing delegation is still effective. When the status is `Inactive` or `Expiring in {n} epochs`, a single-line caption appears below the id row using the copy in §11 (`voting.governance.currentVote.status.expiring` / `.inactive`). If `GovernanceStore` has not yet loaded the cohort for this network, the badge is omitted and a neutral `voting.governance.currentVote.status.unavailable` caption is shown — never a default-to-Active fallback.

### Abstain

```text
┌─ Current delegation ───────────────────────────────────────┐
│  [⊘ Abstain]                                                │
│  Your stake is recorded on chain as not participating in    │
│  governance. Rewards can be withdrawn.                      │
└────────────────────────────────────────────────────────────┘
```

### No Confidence

```text
┌─ Current delegation ───────────────────────────────────────┐
│  [✕ No Confidence]                                         │
│  Your stake counts as Yes on every motion of no-confidence. │
│  Rewards can be withdrawn.                                 │
└────────────────────────────────────────────────────────────┘
```

## 6. Anchor links

Each DRep card has a compact link footer:

- **View details**: in-app route such as `#/governance/dreps/<cip129>`.
- **Anchor metadata ↗**: raw CIP-119 anchor URL, opened with `target="_blank" rel="noopener noreferrer"`.

The panel never inline-renders the anchor JSON. Full anchor rendering, validation details, and field-by-field CIP-119 content belong in the DRep detail flow.

## 7. Form pre-fill behaviour

| Wallet state | Pre-fill source | Baseline shown in CurrentVoteSummary |
|---|---|---|
| `noDelegation` | none; form starts blank | warning + nudge |
| `currentVote` only | `currentVote` -> `selectedVoteType` + `drepInputState.value` | current delegation |

Editing never rewrites `CurrentVoteSummary`; the summary is bound to wallet state, not form state. Switching wallets re-runs the pre-fill rule above.

## 8. Same-vote prevention copy

`submitButtonDisabled` is extended with `isSameAsCurrent`, computed after canonical normalization via `normalizeDRepIdentity`.

| Condition | Tooltip / hint (en-US) | i18n key |
|---|---|---|
| `selected === current` for DRep | `This wallet already votes for this DRep.` | `voting.governance.currentVote.sameVoteHint` |
| `selected === current` for Abstain | `This wallet is already set to Abstain.` | `voting.governance.currentVote.sameVoteHint` |
| `selected === current` for No Confidence | `This wallet is already set to No Confidence.` | `voting.governance.currentVote.sameVoteHint` |

The server-side `same_vote` error remains the safety net when the wallet snapshot changes before the renderer refreshes.

## 9. Confirmation dialog — Previous -> New framing (deferred beyond v1)

> **Status: deferred beyond v1.** Per `task-141` and `task-142` the v1 confirmation dialog renders the selected current target with today's single-row layout and introduces no historical comparison rows. The two-row design below is retained as a future-phase reference only.

When `previousVote` is passed through from the container and is non-null, `VotingPowerDelegationConfirmationDialog` renders a two-row comparison above the fee + passphrase / hardware-wallet status section:

```text
Previous vote     drep1mock...      [Unverified]
New vote          SIPO              [Verified]
```

When `previousVote` is null, the dialog renders today's single-row layout. Both rows truncate long DRep ids via `DRepIdDisplay`'s existing copy-and-truncate behaviour.

## 10. Hardware-wallet UX

The summary renders without a connected device because the data comes from cached wallet polling, not the hardware wallet. Same-vote prevention fires before any hardware-wallet gating opens. A changed delegation follows the existing confirmation flow for disconnected, locked, app-not-open, rejected, and timeout states.

## 11. i18n inventory

### Existing / carried-forward keys

| Key | en-US source copy |
|---|---|
| `voting.governance.currentVote.headerCurrent` | `Current delegation` |
| `voting.governance.currentVote.statusDelegatedToDRep` | `Delegated to DRep` |
| `voting.governance.currentVote.statusAbstain` | `Abstain` |
| `voting.governance.currentVote.statusNoConfidence` | `No Confidence` |
| `voting.governance.currentVote.sameVoteHint` | `This wallet already votes {target, select, drep {for this DRep} abstain {Abstain} no_confidence {No Confidence} other {the same way}}.` |
| `voting.governance.confirmationDialog.previousVote` | `Previous vote` |
| `voting.governance.confirmationDialog.newVote` | `New vote` |

> The `confirmationDialog.previousVote` / `.newVote` keys are reserved for the deferred Previous→New comparison (see §9) and are not wired in v1.

### New keys

| Key | en-US source copy |
|---|---|
| `voting.governance.currentVote.noDelegation.title` | `No governance delegation` |
| `voting.governance.currentVote.noDelegation.warning` | `Your staking rewards cannot be withdrawn until you delegate this wallet's voting power to a DRep, Abstain, or No Confidence.` |
| `voting.governance.currentVote.noDelegation.subline` | `Daedalus will not pick a DRep for you — choose how you want your voting power to participate in Cardano governance.` |
| `voting.governance.currentVote.noDelegation.cta` | `Choose a delegation` |
| `voting.governance.currentVote.drep.viewDetails` | `View details` |
| `voting.governance.currentVote.drep.anchorMetadata` | `Anchor metadata ↗` |
| `voting.governance.currentVote.status.expiring` | `This DRep's voting power will lapse in {n} epochs — consider re-delegating.` |
| `voting.governance.currentVote.status.inactive` | `This DRep is currently inactive. Your voting power will not be counted until they vote again — consider re-delegating.` |
| `voting.governance.currentVote.status.unavailable` | `DRep status is loading.` |
| `voting.governance.currentVote.abstain.caption` | `Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.` |
| `voting.governance.currentVote.noConfidence.caption` | `Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.` |

All ja-JP strings require translator review in Storybook across the five `currentVote` knob values and all four layouts.

## 12. Accessibility

| Concern | Requirement |
|---|---|
| Color-only signalling | Badges pair color with glyph + text (`●`, `⊘`, `✕`). The no-delegation warning uses both color and explicit text. |
| Screen-reader labels | Abstain and No Confidence badges must expose readable labels. |
| DRep id read-out | `DRepIdDisplay` exposes the full id via existing copy/read-out affordances. |
| Focus order | Interactive elements are ordered: view details link, anchor link, vote-type radios, DRep input, submit button. |
| External links | Anchor links include `target="_blank" rel="noopener noreferrer"`. |
| Disabled-submit tooltip | Tooltip text is exposed via `aria-describedby`; the button stays focusable with `aria-disabled="true"`. |

## 13. Storybook knob spec

Every governance story exposes one **Current vote (mock)** knob with exactly five values:

| Option `value` | Label shown in knob | Visual state |
|---|---|---|
| `noDelegation` | `Not delegated (warning)` | no-delegation warning + nudge |
| `drepVerified` | `DRep — verified anchor` | SIPO mainnet fixture with verified source and anchor links |
| `drepUnverified` | `DRep — unverified anchor` | canonical CIP-119 example fixture with unverified source |
| `abstain` | `Abstain` | Abstain badge + rewards-withdrawable caption |
| `noConfidence` | `No Confidence` | No Confidence badge + rewards-withdrawable caption |

`makeGovernanceWallets` is a pure factory. Each call returns a fresh `Wallet[]`; no mutation of `GOVERNANCE_WALLETS` module-level state. The knob value is fed as `key={option}` into `VotingPowerDelegation` so its local form state cannot leak across knob changes.

## 14. References

- Current Vote Display tech design: [current-vote-display-design.md](./current-vote-display-design.md)
- Governance DRep Discovery plan: [../../../plans/governance/governance-drep-discovery-plan.md](../../../plans/governance/governance-drep-discovery-plan.md)
- Storybook current-vote mock: [../../../../storybook/stories/governance/_utils/CurrentVoteMock.tsx](../../../../storybook/stories/governance/_utils/CurrentVoteMock.tsx)
- CIP-1694 (liquid democracy, vote delegation semantics, reward-withdrawal gate): https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md
- CIP-119 test vector spec: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/test-vector.md
- CIP-119 canonical example (`drep.jsonld`): https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/examples/drep.jsonld
- Mainnet test vector — SIPO: https://sipo.tokyo/drep/SIPO.jsonld
- Preprod test vector — Cardano Academy: https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld
