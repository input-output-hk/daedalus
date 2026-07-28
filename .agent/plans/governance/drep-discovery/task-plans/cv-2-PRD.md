# CV-2 PRD: Current-Vote Enrichment (live badge, pre-fill, same-vote prevention, §7 identity block)

> **Planning Status:** in_review | **Slice Status:** not started | **Date:** 2026-07-28 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `cv-2` — "Current-vote 2 - Enrichment" (riskLevel: medium; tasks JSON `:1162-1457`)
> **Tasks:** 15 — task-136, task-137, task-138, task-139, task-140, task-173, task-141, task-142, task-175, task-143, task-144, task-145, task-146, task-147, task-148 (all `pending` at HEAD)
> **Preceding slice:** [cv-1-PRD.md](./cv-1-PRD.md) (closed 2026-07-28)
> **Findings:** [research/cv-2-findings.md](../research/cv-2-findings.md) — written during cv-2 planning, carrying F-1 … F-14
> **Implementation guide:** `cv-2-implementation-guide.md` (authored after this PRD)
> **Anchors:** every `path:line` in this document was opened and verified in the worktree at branch `feat/drep-discovery`, commit `504b44c1a`. Where the tasks JSON or a design doc cites an anchor that no longer matches, the drift is called out in **Corpus-vs-Repo Corrections** and the live repo wins (prompt.md:39-41).

---

## Executive Summary

cv-1 shipped the plumbing: `Wallet.votingTarget` / `currentVote`, the pure
`normalizeDRepIdentity` helper, and `CurrentVoteSummary`'s four core states.
cv-2 turns that read-only panel into a working governance surface:

- **Live lifecycle badge.** `CurrentVoteSummary`'s `drep` state gains the
  delegated DRep's `Active` / `Inactive` / `Expiring in {n} epochs` badge plus
  the three consequence captions, sourced from `GovernanceStore.drepIndex`
  (`GovernanceStore.ts:100`) with **no new IPC and no cardano-cli invocation**
  (task-136).
- **The form learns the current state.** `VotingPowerDelegation` stops caching a
  `Wallet` object (task-137), pre-fills from `wallet.currentVote` through an
  explicit fallback chain (task-138), mounts `CurrentVoteSummary` above the form
  (task-139), and disables submit when the selection is byte-different but
  identity-identical to the current on-chain delegation (task-140).
- **The confirmation dialog stops guessing.** `VotingGovernancePage` replaces the
  `chosenOption.startsWith('drep_script')` heuristic
  (`VotingGovernancePage.tsx:79-81`) with `normalizeDRepIdentity` (task-173),
  keeps its v1 current-target-only prop set (task-141/task-142), and grows the
  identity block into the pre-anchor shared-design-tokens §7 template —
  CIP-129 primary, CIP-105 secondary, signed-payload hex, on-chain source label
  (task-175).
- **Storybook gets a shared, mutation-free fixture layer.** A new
  `storybook/stories/governance/_utils/` module supplies the five-value
  current-vote knob, a pure wallet factory and a drepIndex factory (task-143), a
  key-remounting wrapper (task-144), and the migration of the module-level
  `GOVERNANCE_WALLETS` at `storybook/stories/voting/Governance.stories.tsx:63-97`
  onto per-render factory calls (task-145).
- **Copy and coverage close the slice.** Five new `currentVote` keys plus
  task-175's two confirmation keys ship `!!!`-marked in both locales with the
  `preliminaryCopyMarkers` guard widened (task-146); focused Jest pins the flow,
  the HW path, the letter-case comparator vector and the logger/analytics floor
  (task-147); and the `same_vote` server error stays reachable behind the new
  client-side disable (task-148).

Everything is renderer-only. No new IPC channel, no new cardano-wallet endpoint,
no main-process edit, no signing-path change, no `VotingStore` → `GovernanceStore`
reference.

## Problem Statement — Why Now

- **The panel is inert.** cv-1's `CurrentVoteSummary` shows *who* a wallet
  delegates to but not *whether that delegation still works*. A DRep whose
  `drepActivity` has run out contributes nothing, and the user has no signal.
  `designs/shared-design-tokens.md:22` makes the badge on this panel **binding**:
  "users must be able to tell at a glance whether the DRep they currently
  delegate to is still active or about to lapse."
- **The form contradicts the panel.** `VotingPowerDelegation` starts blank on
  every wallet selection (`VotingPowerDelegation.tsx:231-237` resets to
  `initialState` unconditionally), so a user who already delegates sees their
  current target one line above an empty form and can re-submit the identical
  delegation, paying a fee for a no-op until the server returns `same_vote`.
- **The confirmation dialog mislabels script DReps.** The heuristic at
  `VotingGovernancePage.tsx:79-81` classifies every `drep1…` id as `'key'`, but
  `GovernanceQueryService._credentialToDRepId`
  (`GovernanceQueryService.ts:624-640`) emits CIP-129 for **both** keyHash and
  scriptHash credentials, so the mislabel is systematic, not incidental.
  `normalizeDRepIdentity` (shipped `complete` in cv-1) has **zero production call
  sites** today — task-173 creates the first one.
- **The prerequisites are all landed.** task-106 (`GovernanceStore`), task-109 /
  task-110 / task-111 (sanitization floor), task-113 (dialog identity prop),
  task-115 (HW delegate path), task-129 (`normalizeDRepIdentity`), task-131
  (`Wallet.currentVote`), task-132 (`CurrentVoteSummary` core), task-171 (ja-JP
  `!!!` guard) are `complete`/`verified`. cv-2 is the last Track V slice before
  Track A (`anchor-1`), and `anchor-2`'s `givenName` swap extends surfaces cv-2
  builds.

## Canonical Build Order

The tasks JSON listing order for cv-2 is **`136, 137, 138, 139, 140, 173, 141,
142, 175, 143, 144, 145, 146, 147, 148`**. Verified against every row's
`dependencies` field — every in-slice dependency appears earlier in the listing,
so the JSON order is dependency-valid end to end:

| # | task | `dependencies` (json) | in-slice deps satisfied earlier? |
|---|---|---|---|
| 1 | task-136 | task-132 ✔, task-106 ✔ | n/a (both cross-slice, complete) |
| 2 | task-137 | task-132 ✔ | n/a |
| 3 | task-138 | task-132 ✔, **task-137** | yes (#2) |
| 4 | task-139 | **task-138**, **task-136** | yes (#3, #1) |
| 5 | task-140 | **task-138**, task-129 ✔ | yes (#3) |
| 6 | task-173 | task-129 ✔, task-113 ✔ | n/a |
| 7 | task-141 | **task-140**, **task-173** | yes (#5, #6) |
| 8 | task-142 | **task-141** | yes (#7) |
| 9 | task-175 | **task-173**, task-113 ✔, task-115 ✔, task-129 ✔ | yes (#6) |
| 10 | task-143 | task-131 ✔ | n/a |
| 11 | task-144 | **task-143** | yes (#10) |
| 12 | task-145 | **task-144**, **task-139** | yes (#11, #4) |
| 13 | task-146 | **task-136**, **task-142**, task-171 ✔ | yes (#1, #8) |
| 14 | task-147 | **task-140**, **task-142**, **task-145**, **task-146** | yes (#5, #8, #12, #13) |
| 15 | task-148 | **task-140**, task-109 ✔, task-110 ✔ | yes (#5) |

**One binding amendment (D-13).** task-143 is hoisted to position 1. Its only
dependency is task-131 (`complete`), so the hoist breaks no edge. The reason:
**task-136 AC-4** requires "`drepVerified` Storybook knob renders without console
errors in en-US and ja-JP", and `drepVerified` does not exist at HEAD —
`storybook/stories/governance/CurrentVoteSummary.stories.tsx:23-28` defines only
four options. Building task-136 before task-143 forces a throwaway local
knob edit that task-143/task-145 must then delete (the corpus explicitly warns
against appending a fifth entry to the local constant in place). Hoisting
task-143 lets task-136 consume the shared fixtures directly.

**Canonical cv-2 build order (binding):**

```
143 → 136 → 137 → 138 → 139 → 140 → 173 → 141 → 142 → 175 → 144 → 145 → 146 → 147 → 148
```

Ordering constraints that are *not* free and must not be re-derived:

- **137 before 138** — 138 AC-3's "does NOT cache the selected wallet object" is
  only expressible once selection is an id.
- **139 after 136 and 138** — 139 mounts the finished panel and needs both the
  `drepEntry` prop (136) and the derived selected wallet (137/138).
- **173 before 141** — stated in task-173's own description ("This must land
  before task-141, which edits the same container").
- **175 after 142** — see D-3; 142's assertions are authored to survive 175, and
  175 re-runs them as its own gate.
- **145 after 139** — the knob exercises the mounted panel.
- **136 + 140 + 175 before 146** — 146 populates catalogs from message
  descriptors those three rows mint (D-9).
- **140/142/145/146 before 147** — 147 is the slice's regression harness over all
  of them.

## Per-Task Contract (interaction mode, scope, non-goals, dependencies)

No cv-2 task is in the locked non-autonomous set (task-125, task-166 remainder,
task-158, the release-end `!!!` copy review — prompt.md:190-195). Planning
resolved every open decision O-1 … O-8 from the sources named in each; **no cv-2
task is escalated**, so all fifteen rows are `autonomous`. The one question a
user *could* be asked is recorded in D-5 as a non-blocking option, not a stop
condition.

| Task | Mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-143** — `currentVoteOptions` + pure wallet factory | `autonomous` | New `storybook/stories/governance/_utils/fixtures.ts` exporting `CurrentVoteOption`, `currentVoteOptions`, `useCurrentVoteKnob()`, `resolveCurrentVote(option)`, `makeGovernanceWallets(option)`, `makeDRepIndex(option)` (S-6). Constructs `new Wallet({…})` directly. Supersedes the inline block at `CurrentVoteSummary.stories.tsx:17-52` | Does **not** widen `generateWallet` (`storybook/stories/_support/utils.ts:104-142`); no module-level mutable wallet array; no anchor/CIP-119 content (none exists in cv-2 — D-7); no story wiring (task-145) | task-131 ✔ |
| **task-136** — Live DRep status badge in `CurrentVoteSummary` | `autonomous` | Add optional `drepEntry` prop (S-2); render the shared `DRepStatusBadge` for `active`/`inactive` and a **local** expiring badge for the derived `≤12` overlay (D-1, D-2); three captions; mint **four** message descriptors — `statusExpiringBadge`, `statusExpiring`, `statusInactive`, `statusUnavailable` (D-9); `sameVoteHint` is task-140's and must not be minted here; add the `drepVerified` + `DRep status (mock)` knobs to `CurrentVoteSummary.stories.tsx`; update `CurrentVoteSummary.spec.tsx:61-63` and the four snapshots | No `DRepStatusBadge`/`DRepStatus` widening; no store import inside the component (D-6); no IPC or cardano-cli fallback; no `givenName` (D-5); no catalog edit (task-146) | task-132 ✔, task-106 ✔ |
| **task-137** — `selectedWalletId` state | `autonomous` | Replace `selectedWallet: Wallet` in `FormData`/`Form` (`VotingPowerDelegation.tsx:53-66`) with `selectedWalletId: string \| null`; derive `const selectedWallet = wallets.find(w => w.id === state.selectedWalletId) ?? null` on every render; update the seven read sites (`:136`, `:174`, `:239`, `:244`, `:260`, `:286`, `:333`) | No store-backed form state; no change to `chosenOption` derivation (`:160-163`); no pre-fill logic (task-138) | task-132 ✔ |
| **task-138** — Pre-fill from current on-chain delegation | `autonomous` | Extract the seed into `deriveFormSeed(wallet, inheritedDRepId)` (S-3); apply it in the lazy `useState` initializer (`:115-131`) **and** in `WalletsDropdown.onChange` (`:231-237`), replacing the unconditional `{...initialState, selectedWallet}` reset; extend the `WalletsDropdown` mock at `VotingGovernancePage.spec.tsx:34-38` with `onChange`; add the directory-select-then-pick-wallet regression | No auto-delegation for `noDelegation` (invariant 9); no trim/re-encode of the inherited or current id (invariant 10); no `GovernanceStore` read from `VotingStore`; no "data changed" banner (AC-3's parenthetical alternative is not taken — D-11) | task-132 ✔, task-137 |
| **task-139** — Mount `CurrentVoteSummary` | `autonomous` | Render `<CurrentVoteSummary currentVote={selectedWallet?.currentVote ?? null} drepEntry={…} />` between `WalletsDropdown` (`:242`) and the vote-type `ItemsDropdown` (`:244`), unconditionally (including `noDelegation`); add the `drepIndex` prop to `VotingPowerDelegation`; **cross-targetPath**: destructure `governance` in `VotingGovernancePage.tsx:38-39` and pass `governance.drepIndex` down | No `givenName` read and no unverified→verified story (**AC-3 split — D-5**); no wallet re-poll; no store read inside `CurrentVoteSummary` | task-138, task-136 |
| **task-140** — Disable submit on identical delegation | `autonomous` | New pure `source/renderer/app/utils/governance/isSameVoteTarget.ts` (S-4) + its spec; delete `submitButtonDisabled` from `:139-143` and re-declare it (with `isSameAsCurrent`) after the `chosenOption` derivation at `:160-163`, leaving `formIsValid` (`:135-137`) in place (S-4); render a visible inline `sameVoteHint` paragraph wired via `aria-describedby` (D-10); mint the `sameVoteHint` descriptor; **doc half**: re-anchor + append the two comparator corrections (D-4) | Comparator mutates nothing (invariant 10); never keys on `raw`/`cip129`/`cip105`; the server `same_vote` net stays reachable (task-148); does **not** rewrite `designs/current-vote-display-design.md:97`'s case-insensitive alternative (D-4); the letter-case vector belongs to task-147 | task-138, task-129 ✔ |
| **task-173** — Identity via `normalizeDRepIdentity` | `autonomous` | Replace `VotingGovernancePage.tsx:75-83` with `normalizeDRepIdentity(chosenOption)`; sentinels still yield `null`; null-decode still renders `chosenOption` verbatim; replace `toStoryDRepIdentity` (`Governance.stories.tsx:58-61`); add the 0x23-script assertion to `VotingGovernancePage.spec.tsx` | No re-encoding of `chosenOption`; no new logging on either branch; AC-5's design-doc edit is **already discharged** — verify, do not re-edit (D-4) | task-129 ✔, task-113 ✔ |
| **task-141** — Dialog stays current-target only | `autonomous` | Verification row; **no production edit expected** (the container passes no historical prop at HEAD). Deliverable is an executable pin in `VotingGovernancePage.spec.tsx` asserting the exact prop set handed to `VotingPowerDelegationConfirmationDialog` | No `previousVote`/`newVote` prop; no store-backed comparison state; the reserved keys stay unwired (`current-vote-display-ux.md:168`) | task-140, task-173 |
| **task-142** — Dialog unchanged for current-vote display | `autonomous` | Verification row over `VotingPowerDelegationConfirmationDialog.tsx`. Pins the **fee rows (`:174-177`), the HW-status branch (`:179-185`), and the passphrase input (`:186-202`)** by role/label — never a whole-dialog snapshot (D-3) | Explicitly **excludes** the identity block `:151-172`, which task-175 is chartered to grow (`shared-design-tokens.md:135`); AC-3's "~L118-L127" anchor is replaced with the semantic one (D-3) | task-141 |
| **task-175** — Pre-anchor §7 identity block | `autonomous` | Grow `:151-172` into the four-part template (`shared-design-tokens.md:114-120`): CIP-129 primary under the existing `.drepId` label, CIP-105 secondary when derivable, signed-payload line carrying `credentialHex`, `<DRepSourceLabel source="on-chain" />`; two new message descriptors; Jest decode-equality + HW-hex assertions | Renders the **pre-anchor** template only — no `givenName`, no `Name: Verified off-chain content` (task-154, anchor-2); no `DRepIdDisplay` dual-form mode (anchor-2); no re-derivation in the dialog (identity comes from task-173); sentinels render no identity block | task-173, task-113 ✔, task-115 ✔, task-129 ✔ |
| **task-144** — `GovernanceWrapper` key remount | `autonomous` | New `storybook/stories/governance/_utils/GovernanceWrapper.tsx`, default-exported render-prop component applying `key={option}` and supplying `{ wallets, drepIndex }` (S-6) | Not a Storybook decorator (the sibling `WalletsWrapper.tsx` shape cannot pass fixtures to the story body — D-8); no knob reading inside the wrapper; storybook-only, no source import | task-143 |
| **task-145** — `currentVote` knob on the governance stories | `autonomous` | Wire `useCurrentVoteKnob()` + `GovernanceWrapper` into the three wallet-bearing stories in `storybook/stories/voting/Governance.stories.tsx`; migrate `GOVERNANCE_WALLETS` (`:63-97`) and **all four** reuse sites (`:233`, `:420`, `:457`, `:492`) to per-render factory calls | Scope is the stories that render a current vote (D-8); does **not** register the three unregistered governance story files in `storybook/stories/index.ts:17-18` (D-12); no per-locale story variants and no local `IntlProvider`; no story-id renames | task-144, task-139 |
| **task-146** — Remaining i18n keys | `autonomous` | Run the cv-1 mint procedure over the five `currentVote` + two `confirmationDialog` descriptors minted by task-136/140/175; hand-write ja-JP values keeping `!!!`; extend `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` with the three assertions in D-14; `yarn i18n:manage` idempotent | Never strips a `!!!` (release-end, user-owned); never hand-edits an en-US value away from its `defaultMessage`; does not re-word the existing `confirmationDialog.drepId` copy (D-9) | task-136, task-142, task-171 ✔ |
| **task-147** — Jest current-vote regressions + HW path | `autonomous` | Extend `VotingGovernancePage.spec.tsx` (flow + HW), `tests/jest/governance/isSameVoteTarget.spec.ts` (letter-case vector), and `tests/jest/security/governance-sanitization.spec.ts` (logger/analytics spies over the new domain shapes) — see S-8 | No Cucumber / e2e (`README.md:45`); no new spy scaffolding where the task-111 suite's pattern already exists; no `.test.ts` filenames | task-140, task-142, task-145, task-146 |
| **task-148** — `same_vote` path regression | `autonomous` | Add the `same_vote` case to `source/renderer/app/stores/VotingStore.spec.ts` (`initializeVPDelegationTx` describe) plus a render assertion that the error copy still surfaces through `VotingPowerDelegation.tsx:304-308` | Touches none of the six `same_vote` sites (D-15) — it pins them; no change to `expectedInitializeVPDelegationTxErrors` (`VotingStore.ts:61-65`) | task-140, task-109 ✔, task-110 ✔ |

## Acceptance Criteria (verbatim from governance-drep-discovery-plan-tasks.json)

### task-136 — Add live DRep status badge to CurrentVoteSummary from drepIndex (json :1168-1186)

- "DRep state renders the delegated DRep's live active / inactive / expiring-in-{n}-epochs status badge from shared-design-tokens §1, sourced from GovernanceStore.drepIndex[drepId] (no new IPC / cardano-cli invocation issued by this component)."
- "When the DRep status is Inactive or Expiring, a single-line caption (voting.governance.currentVote.status.inactive / .expiring) appears below the id row."
- "When GovernanceStore has no record for the delegated DRep yet, the badge is omitted and a neutral voting.governance.currentVote.status.unavailable caption is shown — never a default-to-Active fallback and never a fallback IPC lookup."
- "drepVerified Storybook knob renders without console errors in en-US and ja-JP."

*Applied reading:* `drepIndex[drepId]` is a `Map`, so the read is `.get()` after
canonicalization (D-6); "sourced from" is satisfied through the prop chain, not a
component-level store import.

### task-137 — Replace selected Wallet object state with selectedWalletId (json :1187-1203)

- "VotingPowerDelegation stores selectedWalletId (string), not a Wallet object, in local React state."
- "The selected wallet is derived reactively from stores.wallets.all.find() against the latest wallets snapshot."
- "The previous Wallet-object-in-state pattern is removed so polling refreshes cannot strand stale currentVote data."

*Applied reading:* `VotingPowerDelegation` is presentational and receives
`wallets: Array<Wallet>` as a prop (`:35`), fed from `stores.wallets.all` at
`VotingGovernancePage.tsx:63`. AC-2's `stores.wallets.all.find()` is satisfied by
`wallets.find()` over that prop — introducing a store read inside the component
would break the container/presentational split and invariant 4's spirit.

### task-138 — Pre-fill VotingPowerDelegation from current on-chain delegation (json :1204-1225)

- "Selecting a wallet with currentVote pre-fills the form."
- "Selecting a wallet resets the form through a fallback chain — the wallet's `currentVote`, then the inherited `initialFormState.selectedDRepId` from `location.state`, then blank — instead of the unconditional `{ ...initialState, selectedWallet }` reset at VotingPowerDelegation.tsx:231-237."
- "Pre-fill is reactive: when a polling refresh changes `Wallet.currentVote`, the form re-seeds (or surfaces a \"data changed\" indicator) by deriving the selected wallet from `selectedWalletId` against the latest wallets snapshot. It does NOT cache the selected wallet object across re-renders."
- "A wallet with no `currentVote` reached from the DRep directory keeps the directory-supplied DRep ID byte-identical (no trim, no re-encoding, no re-normalization); only a wallet with neither a `currentVote` nor an inherited ID starts blank."
- "A Jest regression covers directory-select-then-pick-wallet: render with `location.state` carrying `selectedDRepId` and no `selectedWalletId`, drive the wallet dropdown's `onChange`, and assert the DRep input renders the same id — the display-only `WalletsDropdown` mock at VotingGovernancePage.spec.tsx:34-38 must be extended to expose `onChange`."
- "The handoff still travels only through React Router `location.state` and local React state: no `GovernanceStore` read from `VotingStore`, and no store-backed pending form state."
- "INHERITED sanitization floor: no DRep id, sentinel, or bech32 string reaches any logger or analytics payload, including the new regression's fixtures."

### task-139 — Render CurrentVoteSummary in VotingPowerDelegation (json :1226-1243)

- "CurrentVoteSummary always renders above the form, including noDelegation warning + nudge state."
- "Layout matches design (verified via Storybook in cv-1/cv-2)."
- "The `drep` state reads `givenName` from `GovernanceStore.drepIndex[drepId]?.givenName`. The panel updates reactively when `drepIndex` is populated or updated; no wallet re-poll is triggered. A Storybook story covers the transition from unverified to verified name."

**AC-3 is split by D-5.** Clause 2 (reactive update on `drepIndex`, no wallet
re-poll) is retained and built. Clause 1 (`givenName` read) and clause 3
(unverified→verified story) are **struck as unbuildable in cv-2** and deferred —
see D-5 for the phase that owns them and the tracker reconciliation that rides
with task-139's commit.

### task-140 — Disable submit on identical-to-current delegation (json :1244-1265)

- "Submit is disabled when the selected vote equals currentVote after normalization."
- "Disabled state shows the appropriate tooltip hint."
- "Existing same_vote server error remains reachable as a safety net."
- "isSameAsCurrent keys on the case-stable (credentialHex, credentialType) pair that normalizeDRepIdentity returns for both sides — never on raw, cip129 or cip105, whose letter case follows the user's input because normalizeDRepIdentity returns the input string untouched (normalizeDRepIdentity.ts:39-40, :56) while the form gate Cardano.DRepID.isValid (VotingPowerDelegation.tsx:133) accepts all-uppercase bech32."
- "Comparator behaviour when credentialHex is absent is explicit: credentialHex is optional on DRepIdentity (governance.types.ts:28) while credentialType is required, so two identities that both lack the hex are never equated and credentialType alone never establishes equality. The letter-case regression vector for this comparison is owned by task-147."
- "Byte-equality preserved: the comparison mutates nothing — the id reaching chosenOption and the delegateVotes dRepId is the form input string byte-for-byte, with no lower-casing, trimming or re-encoding."
- "The comparator sentence of designs/current-vote-display-design.md:95 no longer offers a canonical CIP-129 string as an acceptable comparison key, and the matching claim at task-plans/cv-1-code-review.md:736-738 is corrected by appending a correction entry rather than editing the line in place, since that file is append-only per the README working conventions."

*AC-7 re-anchored and scoped by D-4: the comparator sentence lives at
`designs/current-vote-display-design.md:97`, not `:95`.*

### task-173 — Build the confirmation-dialog DRep identity with normalizeDRepIdentity (json :1266-1286)

- "VotingGovernancePage derives the DRepIdentity via normalizeDRepIdentity(chosenOption); the startsWith('drep_script') heuristic is removed and the abstain / no_confidence sentinel branch still yields null (invariant 13)."
- "When normalizeDRepIdentity returns null — a string Cardano.DRepID.isValid accepts at the form gate (VotingPowerDelegation.tsx:133) but the decoder rejects, e.g. a legacy 28-byte drep1… id — the dialog still renders the raw string verbatim and submission is unaffected; nothing is re-encoded, trimmed, or dropped."
- "storybook/stories/voting/Governance.stories.tsx toStoryDRepIdentity uses the same helper instead of hardcoding credentialType: 'key', so story and container cannot drift."
- "VotingGovernancePage.spec.tsx asserts that a CIP-129 script DRep (0x23 header) yields credentialType: 'script', and the existing byte-equality assertions (row select -> confirmation -> delegateVotes payload) still hold unchanged."
- "The first sentence of designs/current-vote-display-design.md:95 is corrected so CIP-129 drep1… ids are classified by their header byte (0x22 key / 0x23 script) rather than by HRP — it is the documentary basis for the heuristic this task removes. The comparator sentence on the same line belongs to task-140, so the line is re-read at edit time."
- "INHERITED sanitization floor: the normalization path adds no logger or analytics payload carrying a DRep id or bech32 string, including on the null branch — re-asserted with the task-111 spy suite."

*AC-5 is **already satisfied at HEAD** by commit `2ee5f74cf` — verify, do not
re-edit (D-4).*

### task-141 — Keep confirmation dialog current-target only (json :1287-1303)

- "No historical vote-target prop is required for the v1 confirmation dialog."
- "Backward-compatible: dialogs behave as today when only the selected current target is provided."

### task-142 — Verify confirmation dialog remains unchanged for current-vote display (json :1304-1320)

- "Dialog renders the selected target with today's layout."
- "No historical comparison rows are introduced in v1."
- "HW status section (lines ~L118-L127) is untouched."

*AC-1 is scoped by D-3 to exclude the identity block; AC-3's line citation is
replaced by the semantic identification of the `selectedWallet.isHardwareWallet ?`
branch at `VotingPowerDelegationConfirmationDialog.tsx:179-185`.*

### task-175 — Render the pre-anchor §7 confirmation identity block (json :1321-1342)

- "The dialog renders the four parts of the pre-anchor §7 block — CIP-129 primary, CIP-105 secondary when derivable, the signed-payload line carrying the vote.id credential hex, and the on-chain source label via DRepSourceLabel variant 'on-chain' — with both bech32 forms full and monospaced and byte-untouched (no trim, re-encode, or case change)."
- "Secondary forms come only from the identity task-173 supplies; when normalizeDRepIdentity returned null — e.g. the legacy 28-byte drep1… form Cardano.DRepID.isValid accepts — only the primary line renders verbatim and no representation is fabricated."
- "Jest asserts the rendered CIP-129, CIP-105 and credential hex all decode to the same 28 credential bytes, that the primary line is byte-equal to the string handed to delegateVotes, and that the rendered hex equals the keyHashHex / scriptHashHex the hardware path sends to the device."
- "Abstain and No Confidence keep their sentinel labels with no identity block rendered (invariant 13), and the two new confirmation keys — the CIP-105 and signed-payload labels — ship !!!-prefixed in en-US and ja-JP with yarn i18n:manage idempotent; the existing voting.governance.confirmationDialog.drepId label carries the CIP-129 line."
- "INHERITED sanitization floor: the new CIP-105 and credential-hex renderings exist in the DOM only and appear in no logger, analytics, or electron-store payload."

### task-143 — Add currentVoteOptions and pure wallet factory in _utils/fixtures (json :1343-1360)

- "makeGovernanceWallets always returns a freshly-constructed array."
- "No knob handler mutates a pre-existing Wallet instance."
- "currentVoteOptions enumerates exactly five values: noDelegation | drepVerified | drepUnverified | abstain | noConfidence."
- "Anchor fixtures use CIP-119 test vectors: SIPO mainnet, Cardano Academy preprod, canonical CIP-119 example with verified hash."

*AC-4 is satisfied-in-part by D-7 on **both** halves. Named provenance: only the
`drepVerified` pair carries one of the three provenances the plan names — the
Cardano Academy preprod key hash committed at
`research/drep-state-preprod-epoch295-sample.json:2849` — while `drepUnverified`
is the repo's own story vector, and cv-2 mints no new credential. "Verified
hash": no mechanism exists in cv-2 (the fetch/hash-verify pipeline is anchor-1
task-149/task-150) and both entries ship `anchor: null`.*

### task-144 — Add key-based remount in GovernanceWrapper (json :1361-1376)

- "Wrapper exposes a key derived from the selected current-vote option id: noDelegation | drepVerified | drepUnverified | abstain | noConfidence."
- "Children consume the key so VotingPowerDelegation remounts on knob change."

### task-145 — Add currentVote knob to the governance stories (json :1377-1395)

- "Every governance story exposes the five-value Current vote knob with option ids noDelegation | drepVerified | drepUnverified | abstain | noConfidence."
- "No story mutates a module-level GOVERNANCE_WALLETS."
- "Existing storybook/stories/voting/Governance.stories.tsx is migrated from module-level GOVERNANCE_WALLETS to per-render makeGovernanceWallets(option) calls with no shared mutable wallet instances remaining."
- "Every knob value renders without console errors or layout overflow in both en-US and ja-JP."

*AC-1's "every governance story" is scoped by D-8 to the stories that can render a
current vote; AC-4's ja-JP visual pass is OWED (no browser in this container).*

### task-146 — Add remaining CurrentVoteSummary enrichment i18n keys (json :1396-1415)

- "Remaining keys present in en-US.json and ja-JP.json: sameVoteHint, status.expiring, status.inactive, status.unavailable."
- "Confirmation dialog copy remains compatible with the selected current target."
- "Preliminary ja-JP copy is reviewed for length / layout overflow while retaining the leading !!! marker."
- "yarn i18n:manage runs clean."

*Extended by D-9 with a fifth `currentVote` key (`status.expiringBadge`) and
task-175's two `confirmationDialog` keys, and by D-14 with the guard widening.*

### task-147 — Jest governance current-vote regressions + HW path (json :1416-1437)

- "Focused Jest regression suite is green."
- "HW path green for at least disconnected / locked / app-not-open."
- "A regression vector pins that a current vote and a selected vote differing only in bech32 letter case are treated as the same vote and submit stays disabled. This is the letter-case vector task-140's case-stable isSameAsCurrent criterion assigns to this task; it is the executable proof of that comparator and is not discharged by the generic same-vote cases above."
- "Logger and analytics spies confirm no leaked vote target."
- "INHERITED sanitization floor: no DRep id / 'abstain' / 'no_confidence' / CIP-129 / CIP-105 string appears in any logger or analytics payload."
  *(Scope-narrowed — see the invariant 2 carve-out and the Definition of Done
  exception table: the sentinel clause binds **logger** payloads only, because
  the analytics vote kind carries the literal by a reviewed task-110 decision.)*

### task-148 — Same-vote path regression (json :1438-1455)

- "Regression case for the same_vote server error path remains green."
- "VotingStore.expectedInitializeVPDelegationTxErrors.same_vote path is still reachable."

## Planning Decisions (binding, as applied)

### D-1 — The expiring badge is local; `active`/`inactive` reuse the shared badge. (resolves O-1)

`DRepStatusBadge` accepts exactly `DRepStatus`, and `DRepStatus` is a closed
two-value union at `source/common/types/governance.types.ts:35`. Invariant 14 and
`shared-design-tokens.md:20` both state `Expiring soon` is **derived in the
renderer**, not a stored status, so widening `DRepStatus` is forbidden and
widening `DRepStatusBadge` with a parallel `variant` prop would change the two
shipped consumers outside cv-2's fence (`DRepCard.tsx:119`,
`DRepDetailOnchainSection.tsx:99`) — the latter already renders "Expires in
{count} epochs" adjacent to the badge (`DRepDetailOnchainSection.tsx:102-113`),
so a shared expiring variant would duplicate copy there.

**Resolution:** `CurrentVoteSummary` imports and renders `<DRepStatusBadge
status={entry.status} />` unchanged for the two ledger-grounded states — this is
the "reuse existing seams" instruction at prompt.md:239-241 and it costs zero new
i18n keys (`governance.drepDirectory.status.active` / `.inactive` already ship in
both catalogs). The derived `Expiring in {n} epochs` overlay renders as a
component-local badge in `CurrentVoteSummary.tsx` / `.scss`, following the
component's own precedent for local status rendering (`CurrentVoteSummary.tsx:16-18`,
`:58-66`). The two are mutually exclusive, never adjacent. Render precedence:

```
entry == null                     → no badge + status.unavailable caption
entry.status === 'inactive'       → <DRepStatusBadge status="inactive" /> + status.inactive caption
entry.drepActivity <= 12          → local expiring badge + status.expiring caption
otherwise                         → <DRepStatusBadge status="active" />, no caption
```

`inactive` is checked first and is safe: `GovernanceQueryService.ts:506-511`
derives `status = expiry <= currentEpoch ? 'inactive' : 'active'` and
`drepActivity = Math.max(0, expiry - currentEpoch)`, so `inactive ⟺ drepActivity === 0`.

### D-2 — The threshold is `≤12`, not the cohort's `7–12`. (resolves O-2)

`shared-design-tokens.md:20` is the canonical grounding statement and says
"derived in the renderer from the remaining `drepActivity` (**≤12 epochs**)". The
`7–12` parenthetical at `:13` is scoped to *default-cohort entries*, and it is a
restatement rather than a second rule: invariant 7's cohort floor is
`drepActivity > 6` (`GovernanceStore.ts:62`, applied `:179-184`), so for a cohort
member `≤12` and `7–12` describe the same set. `CurrentVoteSummary` renders the
user's **own** delegation, which is not cohort-scoped and can sit at
`drepActivity` 1…6 — precisely the range where a `7–12` gate would show nothing
while the delegation lapses next epoch, defeating the panel's binding purpose
(`shared-design-tokens.md:22`).

**Resolution:** condition is `entry.status === 'active' && entry.drepActivity != null
&& entry.drepActivity <= EXPIRING_MAX_REMAINING_EPOCHS` with
`EXPIRING_MAX_REMAINING_EPOCHS = 12` declared **locally in
`CurrentVoteSummary.tsx`**. No lower bound is coded — `status === 'active'`
already implies `drepActivity >= 1`.

**Duplication is deliberate and must be recorded in code.** This is the fourth
independent statement of an expiry window in the renderer (`DRepCategoryBadge.tsx:50-51`
consumed at `:60-69`; `helpers.ts:181-182` consumed at `:204-211`;
`GovernanceStore.ts:62` at `:179-184`). `DRepCategoryBadge`'s constants are **not
exported** (`DRepCategoryBadge.tsx:50-51`, cf. the `export` list at `:43`, `:45`,
`:60`, `:101`), so importing them is not an option. Follow the existing precedent
at `helpers.ts:177-180` and carry a 2-line comment stating *why* this window
differs from the badge module's: the panel is not cohort-scoped. Do **not** write
a task id, a change history, or a defense of correctness in that comment.

### D-3 — task-142 stays before task-175; its assertions are scoped and authored to survive it. (resolves O-3)

Neither row depends on the other, so both orders are dependency-valid; the JSON
listing order (142 → 175) is kept. The apparent conflict — 142 verifies the file
"unchanged" while 175 grows it — dissolves once 142's scope is read from its own
description: "keeps the existing **fee + passphrase / HW status sections**
unchanged". `shared-design-tokens.md:135` charters task-175 to own exactly the
identity block, so the identity block is **out of task-142's unchanged claim**.

Binding consequences:

- task-142's pins target the fee rows (`:174-177`), the
  `selectedWallet.isHardwareWallet ?` branch (`:179-185`) and the passphrase
  `Input` (`:186-202`) **by role/label queries**. No whole-dialog snapshot, no
  "exactly one `<code>` element", no paragraph-count assertion — every one of
  those breaks the moment task-175 lands.
- task-142 AC-3's "HW status section (lines ~L118-L127)" is wrong at every commit
  in the file's history (the block was `:160-166` when the plan was authored,
  `:180-181` at `bdad1d227`, `:179-185` live). A mechanical `+N` re-anchor yields
  a wrong line. Replace the criterion with the semantic one: *the
  `selectedWallet.isHardwareWallet ?` branch rendering `<HardwareWalletStatus …/>`
  is untouched* — the same anchor `slice-3-implementation-guide.md:840` already
  uses.
- task-175 re-runs task-142's assertions unchanged as its own regression gate; if
  any of them fails, the fix is in task-175's edit, not in weakening the pin.

### D-4 — task-140's and task-173's doc obligations: one pre-discharged, one re-anchored, two corrections owed. (resolves O-4)

Commit `2ee5f74cf` ("docs(gov): fold findings audit into tracker rows and plan
docs", 2026-07-27) split the old single line 95 of
`designs/current-vote-display-design.md` into two paragraphs. At HEAD:

- `:95` carries the header-byte classification sentence — **task-173 AC-5 is
  already satisfied.** task-173 verifies and records this; it does **not** re-edit
  the line or double-write the paragraph.
- `:97` carries the comparator paragraph and already states that "A case-sensitive
  bech32 string comparison — including canonical CIP-129 with its type-byte header
  — is not acceptable", so **AC-7's `:95` anchor is stale and re-points to `:97`.**
  But `:97` also still reads "must key on a case-stable form: the
  (`credentialHex`, `credentialType`) pair, **or a case-insensitive `cip129`
  comparison**" — and task-140 **AC-4** bans keying on `raw`, `cip129` or `cip105`
  outright. The design therefore continues to sanction a key the same task's AC-4
  forbids: exactly the design/code drift AC-7 exists to close. **AC-7's first
  conjunct is satisfied in part, not whole**, and is listed in the Definition of
  Done exception table.
  **Still no rewrite** (seam contract R-10; the file is edited by appending only
  on this row). task-140 appends **one** sentence to `:97` that both records which
  form cv-2 ships — the (`credentialHex`, `credentialType`) pair — **and
  explicitly retires the `case-insensitive cip129` alternative from cv-2 onward,
  citing AC-4.** The offer is superseded in force while remaining present in the
  text; the task's `statusReason` says so in those words rather than claiming the
  conjunct discharged.
- **Two corrections are owed, at two files, and they are separate acts:**
  1. `task-plans/cv-1-code-review.md:736-738` still reads that the comparator "must
     key on `cip129` or on the (`credentialHex`, `credentialType`) pair". The file
     is append-only (`README.md:14`), so task-140 **appends** a discharge entry.
     The substance is already recorded in the promise block at `:1224-1234`; what
     is owed is the formal discharge, not new analysis. The appended entry must
     also note that the promise block's own self-reference (`:1224`, `:1234`) says
     `:737-739` while the note actually occupies `:736-738` (`:739` begins a
     different refutation item) — the tracker's `:736-738` is the correct anchor.
  2. `research/cv-1-findings.md:220-227` (F-9's "Tasked:" paragraph) is **false at
     HEAD**: it states that the design doc "still offers 'canonical CIP-129 string
     including the type-byte header' as an acceptable comparison key". `2ee5f74cf`
     fixed that in the same commit that wrote the sentence. Findings files are not
     append-only (only `<id>-code-review.md` is — `README.md:14`), so this one is
     **corrected in place** by task-140.
- The tracker rows for task-140 and task-173 are still `pending`, so no
  pre-discharge was ever recorded. Both rows' `statusReason` must say so at
  Scribe time, and AC-7's `:95` → `:97` re-anchor rides task-140's build commit.

### D-5 — task-139 AC-3 is split: reactivity retained, `givenName` struck and deferred with a named orphan risk. (resolves O-5)

**Clause 1 (`reads givenName from GovernanceStore.drepIndex[drepId]?.givenName`)
and clause 3 (`a Storybook story covers the transition from unverified to verified
name`) are not buildable in cv-2.** Stated plainly, with evidence:

- `AppDRepDirectoryEntry` (`GovernanceStore.ts:20-31`) carries exactly `drepId`,
  `votingPower`, `status`, `drepActivity`, `anchor`. **No name field.**
- Its IPC counterpart `DRepDirectoryEntry`
  (`source/common/types/governance.types.ts:51-62`) is the same shape, and
  `DRepAnchorPresence` (`:66-72`) is `{ url, hash }` only — a reference, never
  fetched content.
- `DRepIdentity` (`governance.types.ts:20-31`) has no name either.
- **`DRepIndexEntry` — the type the design doc names at
  `designs/current-vote-display-design.md:101` — does not exist in code.** A
  repo-wide grep returns exactly that one design-doc hit. The *index* exists
  (`GovernanceStore.ts:100`); the *entry type* it names does not.
- `givenName` appears exactly once across `source/`, `storybook/` and `tests/`,
  and it is a **negative** regression fixture asserting the field never renders:
  `VotingPowerDelegationConfirmationDialog.spec.tsx:85-98`.
- No CIP-119 parse exists. The fetch / hash-verify / parse pipeline is task-149 +
  task-150 (`anchor-1`), both `pending`.

**Clause 2 (reactive update on `drepIndex`, no wallet re-poll) IS buildable and is
retained**, folded into the same `drepIndex`-sourced read task-136 AC-1 already
mandates — that read is cv-2's only live `drepIndex` consumer, so deleting AC-3
wholesale would drop a constraint cv-2 must still honor.

**Which phase owns the struck clauses — and the orphan risk.** No anchor task
currently owns a `CurrentVoteSummary` name render:

- task-151 (anchor-1) AC-1 scopes verified `givenName` to "the DRep detail view";
  its AC-3 exposes only "verified metadata-completeness state … for the slice-5
  cohort rule" — **not** a name field on the index entry.
- task-154 and task-157 AC-2 (anchor-2) scope `givenName` to the **delegation
  confirmation dialog**.
- task-155 AC-3 covers labeling in "DRep detail and delegation confirmation".
- The only statement that anchor-1 populates the name into the index is **untasked
  prose** at `designs/shared-design-tokens.md:250`.

**Resolution:** the struck clauses are deferred to **`anchor-2`** (the phase that
owns verified-name *display* surfaces), and cv-2 records that re-pointing them
requires two tracker edits cv-2 does **not** make:

1. Extend **task-151** (anchor-1) with an acceptance criterion exposing a verified
   name field on the store's index entry — without it there is still no data
   source in anchor-2.
2. Add or extend an **anchor-2** row owning the `CurrentVoteSummary` name render
   and the unverified→verified Storybook story.

Without both, the deferred work is **orphaned, not rescheduled**. This PRD flags
it; the anchor-1/anchor-2 planning passes own the edits.

**Tracker reconciliation riding with task-139's build commit:** rewrite AC-3 in
`governance-drep-discovery-plan-tasks.json:1241` to the retained reactivity clause
alone; record the split in task-139's `statusReason` naming
`GovernanceStore.ts:20-31` as the evidence; record the two struck clauses and the
orphan risk as a finding in `research/cv-2-findings.md` and in this PRD's Final
Outcome. **Do not edit the tracker during planning.**

### D-6 — `CurrentVoteSummary` receives its entry as a prop; the container reads the store. (resolves O-6)

`CurrentVoteSummary` is a plain function component exported as
`injectIntl(CurrentVoteSummary)` (`CurrentVoteSummary.tsx:102`) with props pinned
to `{ currentVote, intl }` (`:11-14`). Its four committed specs render it directly
with no MobX provider (`CurrentVoteSummary.spec.tsx:44-94`), so an `@inject` store
read would break all four. `VotingPowerDelegation` is also presentational — it
receives `wallets`, `stakePools` etc. as props (`:23-51`) and has no `stores`
access, even though it is already wrapped in `observer` (`:339`).

The resolution is forced by where the selection lives: the selected wallet id is
**local React state inside `VotingPowerDelegation`**, so the container cannot
resolve the index entry — it does not know which wallet is selected. Therefore:

```
VotingGovernancePage  (@inject @observer)
  destructures `governance` from this.props.stores  (available: stores/index.ts:43, :68, :121)
  passes  drepIndex={governance.drepIndex}          → VotingPowerDelegation
VotingPowerDelegation (observer)
  resolves the entry for the selected wallet's currentVote
  passes  drepEntry={…}                             → CurrentVoteSummary
```

Reactivity: the container is `@observer` and `GovernanceStore` **reassigns** the
Map (`GovernanceStore.ts:254`, `:297`) rather than mutating it, so an index
refresh re-renders the container and the whole chain. No wallet re-poll, no IPC,
no cardano-cli. Invariant 4 is untouched: `VotingStore` gains no `GovernanceStore`
reference and `grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts`
must still return nothing after the slice.

**The lookup must canonicalize.** `drepIndex` is keyed by canonical **CIP-129**
(`GovernanceQueryService.ts:624-640` emits CIP-129 for both credential kinds;
`helpers.ts:139-153` canonicalizes before `.get()`), while
`currentVote.drep.raw` is byte-untouched and may be CIP-105
(`normalizeDRepIdentity.ts:54`). A plain `drepIndex.get(raw)` silently misses for
every CIP-105 delegation and — because `tsconfig.json:79-80` disables
`strict`/`noImplicitAny` — a bracket read `drepIndex[drepId]` compiles and
evaluates to `undefined`, reading as "no record yet" for a DRep that **is**
indexed. Reuse the existing exported helper:

```ts
resolveExactDRepMatch<AppDRepDirectoryEntry>(
  currentVote.drep.cip129 ?? currentVote.drep.raw,
  drepIndex
)
```
`source/renderer/app/components/governance/drep-directory/helpers.ts:139-153` —
generic over the value type, trims and lower-cases the query
(`normalizeDRepQuery`, `helpers.ts:28-41`), and returns `T | null`. It performs
no IPC by construction (its own doc comment, `:133-138`). It is a lookup-only
transform: the submitted string is never touched.

**The query must be `cip129`, not `raw`.** The helper canonicalizes only what
`Cardano.DRepID.isValid` accepts — the `drep1…` and `drep_script1…` forms — and
rejects everything else *before* canonicalizing: `helpers.ts:144` is
`if (!Cardano.DRepID.isValid(full)) return null;`. Measured in this worktree,
`Cardano.DRepID.isValid('drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l')`
returns `false`, so passing a CIP-105 `raw` returns `null` and reads as "no
record yet" for a DRep that **is** indexed — the exact miss D-6 exists to
prevent. `normalizeDRepIdentity` always populates `cip129` for both encodings
(`normalizeDRepIdentity.ts:40`, `:55`) and `parseVoting` returns `null` when
`normalizeDRepIdentity` fails (`api.ts:3009-3023`), so `cip129` is always
populated on the production path; `?? raw` covers hand-built fixtures only.

### D-7 — `drepVerified` ships as an option id with cv-2-renderable semantics. (resolves O-7)

task-143 AC-3 requires exactly five values, and `current-vote-display-ux.md:206`
describes `drepVerified` as "SIPO mainnet fixture with **verified source and
anchor links**" — affordances cv-2 cannot render (D-5). Renaming or dropping the
option would force a knob-id churn in anchor-2.

**Resolution:** ship all five ids. Differentiate the two DRep options by the
lifecycle state cv-2 *can* render, and document the rest as arriving in anchor-2:

| option | wallet `currentVote` | `makeDRepIndex` entry | renders in cv-2 |
|---|---|---|---|
| `noDelegation` | `null` | empty map | warning + nudge |
| `drepVerified` | CIP-129 DRep | `{ status: 'active', drepActivity: 30 }` | shared `Active` badge, no caption |
| `drepUnverified` | CIP-129 DRep (the committed vector) | `{ status: 'active', drepActivity: 4 }` | local expiring badge + `status.expiring` caption |
| `abstain` | `{ kind: 'abstain' }` | empty map | Abstain chip + caption |
| `noConfidence` | `{ kind: 'no_confidence' }` | empty map | No Confidence chip + caption |

The `no record yet` path (task-136 AC-3) and the `inactive` badge are **not**
reachable from this five-value wallet knob; they are covered by the second knob
on the component story (S-7) and by Jest (task-147).

**Fixture provenance (AC-4) — satisfied in part on *both* halves.**
`drepUnverified` reuses the checksum-verified vectors already committed at
`storybook/stories/governance/CurrentVoteSummary.stories.tsx:17-21` (copied
byte-for-byte from the wallet fixtures per the comment at `:15-16`).
`drepVerified` is derived with the repo's `bech32` dependency from the **Cardano
Academy preprod** DRep key hash
`e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc`, committed at
`research/drep-state-preprod-epoch295-sample.json:2849` with its
`Cardano Academy.jsonld` anchor at `:2852-2853`, and decoded before commit (cv-1's
D-8 procedure).

Two shortfalls, both recorded rather than papered over:

1. **Named provenance is met for one option, not both.** The plan names three
   fixture provenances — SIPO mainnet, Cardano Academy preprod, the canonical
   CIP-119 example (`governance-drep-discovery-plan.md:103`;
   `designs/current-vote-display-design.md:227`). Only Cardano Academy preprod
   is committed anywhere in this repo, so only `drepVerified` carries one.
   `drepUnverified` is the repo's own synthetic story vector. cv-2 mints no new
   credential to close the gap; **do not describe either pair as a "CIP-119 test
   vector"** in the fixture module, in a spec, or in the tracker.
2. **"with verified hash" has no mechanism in cv-2** — no anchor fetch and no
   Blake2b-256 verify exists until anchor-1 task-149/task-150, and both
   `makeDRepIndex` entries ship `anchor: null`.

The fixture module's comment states exactly the provenance above and the
lower-case constraint the `drepIndex` lookup imposes — nothing stronger.

**Invariant 7 is untouched.** The `drepActivity: 4` entry is a **drepIndex entry
for the user's own delegation**, never a default-cohort or directory-list fixture.
`README.md:67`'s prohibition ("Expiring in 3 epochs *inside the default cohort* is
fixture-only and MUST NOT ship") binds cohort fixtures; no cv-2 fixture enters a
cohort computation.

### D-8 — "Every governance story" is scoped to the stories that render a wallet. (resolves the task-145 ambiguity)

Live governance/voting story inventory, verified:

- `storybook/stories/voting/Governance.stories.tsx` — `storiesOf('Voting / Governance')`
  (`:316`) with `.add('Connected flow')` (`:319-320`), `.add('Voting power delegation')`
  (`:400`), `.add('Voting power delegation - prefilled from directory')` (`:403`),
  `.add('Confirmation dialog - software wallet')` (`:424`),
  `.add('Confirmation dialog - hardware wallet')` (`:462`),
  `.add('Unavailable while syncing')` (`:497`). It is the **only** file that
  renders `VotingPowerDelegation` (`:214`, `:405`).
- `storybook/stories/governance/CurrentVoteSummary.stories.tsx` —
  `storiesOf('Governance / Current Vote Summary')` (`:57`) `.add('Core states')` (`:64`).
- `DRepDirectory` / `DRepDetail` / `DRepDirectoryBanner` / `DRepCategoryBadge`
  stories render **no wallet and no current vote** (`DRepDirectory.stories.tsx:134`
  is `wallets: null` inside a sidebar-menus object).

**Resolution.** The five-value knob is added to the three wallet-bearing current-vote
surfaces — `Voting / Governance > Connected flow`, `> Voting power delegation`,
`> Voting power delegation - prefilled from directory` — and to
`Governance / Current Vote Summary > Core states`. The two confirmation-dialog
stories migrate off `GOVERNANCE_WALLETS` (AC-3) but expose **no** current-vote
knob: they render no current-vote surface. `Unavailable while syncing`
(`:497-507`) is out of scope for both the knob **and** the migration — it renders
only `<VotingUnavailable syncPercentage={…} />` and references
`GOVERNANCE_WALLETS` nowhere, so there are exactly four reuse sites (`:233`,
`:420`, `:457`, `:492`), not five. The four directory/detail/badge stories are
out of scope entirely.

**Story ids are not renamed.** `Core states` keeps its id despite gaining a fifth
option; renaming changes the story URL for no user benefit.

**`GOVERNANCE_WALLETS` anchors, corrected from a live read** (the tracker's
"L57-83 with reuse at L228 / L427-458" is stale *and* under-counts): definition
`Governance.stories.tsx:63-97` (`:96` is the third `generateWallet` call's closing
`),`; `:97` is `];`), reuse at **`:233`, `:420`, `:457`, `:492` — four sites, not
three**. `:420` sits inside the `'Voting power delegation - prefilled from
directory'` story (`:403-423`), which did not exist when the tracker text was
written; **no corpus anchor mentions it**, so task-145's migration touches one
more render site than the tracker implies.

### D-9 — Message descriptors are minted by their consumers; task-146 owns the catalogs. (i18n seam)

The cv-1 mint procedure (cv-1-PRD.md:632-635) is: define messages in source first
→ let `yarn i18n:manage` seed both catalogs → replace only the **ja-JP values** by
hand → keep every `!!!` → never hand-edit an en-US value away from its
`defaultMessage`. cv-2's consumers land before task-146 in the build order, so:

| descriptor (object key) | message id | mint task | en-US `defaultMessage` |
|---|---|---|---|
| `statusExpiringBadge` | `voting.governance.currentVote.status.expiringBadge` | task-136 | `!!!Expiring in {n} epochs` |
| `statusExpiring` | `voting.governance.currentVote.status.expiring` | task-136 | `!!!This DRep's voting power will lapse in {n} epochs — consider re-delegating.` |
| `statusInactive` | `voting.governance.currentVote.status.inactive` | task-136 | `!!!This DRep is currently inactive. Your voting power will not be counted until they vote again — consider re-delegating.` |
| `statusUnavailable` | `voting.governance.currentVote.status.unavailable` | task-136 | `!!!DRep status is loading.` |
| `sameVoteHint` | `voting.governance.currentVote.sameVoteHint` | task-140 | `!!!This wallet already votes {target, select, drep {for this DRep} abstain {Abstain} no_confidence {No Confidence} other {the same way}}.` |
| `drepIdCip105` | `voting.governance.confirmationDialog.drepIdCip105` | task-175 | `!!!CIP-105 DRep ID` |
| `signedPayload` | `voting.governance.confirmationDialog.signedPayload` | task-175 | `!!!Signed payload` |

Notes that are binding:

- **`status.expiringBadge` is a PRD-added fifth key.** `shared-design-tokens.md:13`
  mandates the badge label `Expiring in {n} epochs`, but the key inventory at
  `current-vote-display-ux.md:154-186` lists only the three captions. task-146
  carries it through the catalogs alongside the four its AC-1 names.
- **Reuse nothing that is already taken.** `Expiring in 7–12 epochs` is the
  directory *filter option* under `governance.drepDirectory.filter.expiry.thresholdWindow`
  (`DRepDirectoryFilters.tsx:60-63`; `en-US.json:332`, `ja-JP.json:332`), and
  `{count} epochs` is the detail-view *value* under
  `governance.drepDetail.expiresInEpochs` (`DRepDetailOnchainSection.tsx:25-29`).
  Neither is reused; the ICU **pattern** is, with the argument named **`{n}`**
  (matching `shared-design-tokens.md:13` and `current-vote-display-ux.md:180`,
  not the detail view's `{count}`).
- **All five `currentVote` descriptors live in `CurrentVoteSummary.messages.ts`**,
  including `sameVoteHint`, which renders in `VotingPowerDelegation`. The id
  namespace is fixed by `current-vote-display-ux.md:164`; the two files are
  siblings in the same directory, so `VotingPowerDelegation.tsx` imports
  `messages.sameVoteHint` from `./CurrentVoteSummary.messages`. Do **not** invent
  a `VotingPowerDelegation.messages.ts` entry with a different id.
- **Object-key naming follows the existing flattening convention** in
  `CurrentVoteSummary.messages.ts`: `noDelegationTitle` ↔ `…noDelegation.title`,
  `abstainCaption` ↔ `…abstain.caption`, `drepViewDetails` ↔ `…drep.viewDetails`.
- **`confirmationDialog.drepId` copy is not re-worded.** task-175 AC-4 only
  requires that it *labels* the CIP-129 line. Its value is already `!!!DRep ID` in
  both catalogs (`en-US.json:948`) and it is baked into the dialog's committed
  spec; re-wording is churn with no acceptance criterion behind it.
- **The `sameVoteHint` ICU string is taken verbatim from
  `current-vote-display-ux.md:164`.** For the sentinel branches it renders "This
  wallet already votes Abstain." — grammatically weaker than the §8 table's "This
  wallet is already set to Abstain." (`:129-131`). The pattern string is
  authoritative; the phrasing is flagged for the release-end `!!!` copy review
  (user-owned), not fixed in cv-2.

### D-10 — The same-vote hint is a visible inline paragraph, not a hover tooltip. (task-140 AC-2)

`current-vote-display-ux.md:197` requires the disabled-submit hint be "exposed via
`aria-describedby`" with the button "focusable with `aria-disabled='true'`". The
live submit button (`VotingPowerDelegation.tsx:310-320`) is a react-polymorph
`Button` with a plain `disabled` prop, and the other three disable reasons
(invalid form, in-flight tx, error state) share it.

**Resolution:** when `isSameAsCurrent` is the reason, render the hint as a
**visible** paragraph immediately above the button, mirroring the existing
`state.status === 'form-with-error'` paragraph at `:304-308`, carrying a stable
`id`; set `aria-describedby` on the `Button` pointing at it. Visible text is more
accessible than a hover-only tooltip and satisfies ux `:127`'s "Tooltip / hint"
column. Whether react-polymorph's `Button` forwards `aria-describedby` /
`aria-disabled` must be checked at build time; **if it does not, the visible hint
alone satisfies AC-2** and the aria wiring is recorded as a deviation rather than
worked around with a DOM escape hatch.

### D-11 — task-138 takes the re-seed branch, not the "data changed" indicator.

AC-3 offers "re-seeds (**or** surfaces a 'data changed' indicator)". Re-seeding is
the smaller truthful change: with `selectedWalletId` in state (task-137) the
derived wallet already carries the latest `currentVote` on every poll, so a
`useEffect` keyed on the derived `currentVote` identity re-seeds `drepInputState`
**only while the input is not dirty**. A dirty input is never overwritten — that
would destroy user typing mid-edit and would violate `current-vote-display-ux.md:123`
("editing never rewrites the summary; the summary binds to wallet state, not form
state") in the opposite direction. No indicator UI, no new i18n key.

### D-12 — The unregistered governance stories stay unregistered. (in/out call)

`storybook/main.ts:8` sets `stories: ['../storybook/stories/index.ts']` — a single
manual index, not a glob — and `storybook/stories/index.ts:17-18` registers only
`governance/DRepDirectory.stories` and `governance/CurrentVoteSummary.stories`.
`DRepDetail.stories.tsx`, `DRepDirectoryBanner.stories.tsx` and
`DRepCategoryBadge.stories.tsx` are git-tracked but imported nowhere, so they never
enter the preview bundle.

This is **not** a new discovery: it is recorded as a deliberate deferral at
`cv-1-implementation-guide.md:1851-1857` ("Record-only observation (do NOT fix in
cv-1)") and repeated in task-133's `statusReason`
(`governance-drep-discovery-plan-tasks.json:1020`). **No cv-2 task owns it** —
task-145's `targetPath` is `storybook/stories/governance/` and all four of its
acceptance criteria concern the knob and the `GOVERNANCE_WALLETS` migration.

**Resolution:** cv-2 does **not** register them. Registering three stories no cv-2
task owns is scope creep. The consequence is recorded honestly: task-145 AC-1's
"every governance story" is unverifiable for those three because they never
render, which is why D-8 scopes AC-1 to the stories that do. The gap is carried
forward as a residual item for a later slice.

### D-13 — task-143 is hoisted to build position 1.

See **Canonical Build Order**. Dependency-safe (task-143 depends only on task-131,
`complete`); the reason is task-136 AC-4's forward reference to the `drepVerified`
knob value.

### D-14 — The `!!!` guard is widened in place with three assertions. (resolves O-8)

`tests/jest/i18n/preliminaryCopyMarkers.spec.ts:16-25` filters on
`key in ja && en[key].startsWith('!!!') && !ja[key].startsWith('!!!')`, so it is
blind to (a) a key missing from ja-JP entirely and (b) an en-US key minted
**without** `!!!`. cv-2 mints seven new keys into exactly that blind spot.
cv-1 recorded that the cheap fix is an assertion inside the existing file, not a
new suite.

**Resolution — task-146 adds three `it` cases to the same file:**

1. **Key-set symmetry.** Every key in `en-US.json` exists in `ja-JP.json` and vice
   versa. Measured at HEAD: 1611 keys each, zero asymmetry — **no allow-list
   needed**, the assertion is green on arrival and fails the moment a mint skips a
   catalog.
2. **Namespace marker coverage.** Every key under `voting.governance.currentVote.`
   starts with `!!!` in **both** catalogs. Measured at HEAD: 12 keys, zero unmarked
   in either locale — again green on arrival, no allow-list.

3. **Per-key confirmation-dialog markers.** task-175's two new keys —
   `voting.governance.confirmationDialog.drepIdCip105` and `.signedPayload` —
   start with `!!!` in both catalogs, asserted by name against a
   `PRELIMINARY_CONFIRMATION_KEYS` constant.

Assertion 3 exists because the namespace assertion is deliberately **not**
extended to `voting.governance.confirmationDialog.` — seven of its eight keys
legitimately predate the feature and carry no marker (`en-US.json:946-953`; only
`.drepId` at `:948` is marked), so a namespace sweep there would be red on
arrival. The existing `REVIEWED_JA_JP_EXCEPTIONS` list (`:8-10`) is not touched.

### D-15 — task-148 pins the `same_vote` path; it changes none of its six sites.

The working `same_vote` path spans six places: the literal at `VotingStore.ts:62`
(inside the declaration `:61-65`), `parseApiCode` (`:74-95`), the
`initializeVPDelegationTx` catch, the intl map at `VotingPowerDelegation.tsx:89`,
the descriptor at `VotingPowerDelegation.messages.ts:73`, and the copy at
`en-US.json:973` / `ja-JP.json:973`. task-140's client-side disable makes this
path harder to reach through the UI, which is exactly why task-148 exists: it
pins the store-level behaviour (where the UI gate does not apply) **and** the
render path, without editing any of the six sites.

## Cross-Task Seam Contracts

Six authors write the implementation guide in parallel from this PRD. The
semantics below are **binding and not re-derivable**; contradicting them is a
guide defect, not an implementation choice.

### S-1 — `VotingPowerDelegation` state shape (task-137 → 138, 139, 140)

The `Wallet` object leaves state; **an id replaces it, and the wallet is derived
on every render**.

```ts
// FormData (VotingPowerDelegation.tsx:53-61) and Form (:63-66)
selectedWalletId: string | null;   // replaces `selectedWallet: Wallet` / `Wallet | null`
```

```ts
// derived once, immediately after the useState call (currently :115-131)
const selectedWallet =
  wallets.find((w) => w.id === state.selectedWalletId) ?? null;
```

- `initialState` (`:94-102`) becomes `selectedWalletId: null`.
- Every existing `state.selectedWallet` read migrates to the derived local:
  `:136` (`formIsValid`), `:174` (`initiateTransaction`), `:239`
  (`value={selectedWallet?.id || null}`), `:244` / `:260` (render gates), `:286`
  (`onBrowseDRepsClick`), `:333` (`renderConfirmationDialog`).
- The `Form` type's `Omit<FormData, 'selectedWallet'>` (`:63`) becomes
  `Omit<FormData, 'selectedWalletId'>` with `selectedWalletId: string | null`.
- **No store read is introduced.** `wallets` stays the existing prop (`:35`) fed
  from `stores.wallets.all` (`VotingGovernancePage.tsx:63`), which `Request.ts`
  replaces with fresh `Wallet` instances on every poll — that is the whole point
  (`current-vote-display-design.md:204`).

### S-2 — `CurrentVoteSummary` props and mount point (task-136 → 139)

**New prop, optional, defaulting to `null`** — so the four committed specs
(`CurrentVoteSummary.spec.tsx:44-94`) keep compiling unchanged:

```ts
type Props = {
  currentVote: WalletVotingTarget | null;
  drepEntry?: AppDRepDirectoryEntry | null;   // task-136 adds this
  intl: intlShape.isRequired;
};
```

`AppDRepDirectoryEntry` is imported **as a type** from
`../../../stores/GovernanceStore` (it is exported at `GovernanceStore.ts:20`).
Do not re-declare it and do not import the store class.

**Mount point (task-139), exact:** between the `WalletsDropdown` closing tag
(`VotingPowerDelegation.tsx:242`) and the vote-type `ItemsDropdown` block
(`:244`), matching `current-vote-display-ux.md:31` ("between wallet picker and
vote-type controls"). It renders **unconditionally** — including when
`selectedWallet` is `null`, where `currentVote={null}` produces the `noDelegation`
warning + nudge (task-139 AC-1, invariant 9). It is *not* placed inside the
`{state.selectedWallet && (…)}` gates at `:244` / `:260`.

```tsx
<CurrentVoteSummary
  currentVote={selectedWallet?.currentVote ?? null}
  drepEntry={currentDRepEntry}
/>
```

**New prop on `VotingPowerDelegation` (task-139), also optional:**

```ts
drepIndex?: ReadonlyMap<string, AppDRepDirectoryEntry>;
```

Optional with an empty-map default so every intermediate commit type-checks
(`Governance.stories.tsx:214` / `:405` do not pass it until task-145) and so the
default behaviour is the honest "no record yet" path. The container always passes
it. `ReadonlyMap` matches the live precedent at `DRepDirectory.tsx:81`.

**Container wiring (task-139, cross-targetPath):** add `governance` to the
destructure at `VotingGovernancePage.tsx:38-39` and pass
`drepIndex={governance.drepIndex}` into `<VotingPowerDelegation …>` (`:58-65`).
task-173 and task-141 edit a **different region** of the same file
(`renderConfirmationDialog`, `:66-113`) and land after task-139.

**Badge render, inside `CurrentVoteSummary`'s `drep` branch (`:49-72`):** the
lifecycle badge is appended to the existing `styles.statusRow` (`:58-66`), after
`<DRepSourceLabel source="on-chain" …/>` (`:65`) — the row that already carries
status semantics. The caption renders **below the id row** (`:67-69`) per task-136
AC-2. The existing `styles.statusBadge` span (`:59`) is the vote-kind chip
("Delegated to DRep") and stays.

**Spec debt task-136 must pay:** `CurrentVoteSummary.spec.tsx:61-63` asserts
`screen.queryByText(/Active|Inactive|Expiring/)` is **not** in the document. The
shared badge's labels render as `!!!Active` / `!!!Inactive`, which match that
regex, so the assertion and the colocated snapshots
(`__snapshots__/CurrentVoteSummary.spec.tsx.snap`) fail the moment the badge
lands. task-136 rewrites that case to assert the badge **is** absent when
`drepEntry` is `null` and present when it is not, and regenerates the snapshots.

### S-3 — Pre-fill derivation (task-138, consumed by 137's derived wallet)

One pure helper, colocated in `VotingPowerDelegation.tsx` (module scope, above the
component), applied at **both** seed sites so they cannot diverge:

```ts
function deriveFormSeed(
  wallet: Wallet | null,
  inheritedDRepId: string | undefined
): { selectedVoteType: VoteType; drepInputState: { dirty: boolean; value: string } }
```

Fallback chain, in order (task-138 AC-2/AC-4):

1. `wallet?.currentVote?.kind === 'drep'` → `{ selectedVoteType: 'drep',
   drepInputState: { dirty: true, value: wallet.currentVote.drep.raw } }` —
   `raw` is used **verbatim**, no trim, no re-encode, no normalization.
2. `wallet?.currentVote?.kind === 'abstain' | 'no_confidence'` →
   `{ selectedVoteType: <kind>, drepInputState: initialState.drepInputState }`.
3. `inheritedDRepId` (from `initialFormState.selectedDRepId`) →
   `{ selectedVoteType: 'drep', drepInputState: { dirty: true, value: inheritedDRepId } }`
   — again verbatim; this is the existing behaviour at `:127-129` with its
   byte-equality comment at `:125-126`.
4. otherwise → `initialState`'s blank form.

Applied at the lazy `useState` initializer (`:115-131`) and inside
`WalletsDropdown.onChange` (`:231-237`), replacing
`setState({ ...initialState, selectedWallet })` with
`setState({ ...initialState, selectedWalletId: walletId, ...deriveFormSeed(nextWallet, initialFormState?.selectedDRepId) })`.

The reactive re-seed (AC-3) is a `useEffect` keyed on the derived wallet's
`currentVote` identity that applies `deriveFormSeed` **only when
`state.drepInputState.dirty === false`** (D-11).

### S-4 — The same-vote comparator (task-140 → 147, 148)

**New file, new module — not a closure inside the component:**
`source/renderer/app/utils/governance/isSameVoteTarget.ts`
(sibling of `normalizeDRepIdentity.ts` in the same directory).

```ts
export function isSameVoteTarget(
  chosenOption: string,
  currentVote: WalletVotingTarget | null
): boolean
```

Semantics, exactly:

- `currentVote == null` → `false`.
- `chosenOption === 'abstain'` → `currentVote.kind === 'abstain'`.
- `chosenOption === 'no_confidence'` → `currentVote.kind === 'no_confidence'`.
- otherwise, if `currentVote.kind !== 'drep'` → `false`.
- otherwise: `const selected = normalizeDRepIdentity(chosenOption)`.
  - `selected == null` → `false` (equality cannot be established; the server
    `same_vote` net still applies — task-148).
  - `selected.credentialHex == null || currentVote.drep.credentialHex == null`
    → `false` (task-140 AC-5: two identities that both lack the hex are never
    equated; `credentialType` alone never establishes equality).
  - else → `selected.credentialHex.toLowerCase() === currentVote.drep.credentialHex.toLowerCase()
    && selected.credentialType === currentVote.drep.credentialType`.

Why this is case-stable, measured: `bech32@2.0.0`'s `decode` normalizes the HRP,
so `normalizeDRepIdentity` accepts an all-uppercase id and returns the same
`credentialHex` (`toHex`, `normalizeDRepIdentity.ts:8-9`, always lowercase) while
`raw` / `cip129` keep the caller's case verbatim (`:39-40`, `:54`, `:56`). The
`.toLowerCase()` on both sides is belt-and-braces for hand-built fixtures and
mutates nothing — the function takes strings and returns a boolean, never writing
back (invariant 10).

**Consumption in `VotingPowerDelegation.tsx`:**

```ts
const isSameAsCurrent = isSameVoteTarget(chosenOption, selectedWallet?.currentVote ?? null);
```
**`submitButtonDisabled` moves; `chosenOption` does not.** `isSameAsCurrent`
reads `chosenOption`, which is derived at `:160-163` — *below* the
`submitButtonDisabled` block at `:139-143`. Referencing it from `:139` would be a
temporal-dead-zone `ReferenceError` at render time. So the block at `:139-143` is
**deleted** and re-declared immediately after the `chosenOption` derivation, with
`isSameAsCurrent` declared just above it and folded into the disjunction as
`isSameAsCurrent ||`.
`formIsValid` (`:135-137`) stays exactly where it is. The move is safe because
`submitButtonDisabled` is referenced only in the JSX (`:313`); `chosenOption`
itself is neither moved nor modified.

**Test ownership split:** task-140 **creates**
`tests/jest/governance/isSameVoteTarget.spec.ts` with its own basic vectors
(sentinel↔sentinel, sentinel↔drep, key vs script with the same hash, absent hex,
`null` decode). task-147 **extends the same file** with the letter-case vector
that task-140 AC-5 explicitly assigns to it. No second spec file.

### S-5 — Confirmation-dialog identity block (task-173 → 141, 142, 175)

**task-173, container side** — replace `VotingGovernancePage.tsx:75-83` with:

```ts
const drepIdentity: DRepIdentity | null =
  chosenOption === 'abstain' || chosenOption === 'no_confidence'
    ? null
    : normalizeDRepIdentity(chosenOption);
```

The explicit sentinel guard stays (invariant 13; it also short-circuits before a
bech32 decode attempt). The existing comment at `:72-74` is rewritten to state the
remaining invariant, not the change. No logging is added on either branch.
`toStoryDRepIdentity` (`Governance.stories.tsx:58-61`) is replaced by the same
expression so story and container cannot drift (AC-3).

**task-175, dialog side** — the block at
`VotingPowerDelegationConfirmationDialog.tsx:151-172`. The `drepIdentity ?` branch
grows to four parts, in this order, all inside the existing `styles.content` div:

| part | label | value | condition |
|---|---|---|---|
| 1 | `messages.drepId` (existing, `.drepId`) | `<code>{drepIdentity.raw}</code>` — unchanged element at `:160` | always when identity non-null |
| 2 | `messages.drepIdCip105` (new) | `<code>{drepIdentity.cip105}</code>` | `drepIdentity.cip105 != null` |
| 3 | `messages.signedPayload` (new) | `<code>{`{"vote":{"type":"drep","id":"${drepIdentity.credentialHex}"}}`}</code>` | `drepIdentity.credentialHex != null` |
| 4 | — | `<DRepSourceLabel source="on-chain" />` | always when identity non-null |

`DRepSourceLabel`'s `'on-chain'` variant already exists
(`DRepSourceLabel.tsx:18`) — the component returns `null` for an unknown variant
(`:32`), so the variant string must be exactly `'on-chain'`.

The `:` branch (`:163-172`, sentinel label) is untouched. The fee rows
(`:174-177`), the HW branch (`:179-185`) and the passphrase input (`:186-202`)
are untouched — that is what task-142 pins (D-3).

**task-141's pin** asserts the exact prop set the container hands the dialog:
`chosenOption`, `drepIdentity`, `fees`, `hwDeviceStatus`, `isTrezor`, `onClose`,
`onExternalLinkClick`, `onSubmit`, `redirectToWallet`, `selectedWallet`
(`VotingGovernancePage.tsx:85-111`) — and no `previousVote` / `newVote`.

### S-6 — Storybook fixture module (task-143 → 144, 145, 136)

**`storybook/stories/governance/_utils/fixtures.ts`** (new; the `_utils`
convention exists at `storybook/stories/{news,nodes,wallets}/_utils/`). Exports,
named exactly:

```ts
export type CurrentVoteOption =
  | 'noDelegation' | 'drepVerified' | 'drepUnverified' | 'abstain' | 'noConfidence';

export const currentVoteOptions: Record<string, CurrentVoteOption>;      // knob label → id
export function useCurrentVoteKnob(): CurrentVoteOption;                  // select('Current vote (mock)', currentVoteOptions, 'noDelegation')
export function resolveCurrentVote(option: CurrentVoteOption): WalletVotingTarget | null;
export function makeGovernanceWallets(option: CurrentVoteOption): Wallet[];
export function makeDRepIndex(option: CurrentVoteOption): Map<string, AppDRepDirectoryEntry>;
```

- Knob **label** is `'Current vote (mock)'` — the id already live at
  `CurrentVoteSummary.stories.tsx:66` and mandated by
  `current-vote-display-ux.md:201`. Option labels come from the table at
  `current-vote-display-ux.md:202-208`: `Not delegated (warning)`,
  `DRep — verified anchor`, `DRep — unverified anchor`, `Abstain`, `No Confidence`.
- `makeGovernanceWallets` **constructs `new Wallet({…})` directly** and returns a
  freshly built array every call. It does **not** call and does **not** widen
  `generateWallet` (`storybook/stories/_support/utils.ts:104-142`), whose nine
  positional parameters cannot express `votingTarget` and whose `new Wallet({…})`
  literal (`:118-142`) has no such key — while the domain accepts it
  (`Wallet.ts:130`, `:166`, `:200`, `:255-257`). Wallet ids, names, amounts and
  HW/syncing flags mirror today's `GOVERNANCE_WALLETS` (`Governance.stories.tsx:63-97`):
  `governance-wallet-1` (software, ready), `governance-wallet-2` (Ledger, ready),
  `governance-wallet-3` (syncing). Only wallet 1 carries the `votingTarget`.
- `makeDRepIndex` returns a fresh `Map` keyed by the **canonical CIP-129** form of
  the fixture DRep id (so `resolveExactDRepMatch` finds it — S-2/D-6), with the
  entries in D-7's table.
- **`makeDRepIndex` is a PRD-added export** not named in task-143's ACs. It is
  required by task-136 AC-4 and task-145 AC-1, sits inside task-143's `targetPath`
  and its "fixtures" charter, and rides task-143's commit.
- This module **supersedes** the inline block at
  `CurrentVoteSummary.stories.tsx:17-52` (`KEY_CIP129` / `KEY_CIP105` /
  `KEY_CREDENTIAL_HEX` at `:17-21`, `CURRENT_VOTE_OPTIONS` at `:23-28`,
  `resolveCurrentVote` at `:30-52`). task-136 deletes that block and imports from
  `_utils/fixtures`; it does **not** append a fifth entry in place.

**`storybook/stories/governance/_utils/GovernanceWrapper.tsx`** (new, task-144) —
a **render-prop component**, default-exported (matching the `_utils/*Wrapper.tsx`
default-export convention at `storybook/stories/wallets/_utils/WalletsWrapper.tsx:8`):

```tsx
export type GovernanceStoryFixtures = {
  wallets: Wallet[];
  drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>;
};

type Props = {
  option: CurrentVoteOption;
  children: (fixtures: GovernanceStoryFixtures) => React.ReactNode;
};

export default function GovernanceWrapper({ option, children }: Props) {
  return (
    <React.Fragment key={option}>
      {children({ wallets: makeGovernanceWallets(option), drepIndex: makeDRepIndex(option) })}
    </React.Fragment>
  );
}
```

The key-derivation rule (`current-vote-display-design.md:25`,
`current-vote-display-ux.md:211`) is **identity**: the React key is the option id
verbatim — no composite keys, no index keys. It is deliberately **not** a
Storybook decorator (the `WalletsWrapper.tsx:8` `(story, context)` shape cannot
hand fixtures to the story body) and it **does not read the knob** — the story
does, via `useCurrentVoteKnob()`, so a single knob read drives both the wrapper
key and any other per-story wiring.

### S-7 — Story wiring (task-145, plus task-136's component story)

`Governance.stories.tsx`, per wallet-bearing story:

```tsx
const option = useCurrentVoteKnob();
return (
  <GovernanceWrapper option={option}>
    {({ wallets, drepIndex }) => (
      <VotingPowerDelegation … wallets={wallets} drepIndex={drepIndex} />
    )}
  </GovernanceWrapper>
);
```

- `renderGovernancePanel` (`:204-236`) takes `option` as a parameter rather than
  reading the knob itself, so `Connected flow` (`:319-399`) and
  `Voting power delegation` (`:400-402`) share one implementation.
- `'…prefilled from directory'` (`:403-423`) keeps its `initialFormState`
  (`:411-415`) and swaps `wallets={GOVERNANCE_WALLETS}` (`:420`) for the
  wrapper's `wallets`.
- The two confirmation-dialog stories swap `GOVERNANCE_WALLETS[0]` (`:457`) and
  `[1]` (`:492`) for `makeGovernanceWallets('noDelegation')[0]` / `[1]` — the
  migration AC-3 demands, with no knob added (D-8).
- After the migration, `grep -n GOVERNANCE_WALLETS storybook/stories/voting/Governance.stories.tsx`
  must return nothing.
- **Keep at least one panel story where the DRep input differs from the wallet's
  `currentVote`.** `renderGovernancePanel` exposes the `Initialization error`
  knob including `same_vote` (`:97`); once a wallet carries a `currentVote`,
  task-140's client-side disable would otherwise make that knob unreachable.

`CurrentVoteSummary.stories.tsx` (task-136) gains a **second, independent knob**
covering the badge matrix the five-value wallet knob cannot reach:

```
select('DRep status (mock)', { 'No record yet': 'none', Active: 'active', 'Expiring soon': 'expiring', Inactive: 'inactive' }, 'none')
```
driving the `drepEntry` prop directly. This is the home for task-136 AC-3's
"no record yet" path and for the `inactive` badge. The file keeps its single
`.add('Core states')` id and its global-locale comment (`:54-56`); **no local
`IntlProvider`, no per-locale variants** — `storybook/preview.tsx:8` owns the
`StoryWrapper` with the English/Japanese toggle.

### S-8 — Test file map (task-140, 141, 142, 147, 148, 175)

| what | file | task |
|---|---|---|
| comparator unit vectors | `tests/jest/governance/isSameVoteTarget.spec.ts` (**new**) | 140 creates, 147 extends with the letter-case vector |
| container flow: pre-fill, wallet-change reset, directory handoff | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` (existing, 391 lines) | 138, 147 |
| `WalletsDropdown` mock gains `onChange` | same file, `:34-38` | 138 |
| script-DRep `credentialType` assertion | same file | 173 |
| dialog prop-set pin (no historical props) | same file | 141 |
| same-vote submit disabled + HW disconnected/locked/app-not-open | same file (HW describe `:304-391`) | 147 |
| dialog section pins (fee / HW / passphrase) | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx` (existing) | 142 |
| §7 block: decode-equality, byte-equality, HW hex | same file | 175 |
| panel snapshots + badge/caption states | `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx` (existing) | 136 |
| logger / analytics spies over the new domain shapes | `tests/jest/security/governance-sanitization.spec.ts` (existing, `describe` at `:70` and `:218`) | 147 |
| `!!!` guard widening | `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` (existing) | 146 |
| `same_vote` store path | `source/renderer/app/stores/VotingStore.spec.ts` (existing, `initializeVPDelegationTx` describe) | 148 |

**Cross-`targetPath` note.** task-147 and task-148 carry `targetPath: "tests/jest/"`,
but the flow harness (`buildStores` `:78-122`, `renderFlow` `:126-163`, HW describe
`:304-391`) and the `initializeVPDelegationTx` describe already exist **colocated
under `source/`**. Re-implementing that scaffolding under `tests/jest/` would
duplicate ~150 lines. Both tasks extend the colocated specs and record the
deviation in their `statusReason`, with cv-1 task-134 as the precedent
(cv-1-PRD.md:155 records exactly this pattern). `tests/jest` is ~8% of the suite;
never treat `jest tests/jest` as "the suite".

### S-9 — Sanitization surface cv-2 widens (task-147, discharging cv-1 F-15)

`filterLogData`'s guarded set is **20 exact-match strings** at
`source/common/utils/logging.ts:24-49`, keyed to request/response names. Matching
is exact string equality (`sensitiveData.includes(key)`, `:59`) and a hit
**deletes the whole subtree** (`:59-61`). The recursion is the hand-rolled
`redact` closure at `:51-71` — the comment at `:41-44` credits
`omit-deep-lodash`, but the module imports nothing (`:1-6`), so a task editing
this list must not go looking for a library call.

The renderer-domain names cv-2 makes live are **all unguarded**: `votingTarget`
and `currentVote` (`Wallet.ts:130`, `:166`, `:255-257`), `drepIdentity` and its
members `raw` / `cip129` / `cip105` / `credentialHex` / `credentialType`
(`governance.types.ts:20-31`, live via `VotingGovernancePage.tsx:75-87` and
`VotingPowerDelegationConfirmationDialog.tsx:56`), `chosenOption`
(`VotingStore.ts:285`, `:372`; `VotingPowerDelegation.tsx:26`), and — because the
sentinels ride the `kind` member of `WalletVotingTarget`
(`api/wallets/types.ts:86-93`) — the sentinel guard is defeated by the domain
shape too, not only the id guard. `currentVote` never matches `vote` because the
match is exact.

**cv-2's discharge (task-147's "no DRep id / abstain / no_confidence in logs"
criterion) is the stricter invariant, not a key-list patch:** *no domain `Wallet`
and no `DRepIdentity` ever enters a logger or analytics payload from a cv-2 code
path.*

**Scope of the sentinel half, stated once so no task re-derives it.** The
`abstain` / `no_confidence` clause of task-147 AC-5 binds **logger** payloads.
The analytics `Casted governance vote` event deliberately carries the derived
vote kind as its third argument — `VotingStore._getVoteKind`
(`VotingStore.ts:196-202`) returning `'drep' | 'abstain' | 'no_confidence'`, sent
at `:399-403` and `:430-434` — a task-110 decision recorded at
`research/slice-3-findings.md:132-141` (F-5). cv-2 asserts the analytics payload
is exactly that three-argument vote-kind shape with no bech32 identifier, and
never asserts the literal's absence from it.

The existing governance loggers already meet the invariant —
`GovernanceStore.ts:263` and `:302` log `{ errorType }`; `VotingStore.ts:354` and
`:412` log `{ errorCode }` — and cv-2 adds no logging anywhere. task-147 asserts
it with the task-111 spy pattern over the flows task-137/138/140 (`chosenOption`)
and task-173/175 (`drepIdentity`) create.

If a cv-2 code review nonetheless finds a payload that must be logged, the
fallback is to extend `sensitiveData` with `votingTarget`, `currentVote`,
`drepIdentity`, `chosenOption`, `cip129`, `cip105`, `credentialHex` **and** add
domain-shaped cases to the floor suite — the whole surface, not two keys. Recorded
as R-6.

*Anchor corrections for the F-15 text, verified:* the `filterLogData` describe is
`tests/jest/security/governance-sanitization.spec.ts:70-216` (15 wire-shaped
cases), not `:58-136`; the mapper line is `api.ts:3145`, not `:3153`; the `Wallet`
constructor is `Wallet.ts:176-178`, not `:175-177`. Key-position ids
(`stakeByDRepId`, `governance.types.ts:98`, built at
`GovernanceQueryService.ts:583-616`) can never be reached by key-name filtering,
but they are public ledger data by the repo's own stance
(`setupLogging.ts:178-181`) — a note, not a defect.

### S-10 — Guide-authoring shards (suggested, not binding)

The seams above partition cleanly into six guide shards: **(1)** task-143 +
task-144 (storybook fixtures); **(2)** task-136 (panel badge + component story +
spec/snapshots); **(3)** task-137 + task-138 (form state + pre-fill);
**(4)** task-139 + task-140 (mount + comparator + container `drepIndex` wiring +
the D-4 doc corrections); **(5)** task-173 + task-141 + task-142 + task-175
(container identity + dialog); **(6)** task-145 + task-146 + task-147 + task-148
(story wiring, i18n, regression harness).

## User Stories

- **US-CV2.1 — Know my DRep is lapsing.** As a wallet owner delegating to a DRep
  whose `drepActivity` is running down, I see `Expiring in {n} epochs` on the
  current-delegation panel plus a one-line explanation, so I can re-delegate
  before my voting power stops counting.
- **US-CV2.2 — Don't pay for a no-op.** As a wallet owner, when I select my
  current DRep again — even pasted in a different letter case — submit is
  disabled with a hint saying the wallet already votes that way, instead of
  costing me a fee and a server round-trip.
- **US-CV2.3 — Start from where I am.** As a wallet owner, selecting my wallet
  pre-fills the form with my current delegation, and coming back from the DRep
  directory keeps the id I picked byte-identical.
- **US-CV2.4 — Verify what I am signing.** As a hardware-wallet owner, the
  confirmation dialog shows the CIP-129 id, the CIP-105 id, and the exact signed
  payload credential hex, so I (or a test harness) can prove the on-device
  identifier is the same DRep.
- **US-CV2.5 — Script DReps are labeled correctly.** As an owner delegating to a
  script DRep with a `drep1…` id, the dialog no longer labels my target a key
  DRep.

## Non-Functional Requirements

- **Renderer-only.** No new IPC channel, no new cardano-wallet endpoint, no
  main-process edit, no signing-path change, no polling change
  (`current-vote-display-design.md:14`). Invariant 5 untouched — cv-2 parses no
  lovelace and adds no IPC.
- **No new query load.** The badge reads an already-populated observable. The
  component issues no cardano-cli invocation and no fallback IPC lookup
  (`current-vote-display-design.md:189`, invariants 1 and 6).
- **Sanitization floor (invariant 2, inlined) — with one reviewed carve-out.**
  Operative reading for cv-2: **no DRep identifier** (raw / CIP-129 / CIP-105 /
  `credentialHex`) and **no domain `Wallet` or `DRepIdentity` object** in any
  logger, analytics or electron-store payload; **no `abstain` / `no_confidence`
  literal in any logger payload**. The one exception is the shipped analytics
  vote *kind*: `VotingStore._getVoteKind` (`VotingStore.ts:196-202`) returns
  `'drep' | 'abstain' | 'no_confidence'` and is sent as the third
  `analytics.sendEvent` argument at `VotingStore.ts:399-403` and `:430-434`. That
  is a pre-existing, deliberate task-110 scope choice recorded at
  `research/slice-3-findings.md:132-141` (F-5) — a vote kind reveals no DRep
  identity. cv-2 neither widens nor "fixes" it, and **no cv-2 test may assert
  that `'abstain'` is absent from an analytics payload** (task-147 AC-5 is read
  this way; it will fail otherwise). See S-9. Fixtures, specs and docs MAY
  contain DRep ids — the floor binds runtime logging/analytics/store paths only.
- **Byte-equality (invariant 10, inlined).** The comparator, the pre-fill and the
  identity block all read; none writes back. `chosenOption` and the
  `delegateVotes` `dRepId` remain the form input string byte-for-byte.
- **i18n (invariant 11, inlined).** Every new en-US and ja-JP string keeps the
  leading `!!!`. Removal is a release-end manual review, never a per-slice task.
- **Accessibility.** Glyph + text on every badge, never colour alone
  (`shared-design-tokens.md:18`); WCAG AA 4.5:1 in both themes; the disabled-submit
  hint is visible and `aria-describedby`-linked (D-10); the caption is a sibling
  of the id row, not a tooltip.
- **Performance.** The badge derivation is an O(1) `Map` lookup per render. No
  memoization is introduced; `resolveExactDRepMatch` does a `trim`/`toLowerCase`
  and one bech32 canonicalization per render, which is negligible against the
  existing per-render `Cardano.DRepID.isValid` call at `:133`.

## Architecture: Data Flow (cv-2 delta)

```
GovernanceStore (slice-1)                       Wallet domain (cv-1)
  @observable drepIndex: Map<CIP129, entry>       @observable votingTarget
  rebuilt (never mutated) :254 / :297             @computed currentVote
        │                                                │
        │  drepIndex                                     │  wallets: Array<Wallet>
        ▼                                                ▼
VotingGovernancePage  (@inject @observer)  ──────────────────────────  task-139 adds `governance`
  drepIndex={governance.drepIndex}                  wallets={wallets.all}
  drepIdentity = normalizeDRepIdentity(chosenOption)                    task-173
        │                                                │
        ▼                                                ▼
VotingPowerDelegation (observer)
  selectedWalletId: string | null                                       task-137
  selectedWallet = wallets.find(id)         ← latest poll snapshot
  seed = deriveFormSeed(selectedWallet, initialFormState?.selectedDRepId)  task-138
  entry = resolveExactDRepMatch(currentVote.drep.cip129 ?? .raw, drepIndex)  task-139
  isSameAsCurrent = isSameVoteTarget(chosenOption, currentVote)         task-140
  submitButtonDisabled |= isSameAsCurrent      + visible sameVoteHint
        │                                                │
        ▼ (mounted between :242 and :244)                ▼ (unchanged path)
CurrentVoteSummary                              renderConfirmationDialog
  props { currentVote, drepEntry }                VotingPowerDelegationConfirmationDialog
  drep  → DRepStatusBadge | local expiring badge    §7 block: CIP-129 / CIP-105 /   task-175
          + status caption                          signed payload / on-chain label
  null entry → status.unavailable, no badge         fee + HW + passphrase UNCHANGED  task-142
                                                          │
                                                          ▼
                                            voting.delegateVotes({ chosenOption, … })
                                              chosenOption byte-identical throughout (invariant 10)
                                              same_vote server net still reachable   task-148
```

Storybook mirror: `_utils/fixtures.ts` (`makeGovernanceWallets` /
`makeDRepIndex` / `resolveCurrentVote`) → `GovernanceWrapper` (`key={option}`) →
the three wallet-bearing stories; `CurrentVoteSummary.stories.tsx` drives
`drepEntry` directly through a second knob.

## What cv-2 Deliberately Does NOT Include

- **No verified `givenName` anywhere.** No name field on
  `AppDRepDirectoryEntry`, no CIP-119 parse, no unverified→verified story (D-5).
  The confirmation dialog renders the **pre-anchor** §7 template only
  (`shared-design-tokens.md:135`); `{verified givenName}` and `Name: Verified
  off-chain content` are task-154 (anchor-2).
- **No anchor fetch, hash-verify, cache, or external link.** The entire transport
  floor is anchor-1 (invariant 3). `drep.anchorMetadata` and `drep.viewDetails`
  message descriptors exist from cv-1 (`CurrentVoteSummary.messages.ts:49-60`) and
  stay **unrendered** — cv-2 wires neither link. The in-app details link is gated
  on slice-4 (task-116).
- **No `DRepStatusBadge` / `DRepStatus` widening** and no `Retired` variant
  (invariant 14 — `retired` awaits a distinct unregistration signal).
- **No `DRepIdDisplay` dual CIP-129/CIP-105 mode** (anchor-2; task-175 lays the
  block out in the dialog itself).
- **No Previous → New comparison rows.** Deferred beyond v1
  (`current-vote-display-ux.md:137-148`); `confirmationDialog.previousVote` /
  `.newVote` stay reserved-not-wired (`:168`).
- **No second delegation backend (invariant 4).** Selection still supplies a DRep
  ID to the existing `delegateVotes` / `VotingStore` path via
  `location.state` only. `VotingStore` gains no `GovernanceStore` reference.
- **No cohort, sort, filter, or ordering effect.** The badge is informational
  only (invariant 8); it never reorders, filters, or overrides anything.
- **No Cucumber / e2e.** `README.md:45` — v1 ships no e2e; task-147 is Jest-only.
- **No Storybook index registration** for the three unregistered governance story
  files (D-12).
- **No epoch plumbing.** `DRepListQueryPayload.epoch` exists on the wire
  (`governance.types.ts:92-93`, populated at `GovernanceQueryService.ts:245`) but
  is dropped in `fetchDRepList`'s `runInAction` (`GovernanceStore.ts:251-259`,
  which reads only `payload.dreps` and `payload.fetchedAt`). cv-2 does **not**
  land it. Note the constraint is "**no per-DRep expiry epoch crosses IPC**", not
  "the renderer has no epoch" — a current epoch is reachable at
  `NetworkStatusStore.ts:123` (`networkTip.epoch`, `api/network/types.ts:1-5`).
  What is missing is per-DRep `expiry`, collapsed in main at
  `GovernanceQueryService.ts:506-511`. A real expiry comparison would be a
  main-process + shared-type change, not a renderer edit; `drepActivity` remains
  the only source, exactly as every other renderer consumer already does
  (`GovernanceStore.ts:182-183`, `helpers.ts:206-208` / `:251-257`,
  `DRepCategoryBadge.tsx:62-64`, `DRepDetailOnchainSection.tsx:107-109`).

## i18n Key Inventory (task-146; all keep the leading `!!!`)

New in cv-2 — **seven** keys, five under `voting.governance.currentVote.*` and two
under `voting.governance.confirmationDialog.*`. Descriptors, ids and en-US source
copy are in D-9. ja-JP values are hand-written after the runner seeds both
catalogs, and every one keeps its `!!!`.

Reused unchanged, no new key: `governance.drepDirectory.status.active` /
`.inactive` (the shared badge's labels, already in both catalogs),
`voting.governance.confirmationDialog.drepId` (now labels the CIP-129 line), and
the twelve cv-1 `currentVote` keys at `en-US.json:954-965`.

Explicitly **not** reused: `governance.drepDirectory.filter.expiry.thresholdWindow`
("Expiring in 7–12 epochs", `en-US.json:332`) and
`governance.drepDetail.expiresInEpochs` ("{count} epochs") — see D-9.

Still reserved-not-wired: `voting.governance.confirmationDialog.previousVote` /
`.newVote` (`current-vote-display-ux.md:168`).

## Docs / Designs / Research / Workflows / Skills Consulted

- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  — cv-2 phase `:1162-1457` (authoritative task contracts, quoted verbatim above);
  task-133 `statusReason` `:1020`; anchor-1 phase from `:1458`.
- `.agent/plans/governance/drep-discovery/prompt.md` — PRD minimum contents
  `:68-73`; guide bar `:75-89`; locked invariants `:93-138`; slice order
  `:146-149`; planning decomposition `:160-186`; build loop `:187-220`; status
  rule `:225-233`; convergence `:235-242`; stop conditions `:246-257`; DoD
  `:261-270`.
- `.agent/plans/governance/drep-discovery/README.md` — `:12` status vocabulary,
  `:14` slice docs + append-only rule, `:15` one commit per task, `:18` `!!!`
  rule, `:44` the `CurrentVoteSummary` live-badge commitment, `:45` no e2e in v1,
  `:67` the 6-epoch fixture floor, `:68` current-delegation status visibility,
  `:76` confirmation identity stays DRep-ID-only until anchor-2.
- `designs/shared-design-tokens.md` — §1 badge table `:9-16` (`:13` expiring row),
  contrast rule `:18`, status grounding `:20`, where-rendered `:22`; §7
  `:108-139` (pre-anchor template `:114-120`, post-anchor `:124-131`, never-show
  rule `:133`, block ownership `:135`, identity-equality `:137`, HW gate `:139`);
  `:250` the untasked "anchor-1 populates `givenName` into `drepIndex`" prose.
- `designs/current-vote-display-design.md` — `:14` renderer-only; `:57-59` the
  combined `drep` card (cv-1 F-18); `:95` header-byte classification (task-173
  AC-5, discharged); `:97` comparator (task-140 AC-7); `:101` the non-existent
  `DRepIndexEntry`; §9.1 `:172-193` (`:185` combined rule, `:189` status data
  source); §9.2 `:195-204` (`:204` `selectedWalletId`); §9.3 `:206-210`; §10
  `:212-228`; §12 `:246-255` (stale `.test.ts` names — cv-1 F-3).
- `designs/current-vote-display-ux.md` — IA `:31`; `drep` badge spec `:85`; §7
  pre-fill `:116-123`; §8 same-vote `:125-135`; §9 deferred Previous→New
  `:137-148`; §10 HW `:150-152`; §11 keys `:154-186` (`:164`, `:180-182`, `:186`);
  §12 a11y `:188-197` (`:197`); §13 knob `:199-211`.
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md` —
  Track V `:287`, `drepIndex` dependency `:298`, delegation integration
  `:253-260`, Key Decisions `:152-156`.
- `research/cv-1-findings.md` — F-9 comparator (`:220-227` now false at HEAD —
  D-4), F-15 `filterLogData` wire-keying (`:472`, discharge handed to cv-2), F-18
  combined design card, F-20 `storybook:build` red at HEAD, F-24/F-25 i18n
  blind spots.
- `task-plans/cv-1-PRD.md` — structure and depth precedent; mint procedure
  `:632-635`; cross-`targetPath` precedent `:155`; OWED convention `:570-603`.
- `task-plans/cv-1-code-review.md` — `:736-738` stale comparator note; `:1224-1234`
  the promise block assigning the discharge to task-140; `:3465-3680` slice close
  (`:3609-3633` OWED, `:3635-3671` what cv-2 inherits).
- `task-plans/cv-1-implementation-guide.md` — `:1851-1857` the recorded, deferred
  Storybook index gap; guide skeleton for the downstream authors.
- Live code verified during planning (all anchors in this PRD).
- Skills flagged for implementation, not invoked at planning:
  `bech32-encoding-decoding` (the new `drepVerified` vector, D-7),
  `storybook-creation` (task-143/144/145), `i18n-messaging` (task-146),
  `e2e-test-creation` **not** applicable (`README.md:45`).

## Locked Invariants Touched (inlined)

- **(1) Local-first** — respected by task-136/139: the badge reads an
  already-populated observable. No hosted explorer, indexer, GovTool, Koios or
  Blockfrost call is added.
- **(2) Sanitization floor** — touched by every task that renders or compares a
  DRep id (136, 138, 140, 173, 175) and re-asserted by task-147. No DRep id and
  no CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store
  payload; no `abstain`/`no_confidence` literal in any **logger** payload; the
  task-111 spy suite `tests/jest/security/governance-sanitization.spec.ts` is
  re-run green in this slice. Fixtures/docs MAY contain DRep ids. cv-2 discharges
  cv-1 F-15 via the stricter no-domain-object-in-logs invariant (S-9).
  **Carve-out, carried from task-110 and not re-opened here:** the analytics
  `Casted governance vote` payload's third argument is the derived vote *kind*
  and does carry the literal `'abstain'` / `'no_confidence'`
  (`VotingStore.ts:196-202`, sent at `:399-403` and `:430-434`;
  `research/slice-3-findings.md:132-141` F-5). The invariant's literal text bans
  it; the shipped, reviewed behaviour keeps it because a vote kind reveals no
  DRep identity. cv-2 asserts the payload is exactly the three-argument vote-kind
  shape carrying no bech32 identifier, and asserts the literal's absence only in
  logger payloads.
- **(4) No second delegation backend** — guarded by task-137/138/139/141.
  Selection supplies a DRep ID to the existing `delegateVotes`/`VotingStore` path
  via `location.state` only; `VotingStore` never reads `GovernanceStore`. The new
  `drepIndex` read is container → props, never store-to-store.
- **(6) CLI discipline** — respected by task-136: no per-DRep invocation, no
  fallback IPC lookup, no new spawn.
- **(7) Default cohort is binding** — touched by task-143/145 fixtures. The
  6-epoch floor binds cohort fixtures; cv-2's `drepActivity: 4` entry is a
  `drepIndex` entry for the user's own delegation and never enters a cohort
  computation (D-7).
- **(8) Badges are informational only** — touched by task-136. The lifecycle badge
  never reorders, filters, or overrides the cohort or any sort.
- **(9) No auto-delegation** — touched by task-138/139. `noDelegation` keeps the
  blank form and the panel keeps the CIP-1694 reward-withdrawal warning + CTA;
  Daedalus never picks a delegation.
- **(10) Byte-equality** — touched by task-138/140/173/175. CIP-129, CIP-105 and
  the signed payload `vote.id` stay byte-equal through every change; the
  comparator and the pre-fill mutate nothing; the on-device identifier equals
  `vote.chosenOption`.
- **(11) Preliminary copy** — touched by task-136/140/146/175. Every new en-US and
  ja-JP string keeps the leading `!!!`; removal stays the user-owned release-end
  review. task-146 widens the guard (D-14); it strips nothing.
- **(13) Sentinels are form-only** — touched by task-140/173/175. `abstain` /
  `no_confidence` compare as sentinels, yield a `null` identity, and render no
  identity block.
- **(14) DRep status grounding** — touched by task-136. Canonical on-chain status
  stays `active | inactive`; `expiring` is renderer-derived display state and is
  never stored, never widened into `DRepStatus`; `retired` stays deferred.
- Context only (untouched): **(3)** anchor transport-security floor — cv-2 fetches
  no anchor; **(5)** lovelace losslessness — cv-2 adds no IPC and parses no
  lovelace; **(12)** favorites — untouched.

## Dependencies

- **In-slice chain (built order):** 143 → 144 (storybook fixtures); 136 → 139;
  137 → 138 → {139, 140}; 173 → {141, 175}; 141 → 142; {139, 144} → 145;
  {136, 142} → 146; {140, 142, 145, 146} → 147; 140 → 148.
- **Cross-slice prerequisites, all landed:** task-106 `GovernanceStore`
  (`complete`), task-109 / task-110 / task-111 sanitization floor (`verified`),
  task-113 dialog identity prop (`verified`), task-115 HW delegate path
  (`complete`), task-129 `normalizeDRepIdentity` (`complete`), task-131
  `Wallet.currentVote` (`complete`), task-132 `CurrentVoteSummary` core
  (`complete`), task-171 ja-JP `!!!` guard (`complete`).
- **No cv-2 task depends on anchor-1, anchor-2, slice-8, or the standing phase.**
  cv-2 hands two obligations forward (D-5) and inherits none that block it.
- **Environment:** `nix` is absent, so `nix fmt` cannot run here and stays a
  pre-merge obligation. `gh` and push credentials are absent; work stays local.
  No browser, so the ja-JP visual/overflow pass (task-145 AC-4, task-146 AC-3)
  cannot execute in this container and is OWED, exactly as cv-1 recorded.

## Corpus-vs-Repo Corrections cv-2 Inherits

Recorded here so no guide author re-derives them. **Live repo wins**
(prompt.md:39-41).

| # | corpus claim | live repo | disposition |
|---|---|---|---|
| C-1 | `drepIndex[drepId]` bracket access (tasks JSON `:1181`, `:1241`; design `:59`, `:101`, `:189`) | `Map` at `GovernanceStore.ts:100`; `.get()` is the access (`DRepDetailPage.tsx:91`); `tsconfig.json:79-80` disables `strict`, so a bracket read compiles and yields `undefined` | rewrite as `resolveExactDRepMatch(...)` (D-6). `cv-1-findings.md:619` is a quotation, not a prescription — no edit |
| C-2 | `givenName` on the index entry; type `DRepIndexEntry` | neither exists; `givenName` appears once, as a negative fixture (`VotingPowerDelegationConfirmationDialog.spec.tsx:85-98`) | D-5 split + deferral + orphan flag |
| C-3 | an `expiring` `DRepStatusBadge` variant | `DRepStatus` is a closed two-value union (`governance.types.ts:35`); badge has two labels (`DRepStatusBadge.tsx:7-18`, `:26-29`) and two scss rules (`.scss:25-41`); no triangle glyph anywhere | D-1 local badge. Also: `Expiring in 7–12 epochs` is already taken as a filter label (`DRepDirectoryFilters.tsx:60-63`) and `CurrentVoteSummary.spec.tsx:61-63` asserts the badge's absence — both budgeted (D-9, S-2) |
| C-4 | "no epoch in the renderer"; drop site `GovernanceStore.ts:379-387` | an epoch exists (`NetworkStatusStore.ts:123`); the real drop site is `fetchDRepList`'s `runInAction` (`:251-259`); `_rehydrateDReps` drops nothing | constraint restated as "no per-DRep expiry epoch crosses IPC"; `drepActivity` is the only source (see "What cv-2 Does NOT Include") |
| C-5 | task-173 AC-5 and half of task-140 AC-7 still owed; comparator sentence at `:95` | `2ee5f74cf` discharged both; the sentence is at `:97`; two corrections remain, at two files | D-4 |
| C-6 | task-145: `GOVERNANCE_WALLETS` "L57-83, reuse at L228 / L427-458"; task-142: "HW status ~L118-L127" | definition `:63-97`, **four** reuse sites `:233`, `:420`, `:457`, `:492`; HW branch `:179-185` and never at L118-127 at any commit | D-8 (re-anchor + extra site), D-3 (semantic re-identification) |
| C-7 | `GovernanceWrapper` appears nowhere | absent from code, but already named at `current-vote-display-design.md:25`, tasks JSON `:1363-1371` and `cv-1-code-review.md:1680` | cite the design contract, do not invent (S-6) |
| C-8 | three governance stories exist but are unregistered at `index.ts:15-18` | correct gap; correct anchor is `index.ts:17-18` (`:15-16` are voting); already recorded as a deferral at `cv-1-implementation-guide.md:1851-1857` | D-12 explicit out |
| C-9 | `generateWallet` cannot express `votingTarget` | confirmed: `storybook/stories/_support/utils.ts:104-142`, nine positional params, no `votingTarget` in the `new Wallet({…})` literal | S-6 constructs `Wallet` directly; the helper is not widened |
| C-10 | invariant 4 holds | confirmed — no `GovernanceStore` reference in `VotingStore.ts` | keep it that way; the `same_vote` path spans six sites (D-15) |
| C-11 | design §12 test filenames are `.test.ts` | live convention is `.spec.ts`; the floor suite is `tests/jest/security/governance-sanitization.spec.ts` | already cv-1 F-3, record-only |
| C-12 | prettier `--check` red on four files | red on **238** files repo-wide; 12 within the governance/voting surface; `source/renderer/app/containers/voting/Governance.tsx` cannot be **parsed** by prettier 2.1.2 (line 4 inline `type` import) and makes any run containing it exit 2 | see DoD's formatting rule |

## Risks / Open Questions

- **R-1 (high) — the deferred `givenName` work is orphaned unless anchor planning
  acts.** D-5 strikes two clauses from task-139 AC-3 and points them at anchor-2,
  but no anchor task owns them today and no anchor task adds the store field they
  need. *Mitigation:* the two required tracker edits are named explicitly in D-5
  and repeated in the Final Outcome's residual-gaps section; the anchor-1 and
  anchor-2 planning passes must action them. **If the user prefers the re-point be
  made now rather than recorded, the blocking question is: "Should cv-2 amend the
  anchor-1 task-151 and anchor-2 task-154/task-157 rows to take ownership of the
  `CurrentVoteSummary` verified-name render, or leave that to anchor planning?"**
  This is recorded as an option, not a stop condition — cv-2 can complete without
  it.
- **R-2 (medium) — task-136's badge breaks committed assertions and snapshots.**
  `CurrentVoteSummary.spec.tsx:61-63` and the four colocated snapshots fail the
  moment a lifecycle badge renders. *Mitigation:* S-2 makes the spec rewrite and
  snapshot regeneration part of task-136's scope, not a surprise at review.
- **R-3 (medium) — task-140's disable can hide the `same_vote` server net.** A
  client-side gate that works makes the server path unreachable through the UI.
  *Mitigation:* task-148 pins it at the store level where the UI gate does not
  apply, plus a render assertion; S-7 requires at least one story where the input
  differs from the wallet's `currentVote` so the error knob stays exercisable.
- **R-4 (medium) — four independent copies of an expiry window.** Adding a fifth
  in `CurrentVoteSummary` risks silent divergence. *Mitigation:* D-2 fixes the
  value, requires the `helpers.ts:177-180`-style comment, and forbids importing
  `DRepCategoryBadge`'s constants (which are not exported anyway).
- **R-5 (medium) — task-142 and task-175 edit the same file in sequence.**
  *Mitigation:* D-3 scopes 142's assertions by section and forbids whole-dialog
  snapshots; 175 re-runs them as its own gate.
- **R-6 (medium) — the domain-object logging surface is wide.** `filterLogData`
  guards no renderer-domain name; a single careless `logger.debug({ wallet })` in
  a cv-2 path would leak the full identity. *Mitigation:* S-9's stricter
  invariant plus task-147's spies; the fallback key-list extension is specified so
  a reviewer does not improvise a two-key patch.
- **R-7 (low) — react-polymorph `Button` may not forward `aria-*`.** *Mitigation:*
  D-10 pre-decides the fallback (visible hint alone satisfies AC-2) so the
  implementer does not reach for a DOM escape hatch.
- **R-8 (low) — ja-JP overflow is unverifiable here.** No browser in this
  container. *Mitigation:* recorded as OWED at slice close, exactly as cv-1 did;
  the badge label `Expiring in {n} epochs` / `あと{n}エポックで失効` is the longest
  new string and is the specific overflow candidate to check.
- **R-9 (low) — `resolveExactDRepMatch` is being used outside the directory.**
  It is exported and generic, but it lives under
  `components/governance/drep-directory/`. *Mitigation:* the import direction
  mirrors the existing cross-directory import of `_shared/DRepIdDisplay` from
  `CurrentVoteSummary.tsx:5`; if a reviewer objects, the alternative is to move
  the helper to `source/renderer/app/utils/governance/` — a rename, not a
  redesign. **task-147** pins the lookup: its Step 2 flow cases assert the badge
  and caption rendered from the store-backed `drepIndex`, and a second case
  whose wallet carries a CIP-105 `drep.raw` with `drep.cip129` set proves the
  query is `cip129 ?? raw` (D-6). task-136's unit cases pass `drepEntry` in
  directly and exercise no lookup; task-139 adds no spec file.
- **Resolved, not open:** O-1 … O-8 are all closed by D-1 … D-14. No cv-2 task
  requires an `interactive_decision`, `interactive_validation`, or
  `manual_execution` classification.

## Definition of Done

**Per task** (prompt.md:263-265): acceptance criteria met · verification executed
and reported · code review clean · tasks JSON synchronized (`status`,
`statusReason`, `evidence`, `updatedAt` as `YYYY-MM-DD`) · exactly one commit,
subject-only Conventional Commits, `<type>(gov): task-NNN <short imperative
summary>`.

**Per slice:**

- All 61 verbatim acceptance criteria above pass, except the thirteen scoped
  below. Each carries a truthful `statusReason`; none may be reported green.

  | criterion | disposition | reason |
  |---|---|---|
  | task-136 AC-4 | satisfied in part | knob delivered; "renders without console errors in en-US and ja-JP" needs a browser — **OWED**, and only observable after task-146 seeds the catalogs (until then Storybook legitimately logs `[React Intl] Missing message`) |
  | task-138 AC-3 | satisfied in part | the re-seed is built; it fires only while `drepInputState.dirty === false`, and the criterion's "data changed" indicator alternative is deliberately not built (D-11) |
  | task-139 AC-2 | satisfied in part | mount point matches the IA structurally; the Storybook visual confirmation of the mounted panel is task-145's and needs a browser — **OWED** |
  | task-139 AC-3 | split | D-5 — `givenName` has no data source in cv-2 |
  | task-142 AC-3 | semantically re-anchored | D-3 |
  | task-143 AC-4 | satisfied in part on both halves | D-7 — named provenance met for `drepVerified` only; "verified hash" has no mechanism in cv-2 |
  | task-144 AC-2 | split | structural half (children inside the keyed fragment, so a knob change replaces the subtree) proved by the code plus `tsc`; the *observed* remount — type an id, switch the knob, field blank — is a browser check discharged by task-145 Step 8 and recorded **OWED** until that pass runs |
  | task-145 AC-1 | scoped | D-8 — "every governance story" is scoped to the wallet-bearing stories |
  | task-145 AC-4 | not satisfiable in this container | no browser, so neither the console pass nor the overflow pass can run — **OWED** |
  | task-146 AC-3 (second half) | not satisfied | ja-JP length / layout overflow review needs a running Storybook — **OWED** |
  | task-147 AC-5 | scope-narrowed | read as: no DRep identifier in any logger/analytics/electron-store payload, and no sentinel literal in any **logger** payload. The analytics vote kind is the reviewed exception (invariant 2 carve-out above; `research/cv-2-findings.md` F-14) |
  | task-173 AC-2 | satisfied in part at its own commit | "the dialog still renders the raw string verbatim" is discharged by task-175 Step 3, which owns the dialog's branch predicate |
  | task-140 AC-7 (first conjunct) | satisfied in part | D-4 — `designs/current-vote-display-design.md:97` still contains the `case-insensitive cip129` alternative that AC-4 bans; task-140's appended sentence supersedes it rather than deleting it, because that file is append-only on this row |

  Every row marked **OWED** depends on the same absent browser; they are
  discharged together, or recorded together as still owed, at slice close.
- **Gates, run from the worktree root:**
  - `node_modules/.bin/tsc --noEmit` — exit 0 (baseline: 0 at HEAD, TypeScript
    4.9.5). Use `yarn compile` instead whenever a new `.scss` class lands, because
    its `precompile` hook regenerates the gitignored `*.scss.d.ts` files.
  - `yarn lint` — exit 0 (baseline: exit 0 with **5591** warnings, ~39 s).
  - `node_modules/.bin/jest --testPathPattern='(governance|Governance|voting|Voting|DRep)' --no-coverage --runInBand`
    — baseline at HEAD: **17 suites passed, 1 skipped, 18 total; 269 passed, 12
    skipped, 281 total; 6 snapshots; ~8.5 s**. The one skipped suite is
    environment-gated (`GovernanceCliArgvSmoke.spec.ts:28` self-skips because
    `cardano-cli` is not on PATH), not broken. `--no-coverage` is load-bearing
    (`jest.config.js:19` sets `collectCoverage: true`).
  - `tests/jest/security/governance-sanitization.spec.ts` green (invariant 2).
  - `yarn i18n:manage` runs clean and idempotent after task-146 — the second run
    adds zero keys and deletes zero keys. **task-146's own changes to
    `en-US.json`, `ja-JP.json`, `defaultMessages.json` and
    `translations/messages.json` are kept and ride its commit**; restoring them
    would delete exactly the entries its AC-1 requires. The restore rule binds
    every *other* file the manager rewrites, and every *other* task that runs it
    incidentally: anything clean at HEAD and outside those four is `git restore`d.
- **Formatting rule (binding).** `node_modules/.bin/prettier --write` on
  **explicitly listed files this slice CREATES only** —
  `isSameVoteTarget.ts`, `isSameVoteTarget.spec.ts`, `_utils/fixtures.ts`,
  `_utils/GovernanceWrapper.tsx`. Never `yarn prettier` (its script carries a
  repo-wide `"**/*.*"` glob). Never a pre-existing file: **238 files are
  prettier-2.1.2-dirty at HEAD**, including four of cv-2's five main targets.
  Never a tool-managed JSON (the tasks tracker, the locale catalogs,
  `translations/messages.json`). **Never include
  `source/renderer/app/containers/voting/Governance.tsx` in any prettier
  invocation** — prettier 2.1.2 cannot parse its line-4 inline `type` import and
  exits 2, failing the whole command; cv-2 does not edit that file, and no task
  may batch it in. `nix fmt` is unavailable here and stays a pre-merge obligation
  the user runs.
- **Never read `yarn check:all` as a cv-2 regression** — it is red at HEAD for
  unrelated reasons (`storybook:build` manager-webpack JSX loader; `prettier:check`
  per above). `yarn storybook` (dev server) is the real Storybook floor.
- **Never `git stash`** — the stash stack is shared across worktrees and
  concurrent sessions. Discard with `git restore` / `git checkout -- <paths>`.
- Storybook: **the whole visual pass is OWED, not a runnable gate.** There is no
  browser in this container (F-12's environment), so neither "all five knob
  values plus the four `DRep status (mock)` values render without console errors
  via the global English/Japanese toggle" nor the ja-JP overflow check (R-8) can
  be observed here. What cv-2 *can* gate is the compile-level floor: `tsc`,
  `yarn lint`, and the `yarn storybook` dev-server bundle building. Record both
  halves as OWED at close; never assert either green.
- `grep -n GOVERNANCE_WALLETS storybook/stories/voting/Governance.stories.tsx`
  returns nothing (task-145 AC-3).
- `grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts` returns
  nothing (invariant 4).
- `grep -rn 'task-1[0-9][0-9]' source/ tests/ storybook/` returns nothing (no
  process artifacts in code comments or test names).
- D-4's two corrections exist: an appended entry in `cv-1-code-review.md` and an
  in-place fix at `research/cv-1-findings.md:220-227`.
- `research/cv-2-findings.md` written (or `no new research` recorded in the Final
  Outcome), the code-review log preserved with `Planner:` open/close, `Critiquer:`
  and per-task `Code Review:` entries, and this PRD's Final Outcome filled.
- The phase object carries **no `auditSummary`** — only `slice-1` has one and cv-1
  deliberately added none; cv-2 does not invent one.

## Final Outcome

*(Placeholder — filled at slice close, following the cv-1-PRD.md:497-639
structure.)*

### What shipped, task by task

| task | commit | outcome |
|---|---|---|
| task-143 | | |
| task-136 | | |
| task-137 | | |
| task-138 | | |
| task-139 | | |
| task-140 | | |
| task-173 | | |
| task-141 | | |
| task-142 | | |
| task-175 | | |
| task-144 | | |
| task-145 | | |
| task-146 | | |
| task-147 | | |
| task-148 | | |

### Gates at close (measured, not asserted)

*(tsc / lint / focused Jest suite+test counts / i18n idempotence / prettier scope
— record the actual numbers, not "green".)*

### Deviations from this PRD and its guide

*(Including any D-decision that did not survive contact with the code.)*

### OWED at close (nothing here is faked green)

*(Expected entries, mirroring the Definition of Done exception table — record the
disposition actually reached for each, never a blanket "green":*

- *the browser-dependent set, all blocked by the same absent browser: task-136
  AC-4's console-clean pass, task-139 AC-2's visual confirmation, task-144 AC-2's
  observed remount, task-145 AC-4's console + overflow pass, task-146 AC-3's
  ja-JP length/layout review, and the Storybook slice gate in both locales (R-8);*
- *the pre-merge `nix fmt` run the user owns (F-12);*
- *task-143 AC-4's two halves — named CIP-119 provenance for `drepUnverified` and
  the Blake2b-256 anchor verify (D-7);*
- *the `verified` promotions that need dedicated proof beyond in-task tests.)*

### Residual gaps a later phase inherits

*(Expected entries: task-139 AC-3's struck clauses and the two tracker edits
anchor-1/anchor-2 must make (D-5, R-1); the three unregistered governance story
files (D-12); the `filterLogData` domain-name gap if the stricter invariant is
ever relaxed (S-9, R-6); task-143 AC-4's "verified hash" half (D-7).)*

## References

- Tasks tracker: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1162-1457`
- Orchestration prompt: `.agent/plans/governance/drep-discovery/prompt.md`
- Working conventions: `.agent/plans/governance/drep-discovery/README.md`
- Parent plan: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`
- Designs: `designs/shared-design-tokens.md`, `designs/current-vote-display-design.md`, `designs/current-vote-display-ux.md`
- Preceding slice: `task-plans/cv-1-PRD.md`, `task-plans/cv-1-implementation-guide.md`, `task-plans/cv-1-code-review.md`, `research/cv-1-findings.md`
