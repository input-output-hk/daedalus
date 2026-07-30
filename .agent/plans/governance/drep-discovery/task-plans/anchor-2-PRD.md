# ANCHOR-2 PRD: Anchor Enrichment Completion

> **Planning Status:** `in_review` | **Slice Status:** open (all 6 tasks `pending`) | **Date:** 2026-07-30 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `anchor-2` — "Anchor 2 - Anchor enrichment completion" (`riskLevel: high`; tasks JSON `:1792-1927`)
> **Tasks:** 6 — task-153, task-174, task-154, task-155, task-156, task-157 (all `pending` at the planning anchor `55e8985bf`)
> **Preceding slice:** [anchor-1-PRD.md](./anchor-1-PRD.md) (closed 2026-07-30 at `55e8985bf`)
> **Place in the locked slice order** (`prompt.md:146-149`): `… → cv-1 → cv-2 → anchor-1 → **anchor-2** → slice-8`. anchor-2 is the penultimate slice; `slice-8` closes the feature.
> **Findings:** `research/anchor-2-findings.md` — optional; if anchor-2 produces no new research, "no new research" is recorded in the Final Outcome instead.
> **Implementation guide:** `anchor-2-implementation-guide.md` (authored after this PRD)
> **Evidence basis.** This PRD was authored against a verified, line-anchored grounding brief of the whole planning corpus, whose anchors were re-verified against the worktree at `55e8985bf` (byte-identical to the `feat/drep-discovery` tip). Every `path:line` below traces to that verification. Where the tasks JSON or a design doc cites an anchor that no longer matches, the drift is recorded in **Corpus-vs-Repo Corrections anchor-2 Inherits** and the live repo wins.

---

## Executive Summary

anchor-1 opened the outbound socket, proved the bytes, and spent exactly one
render on the result: `givenName`, in the detail view, behind a *Verified
off-chain content* label. anchor-2 spends the rest of the CIP-119 payload — and
then stops the feature from ever rendering anchor-derived content without a
provenance label again.

- **The CIP-119 pipeline is widened once, at the front.** Today
  `parseVerifiedContent` (`source/main/governance/AnchorVerificationService.ts:43-62`)
  extracts exactly one field and returns `null` when `givenName` is missing
  (`:60`), and the wire type is a single-property interface
  (`source/common/types/governance.types.ts:93-96`). `objectives`, `motivations`,
  `qualifications`, `references`, `paymentAddress` and `doNotList` are parsed by
  nothing and cross no seam. **task-157 widens the type, the parser, the log
  redaction list and `AnchorEnrichEntry` in one pass** (D-2 / S-1…S-3); every
  later task in the slice consumes an already-widened contract.
- **The `givenName`-required parser inverts author intent and is relaxed.** A
  CIP-119 document carrying `doNotList: true` and no `givenName` parses to `null`
  → `ParseFailed` → `unavailable` today, so the DRep **stays in the default
  cohort** — the exact opposite of what it asked for. task-157 makes every parsed
  field optional and moves the "no name, no block" decision to the renderer,
  which already holds that guard (`DRepDetailAnchorContent.tsx:66`).
- **`doNotList` becomes a cohort exclusion, not a disappearance.** task-153
  projects the flag onto `AppDRepDirectoryEntry` and filters `defaultCohort`
  (`GovernanceStore.ts:211-225`) only. `showAllList` (`:279-282`), the search
  index (`DRepDirectory.tsx:146-149`) and `drepIndex` (`:131`, rebuilt `:449`)
  are untouched, so a `doNotList` DRep stays reachable by show-all, search and
  direct ID entry — which the design doc already promises at
  `drep-discovery-design.md:239`.
- **Identity gets its second form and its verified name.** task-174 gives
  `DRepIdDisplay` an opt-in both-forms mode deriving CIP-105 through
  `normalizeDRepIdentity` (`source/renderer/app/utils/governance/normalizeDRepIdentity.ts:17-62`),
  shipping shared-design-tokens §4 `:78` (detail: both forms, monospaced, a copy
  button each) and §11 `:248` (deduped search rows: both forms stacked). Cards
  stay CIP-129-primary and ID-only per `drep-discovery-design.md:251-259`.
  task-154 then fills the name slot the confirmation dialog has been holding open
  since cv-2 (`VotingPowerDelegationConfirmationDialog.tsx:163-165`) — and
  deletes that reservation comment as it does.
- **Then one sweep, then one confirmation.** task-155 runs *after* every
  anchor-derived surface exists, so it is a single audit pass — one section-level
  on-chain label on `DRepDetailOnchainSection` (which imports no `DRepSourceLabel`
  today, `:1-7`), rendered as a `Source` row in that section's `<dl>` and costing
  one new key (D-4), an audit of every remaining anchor-derived render, and
  regression coverage over the dialog. task-156 closes the slice by confirming
  `Abstain` / `No Confidence` never became directory entries.

Byte-equality is the release-blocking constraint of the slice: task-154 AC-2/AC-3
and task-174 AC-3 all assert that adding a name and a second ID encoding changes
nothing that reaches the signer or the device. **Zero npm dependencies are added
and zero new IPC channels are opened** — anchor-2 rides
`GOVERNANCE_DREP_ANCHOR_CHANNEL` (`source/common/ipc/api.ts:670-672`) unchanged.

## Problem Statement — Why Now

- **Five verified fields are fetched, hash-checked, and thrown away.** The
  transport, digest gate and immutable cache all landed in anchor-1, and
  `resolveFromCacheOrFetch` (`AnchorVerificationService.ts:66-95`) already gates
  every step on the Blake2b-256 check (`:85-87`, comment `:64-65`). The only
  reason `objectives` / `motivations` / `qualifications` / `references` /
  `paymentAddress` do not render is that `parseVerifiedContent:57-61` never reads
  them. The security cost of rendering them is already paid; only the plumbing is
  missing.
- **`doNotList` is a stated author preference the app currently ignores.** The
  cohort filter (`GovernanceStore.ts:216-221`) tests `status` and `drepActivity`
  and nothing else. A DRep who publishes `doNotList: true` is still promoted into
  the Recommended default view. `drep-discovery-design.md:239` already commits
  the app to honouring it "once `doNotList` lands in `anchor-2`" — the doc is
  ahead of the code.
- **Two shipped design contracts are unbuilt on the identity surface.**
  shared-design-tokens §4 `:78` ("In detail, show both forms fully, monospaced,
  each with its own copy button") and §11 `:248` (deduped search rows show both
  ID forms stacked) are both unshipped: `DRepIdDisplay` takes one `drepId` prop
  (`:28-32`) and renders one truncated `<code>` with one copy button (`:71-84`).
  The detail wireframe at `drep-discovery-design.md:84-85` already draws the
  CIP-105 line that no code emits.
- **The confirmation dialog is holding a slot open with a comment.**
  `VotingPowerDelegationConfirmationDialog.tsx:163-165` reads: "Rendered
  untouched: must stay byte-equal to chosenOption and the delegateVotes dRepId.
  Name slot is reserved for anchor-2; unverified names never render here." That
  is a live, dated, owner-assigned obligation on task-154, and it is the one
  existing comment in the governance tree that names a slice — it must be
  deleted, not left as stale history, when the slot is filled.
- **`verifiedName` is an orphan field.** `AppDRepDirectoryEntry.verifiedName`
  (`GovernanceStore.ts:35`) is written by `_applyVerifiedNames` (`:521-538`),
  hash-guarded, and already in the log-redaction list
  (`source/common/utils/logging.ts:63`) — but **read by no component**. The
  detail view reads `state.givenName` off `AnchorEnrichEntry`
  (`DRepDetailAnchorContent.tsx:66,79`) instead. Two live name channels with one
  consumer between them is a defect anchor-2 must close before it adds a third
  surface (D-9).
- **The provenance rule has one measurable gap.**
  `shared-design-tokens.md:45`: "Every rendered field gets an explicit provenance
  label. This is the single most important anti-misleading-content control."
  `DRepDetailOnchainSection.tsx` imports no `DRepSourceLabel` at all (`:1-7`);
  its provenance rides entirely on the section heading string "!!!On-chain"
  (`:11`). task-155 is the row that closes it.

## Canonical Build Order

The tasks JSON listing order for anchor-2 is **`153, 174, 154, 155, 156, 157`**
(objects at `:1798-1826`, `:1827-1849`, `:1850-1867`, `:1868-1885`,
`:1886-1902`, `:1903-1925`). Dependencies verified per row — **every dependency
of every task is `complete`, so anchor-2 has no intra-slice dependency edges at
all and the JSON order carries no ordering information:**

| # | task | `dependencies` (json) | all `complete`? | in-slice edges |
|---|---|---|---|---|
| 1 | task-157 | task-151, task-152 | yes | none |
| 2 | task-153 | task-151, task-118, task-122, task-172 | yes | none |
| 3 | task-174 | task-107, task-116, task-121, task-129 | yes | none |
| 4 | task-154 | task-151, task-115 | yes | none |
| 5 | task-155 | task-151 | yes | none |
| 6 | task-156 | task-151 | yes | none |

**Canonical anchor-2 build order (binding):**

```
157 → 153 → 174 → 154 → 155 → 156
```

This deviates from the JSON listing order deliberately and on technical grounds
(D-1). The constraints that force it:

- **157 before 153** — both extend `VerifiedDRepAnchorContent`
  (`governance.types.ts:93-96`) and `parseVerifiedContent`
  (`AnchorVerificationService.ts:43-62`). Whichever runs first pays the plumbing
  cost. 157 needs five fields there; 153 needs one boolean. Widening once, in the
  wider row, means task-153 adds a projection to an already-widened contract
  rather than the reverse.
- **153 after 157's parser relaxation** — task-153 AC-1 is unsatisfiable for a
  `doNotList: true` document that omits `givenName` until `parseVerifiedContent`
  stops returning `null` on a missing name (`:60`).
- **174 and 154 before 155** — task-155 is a sweep, and a sweep that runs before
  the surfaces it audits guarantees a second sweep. task-154 adds the
  confirmation dialog's verified-name line; that line is itself anchor-derived
  content that task-155 AC-2 must confirm carries a label.
- **155 after 157** — task-157 attaches the verified-off-chain label to every
  field it adds (S-7). Running 155 first would label a detail view that is about
  to grow five unlabelled fields.
- **156 last** — it is a verification row over the *end state* of the directory,
  its empty state and the confirmation dialog. Running it earlier verifies a
  directory that four later commits still change.

**Recorded deviation from the grounding brief.** The prep pass recommended
`157 → 153 → 155 → 174 → 154 → 156`, placing the sweep third. That ordering
leaves task-154's confirmation-dialog name line outside the sweep's audit
window. The canonical order above supersedes it.

## Per-Task Contract (interaction mode, scope, non-goals, dependencies)

**No anchor-2 task is in the locked non-autonomous set.** The set names exactly
task-125 (`manual_execution`), the task-166 remainder (`manual_execution`),
task-158 (event-driven standing guardrail) and the release-end `!!!` copy review
(user-owned). None is in this phase. Planning resolved every open question named
in the grounding brief (G-1…G-7) plus five further gaps recorded as D-10…D-14;
**no anchor-2 task is escalated**, so all six rows are `autonomous`.

| Task (build pos / json pos) | Mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-157** — Render remaining verified CIP-119 profile fields (build 1 / json 6) | `autonomous` | **Owns the single widening of the CIP-119 pipeline (S-1…S-3):** `VerifiedDRepAnchorContent` (`governance.types.ts:93-96`) gains `objectives`, `motivations`, `qualifications`, `references`, `paymentAddress` **and `doNotList`**; `parseVerifiedContent` (`AnchorVerificationService.ts:43-62`) is relaxed so every field is optional and a missing `givenName` no longer returns `null`; per-field length policy replaces the blanket 80-char `readCip119String` clamp (D-10); `filterLogData`'s key list (`logging.ts:44-63`) gains all six names; `AnchorEnrichEntry.verified` (`GovernanceStore.ts:49-52`) carries the widened content. Renders the five profile fields in `DRepDetailAnchorContent.tsx` with a verified-off-chain label each, the `references` `@type` split (Link / Identity / default bucket) per `drep-discovery-design.md:220-224`, and the read-only `paymentAddress` block per `:226`. Reference URIs route through the hardened path via a shared `isHttpsAnchorUrl` predicate (D-13). | **DETAIL VIEW ONLY.** AC-2's confirmation half is delegated to task-154 (S-5, D-14) — task-157 makes the data available and adds nothing to `VotingPowerDelegationConfirmationDialog.tsx`. No `image` / `imageObject` (deferred, `drep-discovery-design.md:218`). No verified content on `DRepCard` (`:216`, `:251-259`). No cohort/filter/sort/search consumption of any new field — that is task-153's `doNotList` projection only. No new IPC channel. No `doNotList` projection onto `AppDRepDirectoryEntry` (task-153 owns that). No pre-population of any send-form or delegation-form field from `paymentAddress`. | task-151 ✔, task-152 ✔ |
| **task-153** — Honor CIP-119 `doNotList` in default cohort (build 2 / json 1) | `autonomous` | Projects `doNotList` from `anchorStateByDRepId` onto `AppDRepDirectoryEntry` (`GovernanceStore.ts:23-36`) through the same hash-guarded pass that projects `verifiedName` (`:521-538`, D-11); excludes `entry.doNotList === true` from `defaultCohort` (`:211-225`); extends `isStaleFavorite` (`helpers.ts:279-288`) to recognise the flag alongside the deferred `retired` status; re-verifies task-122 AC-5 against the real predicate and updates task-122's tracker row in the same commit. | **CONSUMES `doNotList` as already present on the wire type, the parser and `AnchorEnrichEntry`** (S-1, S-2) — it must not re-declare or re-widen any of them. No filtering of `showAllList` (`:279-282`), `top35DRepIds` (`:264-272`), the search index (`DRepDirectory.tsx:146-149`) or `drepIndex` (`:131`, `:449`). No new `DRepStatus` member (AC-5, invariant 14). No `Retired` / `Excluded from default cohort` badge — unimplementable, see D-7. No bulk anchor prefetch to make the flag globally known (D-5). No design-doc edits beyond D-7's `:112` strike; AC-9/AC-10/AC-11/AC-12 are verify-and-record (D-6). | task-151 ✔, task-118 ✔, task-122 ✔, task-172 ✔ |
| **task-174** — Dual CIP-129 / CIP-105 DRep ID display (build 3 / json 2) | `autonomous` | Additive opt-in both-forms mode on `DRepIdDisplay` (`:28-32`, `:71-84`), deriving CIP-105 internally via `normalizeDRepIdentity` and omitting the second row when the id does not decode; wired into `DRepDetail.tsx:113` (full, monospaced, a copy button each) and into deduped search-result rows by threading a search-result boolean `DRepDirectory.tsx:137-140` → `DRepDirectoryList.tsx:88-100` → `DRepCard.tsx:126` → `DRepIdDisplay` (D-8, D-12); per-form copy and aria labels in both locales; the task-111 spy suite re-asserted over the render and copy paths. | Cards keep the CIP-129-primary truncated single line with one copy button (`DRepCard.tsx:126`). No change to `buildDRepSearchIndex` (`helpers.ts:70-84`), `searchDRepsByIdPrefix` (`:106-131`), `filterDReps` (`:189-220`) or `sortDReps` (`:243-277`) — the search pipeline's carrier type is not widened (D-8). No hand-rolled bech32 re-encode. No change to the confirmation dialog's §7 block (task-175 owns it; task-154 owns the name line). No bech32 string in either clipboard warn payload (`DRepIdDisplay.tsx:52-54`, `:62-65`). AC-6 is verify-and-record (D-6). | task-107 ✔, task-116 ✔, task-121 ✔, task-129 ✔ |
| **task-154** — Confirmation dialog verified name, byte-equality preserved (build 4 / json 3) | `autonomous` | Exactly two things per `shared-design-tokens.md:135`: the `{verified givenName}` line and the extended `On-chain · Name: Verified off-chain content` source label, composed from two `DRepSourceLabel` instances (S-6). Sources the name from `entry.verifiedName` via `governance.drepIndex` in `VotingGovernancePage.tsx:84-87` (D-9) and passes it as a new prop into `VotingPowerDelegationConfirmationDialog.tsx` (props `:55-71`). Deletes the reservation comment at `:163-165`. Discharges task-157 AC-2's confirmation half (D-14). Release-blocking HW assertion in `shelleyLedger.spec.ts` / `shelleyTrezor.spec.ts` that the on-device DRep ID equals `vote.chosenOption`. Adds the new `voting.governance.confirmationDialog.*` key to `PRELIMINARY_CONFIRMATION_KEYS` (`preliminaryCopyMarkers.spec.ts:18-21`). | Does not touch the pre-anchor block contents above the name line (`shared-design-tokens.md:135`). Never renders an unverified name (`:133`). No change to `chosenOption` (`VotingPowerDelegation.tsx:242-245`), `drepIdentity` decoding (`VotingGovernancePage.tsx:84-87`), the signed-payload string (`:190`) or `VotingStore.delegateVotes` (`VotingStore.ts:424-425`). No new `DRepSourceLabel` variant. No name in the sentinel branch (`:201-210`). `VotingStore` still never reads `GovernanceStore` (invariant 4). | task-151 ✔, task-115 ✔ |
| **task-155** — Apply source labeling to DRep Discovery content (build 5 / json 4) | `autonomous` | **A sweep, not a re-label.** Adds ONE section-level `DRepSourceLabel source="on-chain"` to `DRepDetailOnchainSection.tsx`, as a `Source` row at the end of the section's `<dl>` and **not** beside the `<h2>` — the heading and the pill are byte-identical strings in both locales (D-4). That row's `<dt>` costs one new key, `governance.drepDetail.onchain.source`, so this task is **+1 i18n key, not zero**. Audits every remaining anchor-derived render across the directory card, the detail view sections and the confirmation dialog for a label; adds the regression assertions that close AC-2 as a standing check. Confirmation-dialog coverage is **assertion-only** — task-154 lands the label first in the canonical order (D-3). | Does not re-label the fields task-157 already labelled (S-7). Adds no `DRepSourceLabel` variant. **`CurrentVoteSummary` is out of scope** — it renders no anchor-derived content and already carries `source="on-chain"` at `:90`; its enrichment is cv-track work (R-3 / D-3). Puts no verified content on `DRepCard` (`drep-discovery-design.md:216`). No per-field label proliferation on the on-chain section. | task-151 ✔ |
| **task-156** — `Abstain` / `No Confidence` directory treatment (build 6 / json 5) | `autonomous` | Verification row. Asserts the directory never renders a sentinel row; asserts the confirmation dialog's sentinel branch (`:117-120`, `:201-210`) still works through the existing form path; confirms the empty-state copy (`DRepEmptyState.tsx:73-103`, `:105-123`) does not conflict. Records the IA rationale in `drep-discovery-design.md` as documentation, not in-app copy (D-15). | No new in-app strings — the app must not explain an absence to the user. No `DRepStatus`, `DRepSourceLabel` or `DRepEmptyState` variant changes. No sentinel entry in any list, index or fixture. | task-151 ✔ |

## Acceptance Criteria (verbatim from governance-drep-discovery-plan-tasks.json)

Quoted exactly as the tracker holds them, **in canonical build order**. The JSON
listing position is given for each. Annotations marked **[D-n]** re-scope or
correct a criterion and are binding on the guide author; the quoted text itself
is never edited.

### task-157 — Render remaining verified CIP-119 profile fields (build 1, json position 6; json `:1903-1925`)

*(`acceptanceCriteria` array at `:1915-1924`)*

1. "Verified objectives, motivations, qualifications, references, and paymentAddress render in the detail view, each with a verified off-chain content source label — the drep-discovery-design.md:215 render list minus givenName (task-151) and minus image, which is deferred for this release with the deferral recorded in that design doc."
   **[D-6]** The render list is at **`:216`**, not `:215`. The image deferral is **already recorded** at **`:218`** — verify the text is present at that anchor and record it in the task's `statusReason`; **do not add a second deferral paragraph.**
2. "Delegation confirmation shows the verified display name only when verified metadata is available."
   **[D-14]** Jointly discharged. task-157 makes the data available; the confirmation-dialog gating logic ships in **task-154's** commit (S-5). task-157's `statusReason` records the hand-off; task-154's AC-1 subsumes it.
3. "Chain-native view remains fully functional when anchor is unavailable or fails verification."
4. "Verified references render split by @type: Link entries as outbound links, Identity entries in a distinct sub-section, and entries with an unrecognised or missing @type in a documented default bucket; Jest asserts an Identity entry renders under the claim caption and never under the plain Link treatment."
5. "The Identity sub-section carries caption copy stating the identity is claimed by the DRep and unverified by Daedalus, with guidance to visit the URL and confirm the DRep ID is published there."
6. "Verified paymentAddress renders in the DRep detail view only, read-only with a copy button, under a stated-payment-address label alongside the verified off-chain content source label; the copy states the address is the DRep's own claim and that delegating voting power requires no payment to any address, and the value never pre-populates a send-form or delegation-form field."
7. "Reference URIs open only through the hardened HTTPS-only open-external-url path delivered by task-152."
8. "New en-US and ja-JP strings keep the leading !!! marker, and the detail view renders correctly when references or paymentAddress is absent from the verified payload."

### task-153 — Honor CIP-119 `doNotList` in default cohort (build 2, json position 1; json `:1798-1826`)

*(`acceptanceCriteria` array at `:1812-1825`)*

1. "Default cohort excludes DReps with `doNotList=true`."
   **[D-5]** True only for DReps whose anchor was fetched in the current session — the anchor fetch is lazy and per-detail-visit. Documented limitation; see D-5 for the user-visible consequence and the forbidden workarounds.
2. "Show-all and search still surface `doNotList=true` DReps."
3. "Direct DRep ID entry resolves `doNotList=true` DReps to their detail view."
4. "Jest covers both the excluded default-cohort path and the search/direct-entry reachability path."
5. "`isStaleFavorite` (drep-directory/helpers.ts:279-288) recognizes `doNotList=true` alongside the deferred `retired` status, reading the flag from the verified-metadata field this task adds to `AppDRepDirectoryEntry`; `DRepStatus` stays `'active' | 'inactive'` and gains no new member."
   **[D-2]** "the verified-metadata field this task adds to `AppDRepDirectoryEntry`" stands unchanged: task-153 **does** add the renderer-store projection. What it does not add is the wire type, the parser field or the `AnchorEnrichEntry` member — those are task-157's (S-1, S-2). Anchor `:279-288` verified accurate.
6. "A favorited `doNotList=true` DRep renders `governance.drepFavorites.staleCaption` in the favorites view through the real `isStaleFavorite` helper with no `isStaleFavoriteEntry` prop injected, and still renders no caption in the directory view."
7. "The `doNotList` transition never purges a favorite: the entry stays in the favorites list with its status badge and inline caption per drep-discovery-design.md:111."
   **[D-7] REWRITTEN — not implementable as stated.** Correct anchor is **`:112`**, and `:112` claims a `Retired` / `Excluded from default cohort` status badge that cannot exist: `DRepStatusBadge.tsx:26-29` builds an exhaustive `Record<DRepStatus, string>` over `active | inactive`, and AC-5 forbids adding a `DRepStatus` member. **Implement as:** the favorited entry keeps its real `active`/`inactive` badge **plus** the inline `governance.drepFavorites.staleCaption` (`DRepCard.tsx:128-132`), and `drep-discovery-design.md:112` is reconciled by striking the impossible badge claim (see Doc Reconciliations).
8. "task-122 AC-5 is re-verified against the real predicate, and task-122's tracker status/statusReason are updated in this task's commit to record that the previously-deferred acceptance path is now exercised."
   **[D-16]** The tasks JSON is tool-managed: edit the row's `status`/`statusReason` values only, preserve surrounding formatting, and never run prettier over the tracker.
9. "drep-discovery-design.md:228 defines `Show all` as the full registration list — top-35, sub-floor and inactive entries included — in the seeded session order, matching `GovernanceStore.showAllList`."
   **[D-6] ALREADY SATISFIED at `:239`** (not `:228`, which is the `## Default-Cohort UX` heading). Verify and record; make no edit.
10. "The same sentence records that `doNotList=true` DReps remain reachable through show-all, search and direct DRep ID entry, matching this task's cohort-exclusion change."
    **[D-6] ALREADY SATISFIED** by the same sentence-block at `:239`. Verify and record; make no edit.
11. "The favorites empty-state illustration is resolved one way: either the asset ships in `DRepEmptyState`'s `noFavorites` branch, or 'prominent illustration' is struck from drep-discovery-design.md:109 — the decision is recorded, not left as drift."
    **[D-6] ALREADY RESOLVED at `:110`** (not `:109`, which is blank) as "No illustration ships — the earlier 'prominent illustration' claim is resolved as dropped, not deferred", matching `DRepEmptyState.tsx:105-123`. Verify and record; make no edit.
12. "The remainder of the show-all section is left intact: the opt-in sorts and the popularity-sort guardrail at drep-discovery-design.md:230-234 are unchanged."
    **[D-6]** Correct anchors: opt-in sorts at the tail of **`:239`**, popularity guardrail at **`:241-245`** (live implementation `DRepDirectory.tsx:351-355`). This is a do-not-touch criterion; verify unchanged at close.

### task-174 — Dual CIP-129 / CIP-105 DRep ID display (build 3, json position 2; json `:1827-1849`)

*(`acceptanceCriteria` array at `:1841-1848`)*

1. "DRepIdDisplay gains an additive both-forms mode — existing single-form call sites keep today's rendering — deriving CIP-105 from the entry's CIP-129 id via normalizeDRepIdentity, omitting the second row cleanly when the id does not decode, never throwing and never falling back to a hand-rolled re-encode."
2. "DRepDetail (DRepDetail.tsx:103) renders both forms in full and monospaced with a copy button each per shared-design-tokens §4 :76 and the wireframe at drep-discovery-design.md:84-85; deduped search-result rows render both forms stacked per §11 :244; directory cards (DRepCard.tsx:121) keep the CIP-129-primary truncated form with one copy button."
   **[D-12]** Corrected anchors: `DRepIdDisplay` in the detail identity block is **`DRepDetail.tsx:113`** (block `:112-115`; `:103` is a `</div>` in the not-found branch); the card call site is **`DRepCard.tsx:126`** (`:121` is a button attribute close); §4's binding sentence is **`shared-design-tokens.md:78`** (`:76` is the heading); §11's deduped-row rule is **`:248`** (`:244` is blank).
3. "A unit test asserts the rendered CIP-129 and CIP-105 decode to the same credential bytes as the entry's `drepId`, each copy button copies exactly the form it labels, and the 'Select for delegation' handoff still passes entry.drepId so the signed payload `vote.id` and the on-device `vote.chosenOption` are unchanged."
4. "INHERITED sanitization floor: the task-111 spy suite is re-asserted over the dual-ID render and copy paths, and both clipboard warn payloads stay id-free — the unavailable branch keeps drepIdLength only and the failure branch keeps { error, drepIdLength } — with no bech32 string added for either form."
5. "New per-form copy and aria labels land in en-US and ja-JP with the leading !!! and `yarn i18n:manage` runs clean; DRepDirectory.spec.tsx truncation assertions are re-checked for the search-result rows that now carry a second `<code>` (card rows keep exactly one), and the directory and detail stories cover the dual-ID rendering in both locales."
   **[Storybook convention]** "both locales" is satisfied through the global English/Japanese toggle in `storybook/preview.tsx`. No local `IntlProvider`, no `*_ja` story exports.
6. "drep-discovery-design.md:240-241 is corrected so the card claim matches §4 :76 — cards are CIP-129-primary, both forms belong to the detail view and deduped search rows — leaving one dual-ID renderer outside the confirmation dialog."
   **[D-6] ALREADY SATISFIED.** The contradiction is gone: `:240` is blank, `:241` is "**Popularity-sort guardrail.**", and task-165's "Directory Identity: ID-Only in v1" section at **`:251-259`** now states the card contract explicitly. Verify and record; **do not re-edit**.

**[D-8] Description conflict.** task-174's description claims "search rows can
pass through the CIP-105 form buildDRepSearchIndex already derives per entry".
That is **false**: `searchDRepsByIdPrefix` discards the derived form at
`helpers.ts:129` (`.map(({ entry }) => entry)`) and returns
`AppDRepDirectoryEntry[]`. AC-1's own path — derive via `normalizeDRepIdentity`
— is the implementable one and wins.

### task-154 — Confirmation dialog verified name, byte-equality preserved (build 4, json position 3; json `:1850-1867`)

*(`acceptanceCriteria` array at `:1862-1866`)*

1. "Confirmation renders verified givenName plus DRep ID per shared-design-tokens §7 (identity equality rule)."
   **[D-9]** The value is `entry.verifiedName` reached via `governance.drepIndex`, not `AnchorEnrichEntry.givenName`. **[D-14]** This criterion also discharges task-157 AC-2's confirmation half.
2. "CIP-129, CIP-105, and signed payload `vote.id` remain byte-equal across the delegate -> verified-name transition."
3. "Hardware-wallet test asserts on-device DRep ID equals `vote.chosenOption`."
   Release-blocking per `shared-design-tokens.md:139`. Lives in `shelleyLedger.spec.ts` / `shelleyTrezor.spec.ts` against `parseVoteDelegation` (`shelleyLedger.ts:71`, `shelleyTrezor.ts:71`).

### task-155 — Apply source labeling to DRep Discovery content (build 5, json position 4; json `:1868-1885`)

*(`acceptanceCriteria` array at `:1879-1884`)*

1. "Consistent on-chain vs verified-off-chain distinction is applied across all DRep Discovery surfaces."
   **[D-3]** "All DRep Discovery surfaces" is scoped to the directory card, the detail view sections and the confirmation dialog. `CurrentVoteSummary` is explicitly out of scope.
2. "No anchor-derived content is shown without the verified off-chain content label."
3. "Apply the source label to verified display names in DRep detail and delegation confirmation."
   **[S-7]** Detail is already labelled by task-151 (`DRepDetailAnchorContent.tsx:73-87`) and by task-157 for the new fields; the confirmation label ships in task-154. task-155's work here is audit plus regression assertion, not a second edit.
4. "Local on-chain view remains complete and useful without anchor data."

### task-156 — `Abstain` / `No Confidence` directory treatment (build 6, json position 5; json `:1886-1902`)

*(`acceptanceCriteria` array at `:1897-1901`)*

1. "Directory never renders Abstain or No Confidence as DRep entries."
2. "Confirmation dialog still supports Abstain and No Confidence via the existing form path."
   Live proof today: the sentinel gate at `VotingPowerDelegationConfirmationDialog.tsx:117-120` and the identity-block-free sentinel branch at `:201-210`.
3. "Directory empty-state copy does not conflict with these form-only choices."
   Satisfied by inspection at HEAD: `DRepEmptyState.tsx:73-103` renders `governance.drepDirectory.empty.noResults` with no sentinel mention. Verify at slice close against the end state, since four earlier commits touch the directory.

## Planning Decisions (binding, as applied)

D-1 … D-16 are binding on the implementation guide's six section authors;
contradicting one is a guide defect, not an implementation choice.

### D-1 — Build order is `157 → 153 → 174 → 154 → 155 → 156`, deviating from the JSON listing order.

anchor-2 has **zero intra-slice dependency edges** — all twelve dependency
entries across the six rows are cross-slice and `complete` — so the JSON listing
order `153, 174, 154, 155, 156, 157` encodes nothing and is not a contract. The
canonical order is derived from three file-level couplings and one sequencing
rule: 157 and 153 serialize on `governance.types.ts:93-96` and
`AnchorVerificationService.ts:43-62`; 157, 155 and 174 serialize on the detail
tree; 154, 155 and 157-AC-2 all land in the confirmation dialog's identity block
(`VotingPowerDelegationConfirmationDialog.tsx:157-200`); and a labelling sweep
must be the last thing that runs over the surfaces it audits.
*Affected:* all six. *Rationale detail:* Canonical Build Order above.

### D-2 — Each shared seam is widened exactly once, by a named owner.

Four seams are touched by more than one task. Each has a single owner; every
other task consumes it as already present and must not re-declare it:

| seam | anchor | owner | consumers |
|---|---|---|---|
| `VerifiedDRepAnchorContent` | `governance.types.ts:93-96` | **task-157** (S-1) | task-153 (`doNotList`) |
| `parseVerifiedContent` | `AnchorVerificationService.ts:43-62` | **task-157** (S-2) | task-153 |
| `filterLogData` key list | `logging.ts:44-63` | **task-157** (S-3) | task-153, task-174 |
| `AnchorEnrichEntry.verified` | `GovernanceStore.ts:49-52` | **task-157** (S-1) | task-153, task-155 |
| CIP-105 display derivation | `normalizeDRepIdentity.ts:17-62` | **task-174** (S-4) | task-155 (audit only) |
| §7 confirmation name + label | `VotingPowerDelegationConfirmationDialog.tsx:157-200` | **task-154** (S-5, S-6) | task-157 AC-2, task-155 (assertion only) |
| verified-off-chain field labels | `DRepDetailAnchorContent.tsx` | **task-157** (S-7) | task-155 (audit only) |

**task-153's AC-5 remains literally true.** It adds the verified-metadata field
to `AppDRepDirectoryEntry` (`GovernanceStore.ts:23-36`) — the renderer-store
projection — which task-157 does not touch. The wire type, the parser and
`AnchorEnrichEntry` are the seams it inherits.
*Affected:* all six.

### D-3 — `CurrentVoteSummary` is out of scope for task-155.

`CurrentVoteSummary.tsx:90` already renders `<DRepSourceLabel source="on-chain" />`
and the component renders **no anchor-derived content at all** today, so AC-2 is
vacuously satisfied there. Its verified-name enrichment is cv-track work already
assigned by `current-vote-display-design.md:59`. Reading task-155's "all DRep
Discovery surfaces" to include it would import new scope beyond any acceptance
criterion, and the component's own comment at `:41-42` records that the
`DRepSourceLabelVariant` union cannot express its other states — a change that
would collide with S-6's "no new variant" rule.
*Affected:* task-155. Recorded so a later reviewer does not read the boundary as
a miss.

### D-4 — `DRepDetailOnchainSection` gains one section-level source label, not one per field, and it lands as a `Source` row in the `<dl>`.

The section imports no `DRepSourceLabel` (`:1-7`); provenance rides on the
heading string `governance.drepDetail.onchain.title` = "!!!On-chain" (`:11`).
`shared-design-tokens.md:45` requires an explicit provenance label, so the gap is
real. Exactly **one** section-level label closes it. A per-field sweep would
triple the label count on the detail view for zero informational gain and fight
the §2 "small pill" visual.

**Placement, superseding this entry's first draft ("beside the section
`<h2>`").** The label goes in a `Source` row appended to the end of the
section's `<dl>`, mirroring the shipped sibling pattern at
`DRepDetailAnchorSection.tsx:99-109`. Reason: the heading string and the pill
string are byte-identical in **both** locales — `governance.drepDetail.onchain.title`
and `governance.drepDirectory.source.onChain` are both `"!!!On-chain"` in en-US
and both `"!!!オンチェーン"` in ja-JP (measured at `55e8985bf`) — so a pill
adjacent to the `<h2>` would print the same words twice on one line. The
`Source` row keeps the count at one label while reading correctly.

**Cost:** one new key, `governance.drepDetail.onchain.source` = `"!!!Source"` /
`"!!!ソース"`, for the row's `<dt>`. It must be a **new** id, not a reuse of
`governance.drepDetail.anchor.source`, which is declared in a different module's
local `defineMessages` (`DRepDetailAnchorSection.tsx:25-29`) — two blocks
declaring one id is a duplicate-id error at extraction. This is why task-155's
row in the i18n Key Inventory is 1, not 0.
*Affected:* task-155.

### D-5 — `doNotList` is session-scoped and lazily known; that limitation is documented, not engineered around.

**What the corpus already resolves.** The lazy, per-detail-visit anchor fetch is
the stated v1 model and bulk prefetch is explicitly deferred:
`drep-discovery-design.md:247` is the no-prefetch rule, carried forward as an
explicit non-goal of anchor-1's task-151. The grounding brief also cites
`shared-design-tokens.md:250` for the same point, though it summarises that line
two different ways in two places (once as "verified-name search is deferred
beyond v1", once as "lazy per-detail fetch is the v1 model with bulk prefetch
deferred") — **the exact wording of `shared-design-tokens.md:250` is UNVERIFIED
here** and the guide author should quote it directly. Either way the corpus
resolution and the default agree, so the default applies unchanged.

**Consequence, stated plainly.** `doNotList` reaches
`AppDRepDirectoryEntry` only through the same fetch that populates
`verifiedName` (`GovernanceStore.fetchAnchorContent:408-451`, triggered from
`DRepDetailPage`). Therefore: a `doNotList: true` DRep **remains in the default
cohort until some user action in the current session has fetched its anchor**,
and an unvisited `doNotList: true` favorite shows no stale caption. The exclusion
is a best-effort courtesy to the DRep's stated preference, **not a security or
privacy control**, and nothing in the app depends on it being complete.

**Forbidden workarounds:** do not inject store state into the acceptance test to
simulate global knowledge; do not add bulk anchor fetching (it would breach the
CLI/anchor budget of invariant 6 and the no-prefetch rule); do not gate cohort
membership on an unresolved fetch (that would empty the cohort on a cold start).
*Affected:* task-153 (AC-1, AC-6, AC-7).

### D-6 — Five acceptance criteria are already satisfied on disk and are re-scoped from "make the edit" to "verify and record".

Task-165's inserted "Directory Identity: ID-Only in v1" section shifted
`drep-discovery-design.md` anchors; the required content is present at every
corrected anchor. Re-scoping prevents an implementer adding a duplicate
paragraph beside the existing one.

| AC | cited anchor | corrected anchor | disposition |
|---|---|---|---|
| task-153 AC-9 | `:228` | **`:239`** | present — verify + record |
| task-153 AC-10 | `:228` (same sentence) | **`:239`** | present — verify + record |
| task-153 AC-11 | `:109` | **`:110`** ("No illustration ships…resolved as dropped, not deferred") | resolved — verify + record; matches `DRepEmptyState.tsx:105-123` |
| task-153 AC-12 | `:230-234` | **`:239` (sorts) + `:241-245` (guardrail)** | do-not-touch — verify unchanged |
| task-157 AC-1 (image deferral half) | `:215` | **`:218`** | present — verify + record |
| task-174 AC-6 | `:240-241` | **superseded by `:251-259`** | contradiction already gone — verify + record; do not re-edit |

*Affected:* task-153, task-157, task-174.

### D-7 — task-153 AC-7's status-badge claim is unimplementable; the favorite keeps its real badge plus the inline caption, and the design doc is struck.

`drep-discovery-design.md:112` states a stale favorite shows "its current
`Retired` or `Excluded from default cohort` status badge (shared tokens §1)".
Neither value can exist: `DRepStatus` is the closed union `'active' | 'inactive'`
and `DRepStatusBadge.tsx:26-29` builds an exhaustive
`Record<DRepStatus, string>` over exactly those two — adding a member breaks the
record, and task-153 AC-5 forbids adding one. "Excluded from default cohort" is
implemented nowhere. **Live repo and AC-5 win.** Implementation: the favorited
`doNotList` entry renders its real `active`/`inactive` badge **plus**
`governance.drepFavorites.staleCaption` (`DRepCard.tsx:128-132`, message
`:53-57`), reached through the real `isStaleFavorite` predicate with no
`isStaleFavoriteEntry` prop injected — satisfiable today because
`DRepDirectory.tsx:90` declares the prop optional with no default and
`DRepDirectoryList.tsx:40,53` defaults it to the real helper.
The doc is reconciled by striking the impossible badge claim (see Doc
Reconciliations). *Affected:* task-153 (AC-6, AC-7).

### D-8 — task-174's search-row CIP-105 pass-through claim is false; the display derives its own second form.

The description asserts search rows can carry the CIP-105 form
`buildDRepSearchIndex` derives. `buildDRepSearchIndex` (`helpers.ts:70-84`) does
derive it, but `searchDRepsByIdPrefix` discards it at `:129`
(`.map(({ entry }) => entry)`) and returns `AppDRepDirectoryEntry[]`, the same
type the cohort path returns; `filterDReps` (`:189-220`) and `sortDReps`
(`:243-277`) consume and emit that same type. Passing the derived form through
would mean changing three signatures and `helpers.spec.ts:118-183`. **AC-1's own
path wins:** `DRepIdDisplay` derives CIP-105 internally via
`normalizeDRepIdentity`, and a search-result boolean selects the stacked
variant (D-12). This also keeps one display-derivation mechanism; the
`@cardano-sdk/core` `Cardano.DRepID.toCip105DRepID` call at `helpers.ts:77`
stays as the **search index's** derivation and is not unified — the duplication
is intentional and scoped. *Affected:* task-174.

### D-9 — One verified-name source per surface: detail reads `AnchorEnrichEntry.givenName`, confirmation reads `entry.verifiedName`.

`verifiedName` (`GovernanceStore.ts:35`) is written and hash-guarded
(`_applyVerifiedNames:521-538`, which drops the name when
`entry.anchor.hash !== state.hash`) and is already in the redaction list
(`logging.ts:63`) — but **read by no component**. The detail view reads
`state.givenName` off `AnchorEnrichEntry` (`DRepDetailAnchorContent.tsx:66,79`).
Leaving both live across a third surface is how the two drift.

**Binding split:** the detail view keeps `AnchorEnrichEntry.givenName` (it needs
the `loading` / `unavailable` states the entry union carries, and the `host` for
the source-label tooltip). The **confirmation dialog reads
`entry.verifiedName` via `governance.drepIndex`**, resolved in
`VotingGovernancePage.tsx:84-87` where `drepIndex` is already in scope (passed at
`:74`). Reasons: it is the hash-guarded projection; it is already
sanitization-listed; and the confirmation dialog must not depend on the detail
view's per-visit fetch state. The same projection pass carries `doNotList`
(D-11), so both surfaces read one hash-guarded output.
*Affected:* task-154, task-155, task-157, task-153.

### D-10 — Per-field CIP-119 length policy; a `paymentAddress` is never truncated.

`readCip119String` (`AnchorVerificationService.ts:29-41`) applies
`GIVEN_NAME_MAX_LENGTH = 80` (`:17`) to whatever it is handed. The helper is
generic; the constant is a `givenName` rule. Reusing it for `objectives`,
`motivations` and `qualifications` would silently clip long-form prose at 80
characters — CIP-119 caps none of them. task-157 therefore introduces explicit
per-field policy at the parse boundary in main, so the renderer never receives an
unbounded string and no clamp logic is duplicated in the renderer:

| field | bound | on exceeding |
|---|---|---|
| `givenName` | 80 (unchanged, CIP-119 rule) | clamp |
| `objectives`, `motivations`, `qualifications` | 1000 chars each — a rendering-safety bound, not a CIP-119 rule | clamp |
| `references` | at most 20 entries; each `uri` ≤ 2048 chars; each label ≤ 200 chars | drop the offending entry |
| `paymentAddress` | ≤ 128 chars | **reject (emit `null`) — never clamp** |

The `paymentAddress` asymmetry is the load-bearing part: the value is rendered
read-only **with a copy button** (AC-6), and a truncated address that a user can
copy is worse than an absent one. The transport's ~1 MB body cap already bounds
the aggregate; these bounds bound the individual render.
*Affected:* task-157.

### D-11 — `doNotList` rides the existing hash-guarded projection pass, which is renamed to match what it now does.

`_applyVerifiedNames` (`GovernanceStore.ts:521-538`) re-applies verified state
onto the list after every rebuild and drops the value when the entry's anchor
hash no longer matches the fetched state's hash — "so a re-registered anchor can
never keep showing the old name". `doNotList` needs exactly the same guard: a
re-registered anchor must not keep excluding a DRep from the cohort. task-153
extends that pass rather than adding a second one, and **renames it to
`_applyVerifiedMetadata`** — a function named `_applyVerifiedNames` that also
applies a cohort-exclusion flag is a name that lies. The rename is the smallest
truthful change; it is private, and `tsc --noEmit` finds every call site.
*Affected:* task-153.

### D-12 — The search-result variant is selected by a boolean threaded through the existing row path, not by a second component.

Search results and the cohort list render through the same
`DRepDirectoryList` / `DRepCard` path (`DRepDirectory.tsx:363-371`,
`DRepDirectoryList.tsx:88-100`, `<DRepCard>` at `:89`), and `visibleEntries` is a
plain `AppDRepDirectoryEntry[]` with **no signal distinguishing a search result
from a cohort row**. task-174 threads one boolean from `DRepDirectory`'s existing
`isSearchActive` (`:137-140`) → `DRepDirectoryList` → `DRepCard` → the new
opt-in `DRepIdDisplay` mode. The `DRepIdDisplay` prop is opt-in and defaults to
today's single-form render, so every existing call site — including the card's
own `:126` — is unchanged by construction (AC-1). Aria labels stay per-form and
distinct: the current `<code aria-label={drepId}>` (`:71-84`) carries the full
id, so two `<code>` elements need two distinct labels, not a shared one.
*Affected:* task-174.

### D-13 — The renderer-side https gate is lifted into a shared predicate, not copied.

task-157 AC-7 requires reference URIs to open only through the hardened path.
The pattern already exists: `DRepDetailAnchorSection.tsx:44-52`'s
`isHttpsAnchorUrl` gates the anchor URL into a link versus inert text, and the
click handler `:81-84` calls `onOpenExternalLink(anchor.url)` →
`AppStore.openExternalLink` (`AppStore.ts:80-83`) → `openExternalUrlChannel` →
`handleOpenExternalUrl` (`open-external-url.ts:24-34`). The renderer gate is
load-bearing, not belt-and-braces: main's rejection is fire-and-forget, so a
blocked URL surfaces as an unhandled promise rejection rather than a visible
error. task-157 **lifts the predicate into
`source/renderer/app/utils/governance/` beside `normalizeDRepIdentity.ts`** and
imports it in both places rather than duplicating a three-line security guard.
Non-https reference URIs render as inert text, matching the anchor URL treatment
— never as a dead link. *Affected:* task-157.

### D-14 — task-157 AC-2 is jointly discharged, and the hand-off is recorded in both tracker rows.

R-2's seam split gives the confirmation dialog to task-154; the canonical build
order puts task-154 **after** task-157. Consequence: task-157 cannot report AC-2
green from its own commit. task-157's `statusReason` records that AC-2's
confirmation half is discharged by task-154; task-154's AC-1 subsumes it and its
`statusReason` records the inbound hand-off. Neither row may be marked complete
on an AC the other owns without that cross-reference.
*Affected:* task-157, task-154.

### D-15 — task-156 documents the IA rationale in the design doc, never as in-app copy.

The task description asks to "Document the IA rationale in the directory". In-app
copy explaining why something is *absent* adds a user-facing sentence about a
non-entity, plus two new `!!!` strings, for zero user benefit — and would put
the literals `Abstain` / `No Confidence` on a directory surface the invariant
says they never appear on. The rationale therefore lands in
`drep-discovery-design.md` as a short paragraph adjacent to the directory
empty-state discussion (see Doc Reconciliations for the text; the insertion
anchor is fixed by the guide author against the live file). AC-3 is discharged by
verifying the shipped empty-state copy, not by editing it.
*Affected:* task-156.

### D-16 — Tracker edits are value-only; the tasks JSON is never reformatted.

task-153 AC-8 requires updating task-122's `status`/`statusReason` in
task-153's commit, and every row in this slice updates its own. Edit the values
in place and preserve surrounding formatting. **Never** run
`node_modules/.bin/prettier` over
`governance-drep-discovery-plan-tasks.json`, the i18n locale catalogs, or
`translations/messages.json` — they are tool-managed.
*Affected:* all six.

## Cross-Task Seam Contracts

### S-1 — `VerifiedDRepAnchorContent` and `AnchorEnrichEntry` are widened once, by task-157.

`governance.types.ts:93-96` today:

```ts
/** CIP-119 fields extracted from anchor bytes that passed Blake2b-256 verification. */
export interface VerifiedDRepAnchorContent { givenName: string | null; }
```

task-157 widens it to carry `givenName`, `objectives`, `motivations`,
`qualifications`, `references`, `paymentAddress` **and `doNotList`** — every
field nullable, `| null` rather than `?` (matching the existing convention at
`:55`/`:61`, since an optional property lets a construction site omit it
silently). `references` is a typed array carrying at minimum `uri`, an optional
label, and the raw `@type` discriminator preserved for the renderer's Link /
Identity / default-bucket split. `AnchorEnrichEntry`'s `verified` member
(`GovernanceStore.ts:49-52`) is widened in the same commit to carry the content
alongside `hash` and `host`. **task-153 adds nothing here.**

### S-2 — `parseVerifiedContent` is relaxed once, by task-157.

`AnchorVerificationService.ts:57-61` today returns `null` when `givenName` is
missing, which routes to `ParseFailed` → `unavailable`. After task-157 the
function returns a `VerifiedDRepAnchorContent` whenever the body parses as a JSON
object, with every field independently nullable; per-field bounds follow D-10.
The "no name, no block" decision moves to the renderer, which already holds that
guard at `DRepDetailAnchorContent.tsx:66`. **This is a behavioural change to an
anchor-1 file and must be called out explicitly in the guide, with the
`AnchorVerificationService.spec.ts` cases that pin both the old and new
behaviour.** The digest gate is untouched: unverified bytes still never reach
`JSON.parse` (`:85-92`, comment `:64-65`).

### S-3 — `filterLogData`'s key list is extended once, by task-157.

`source/common/utils/logging.ts:44-63` already redacts `drepId`,
`drepIdentity`, `votingTarget`, `chosenOption`, `credentialHex`, `anchorUrl`,
`anchorContent`, `givenName`, `verifiedName`. It does **not** contain
`objectives`, `motivations`, `qualifications`, `references`, `paymentAddress` or
`doNotList`. task-157 adds all six in one edit, with matching `filterLogData`
cases in the `:78` block of `tests/jest/security/governance-sanitization.spec.ts`
and a call-boundary case in the `:310` block for any new path that can reach a
sink. `paymentAddress` is a bech32 address — treat it as sensitive-shaped even
though it is public data.

### S-4 — CIP-105 for display is always `normalizeDRepIdentity`; task-174 owns the both-forms mode.

`normalizeDRepIdentity` (`normalizeDRepIdentity.ts:17-62`) returns
`{ raw, cip129, cip105, credentialHex, credentialType }` and, per its own
docblock `:11-15`, "Unknown HRP, length mismatch, bad checksum, or bad header
returns null; never throws, never logs." No surface hand-rolls a re-encode.
`DRepIdDisplay` derives the second form internally behind a new opt-in prop
(D-12); the omission path when the id does not decode is the `null` return, not
a thrown error. `helpers.ts:77`'s `Cardano.DRepID.toCip105DRepID` remains the
**search index's** derivation and is out of scope (D-8).

### S-5 — The confirmation dialog's verified name comes from `entry.verifiedName` via `governance.drepIndex`; task-154 owns the render.

Resolved in `VotingGovernancePage.tsx:84-87` alongside the existing
`drepIdentity` decode, passed to
`VotingPowerDelegationConfirmationDialog.tsx` as a new prop beside `:55-71`, and
rendered inside the §7 block `:157-200` — **only** as the `{verified givenName}`
line plus the extended source label, per `shared-design-tokens.md:135`. The
sentinel branch (`:201-210`) never receives a name. task-157's guide covers the
detail view only and must state that boundary; task-155's guide asserts the label
and does not re-render the line.

### S-6 — `DRepSourceLabel` gains no new variant; the §7 composite is two instances.

The variant union (`DRepSourceLabel.tsx:52-57`) has no composite member and the
tooltip map (`:74-80`) is keyed per-variant, so a composite variant would need a
composite tooltip that misdescribes both halves. The §7 label
"On-chain · Name: Verified off-chain content" is built by keeping the existing
`<DRepSourceLabel source="on-chain" />` on the ID block
(`VotingPowerDelegationConfirmationDialog.tsx:195-199`) and adding a second
`source="verified-off-chain"` adjacent to the name line, joined by a literal
separator in the JSX. Each tooltip stays correct and task-175's block is
otherwise unchanged.

### S-7 — Label ownership: task-157 labels what it renders; task-155 sweeps what remains.

task-157 attaches `<DRepSourceLabel source="verified-off-chain" host={…} />` to
every field it adds, following the shipped `givenName` pattern at
`DRepDetailAnchorContent.tsx:73-87`. task-155 therefore never re-labels a
task-157 field. task-155's edit surface is exactly one: the section-level
on-chain label on `DRepDetailOnchainSection`, rendered as a `Source` row in that
section's `<dl>` with one new key `governance.drepDetail.onchain.source` (D-4).
Everything else it does is audit and regression assertion.

## Doc Reconciliations anchor-2 Owns

Two real edits. Everything else in this table is verify-and-record (D-6) — an
implementer who adds a paragraph beside existing text has introduced a
duplicate, which is a defect.

### Edit 1 (task-153, D-7) — strike the impossible badge claim at `drep-discovery-design.md:112`

**Current text** (as distilled and verified in the grounding brief; the guide
author re-reads the live line before editing):

> **Stale favorites.** If a favorited DRep becomes Retired or appears with `doNotList=true` after `anchor-2` lands, it remains in the favorites list with its current `Retired` or `Excluded from default cohort` status badge (shared tokens §1) and an inline caption: `governance.drepFavorites.staleCaption` → *"This DRep is no longer in the default cohort."* No automatic removal. The user unfavorites explicitly.

**Replacement text:**

> **Stale favorites.** If a favorited DRep appears with `doNotList=true` after `anchor-2` lands, it remains in the favorites list with its current status badge and an inline caption: `governance.drepFavorites.staleCaption` → *"This DRep is no longer in the default cohort."* `DRepStatus` is the closed union `active | inactive`, so no `Retired` or `Excluded from default cohort` badge exists to show; the caption alone carries the signal. `Retired` stays deferred until a distinct unregistration signal exists. No automatic removal. The user unfavorites explicitly.

*Why:* `DRepStatusBadge.tsx:26-29` is an exhaustive `Record<DRepStatus, string>`
over `active | inactive`, and task-153 AC-5 plus invariant 14 forbid adding a
member. The claim is unimplementable in this release.

### Edit 2 (task-156, D-15) — record the Abstain / No Confidence IA rationale in `drep-discovery-design.md`

**New paragraph** (insertion anchor adjacent to the directory empty-state
discussion, fixed by the guide author against the live file; content is binding):

> **`Abstain` and `No Confidence` are form-only.** Both are delegation-form sentinels, not DReps: they have no registration, no anchor, no voting power and no detail view, so they never appear as directory entries, search results, favorites or cohort members. They are chosen in the delegation form and carried through the existing path — `VotingPowerDelegation` sets `chosenOption` to the sentinel string, and the confirmation dialog renders a vote label with no identity block. Because they are not entries, the directory's empty state must never suggest the directory is the place to find them; its copy stays scoped to registered DReps and filters.

### Verify-and-record (no edit)

| owner | anchor | what to confirm | where recorded |
|---|---|---|---|
| task-153 AC-9/AC-10 | `drep-discovery-design.md:239` | the Show-all definition and the `doNotList` reachability sentence are both present and match `GovernanceStore.showAllList:279-282` | task-153 `statusReason` |
| task-153 AC-11 | `:110` | "No illustration ships — the earlier 'prominent illustration' claim is resolved as dropped, not deferred", matching `DRepEmptyState.tsx:105-123` | task-153 `statusReason` |
| task-153 AC-12 | `:239` (sorts) and `:241-245` (popularity guardrail) | unchanged after the slice; live behaviour still `DRepDirectory.tsx:351-355` | task-153 `statusReason` |
| task-157 AC-1 | `:218` | the `image` / `imageObject` deferral paragraph is present | task-157 `statusReason` |
| task-174 AC-6 | `:251-259` | task-165's "Directory Identity: ID-Only in v1" section supersedes the old card dual-ID claim; `:240` is blank and `:241` is the popularity guardrail | task-174 `statusReason` |

## User Stories

- *As someone choosing a DRep,* I can read the DRep's stated objectives,
  motivations and qualifications in the detail view, each marked as verified
  off-chain content fetched from a named host — so I can tell a claim apart from
  a chain fact.
- *As someone checking a DRep's identity claim,* an `Identity` reference is shown
  in its own sub-section under copy that says Daedalus has not verified it and
  tells me to visit the URL and check the DRep ID is published there — not as a
  plain link that implies endorsement.
- *As someone worried about scams,* a stated payment address is shown read-only
  with copy, under copy telling me delegating voting power requires no payment to
  any address — and it can never pre-fill a send form.
- *As a DRep who published `doNotList: true`,* I am not promoted in the
  Recommended default view, while anyone who has my ID can still find and
  delegate to me.
- *As someone who favorited a DRep who later opted out,* my favorite is not
  silently deleted: it stays with its status badge and a caption telling me it is
  no longer in the default cohort.
- *As someone verifying a DRep ID against an external source,* the detail view
  shows both the CIP-129 and CIP-105 encodings in full, monospaced, each with its
  own copy button.
- *As someone signing a delegation,* the confirmation shows a verified name when
  one exists, and the ID it shows is byte-identical to what the ledger signs and
  what my hardware wallet displays.

## Non-Functional Requirements

- **Zero new npm dependencies.** `blake2b`, `blakejs`, `bignumber.js` and
  `bech32` are already production deps; `https`/`dns` are Node builtins.
- **Zero new IPC channels.** anchor-2 rides `GOVERNANCE_DREP_ANCHOR_CHANNEL`
  (`api.ts:670-672`) with a widened response payload only. The main handler
  (`governanceAnchorChannel.ts:18-35`) still never rejects and still logs only
  `{ status, reason }` (`:29-32`).
- **Zero new CLI invocations.** The bulk `--all-dreps` refresh is unchanged; no
  per-DRep CLI call is added (invariant 6).
- **Bounded render cost.** Per-field clamps (D-10) keep any single anchor from
  pushing an unbounded string into an observable or the DOM; the transport's
  ~1 MB body cap is unchanged and is not raised.
- **No regression in the chain-native path.** Every new render is behind a
  per-field null guard (task-157 AC-3, AC-8); the detail view must be complete
  and useful with `anchorState` `null`, `loading` or `unavailable`.
- **Locale parity.** Every new key lands in both `en-US.json` and `ja-JP.json`
  with the leading `!!!`; `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:55-62`
  enforces it for `governance.*` and `:18-21`'s explicit list for
  `voting.governance.confirmationDialog.*`.
- **ja-JP layout risk is real and inherited.** anchor-1 F-16's overflow pass is
  still owed; task-174 adds a second `<code>` line to search rows and task-157
  adds five fields including long-form prose. No browser exists in this
  environment, so the visual pass is OWED, not green.
- **Lint gate is errors-only.** `yarn lint` must exit 0 with 0 errors; the
  ~5591 pre-existing warnings at HEAD are not the gate and will move upward as
  new files land under `source/` or `storybook/`.

## Architecture: Data Flow (anchor-2 delta)

```
on-chain anchor (url, hash)                                  [unchanged]
  → main: fetchAnchorBytes  — TLS on, no redirects, ≤10s, ~1 MB, JSON allow-list,
                              SSRF + DNS-rebinding guard                [unchanged]
  → main: Blake2b-256 over the bounded bytes; mismatch → HashMismatch   [unchanged]
  → main: immutable hash-keyed cache write                             [unchanged]
  → main: parseVerifiedContent                                    ◄── WIDENED (S-2)
          givenName | objectives | motivations | qualifications |
          references[] | paymentAddress | doNotList     — all nullable, per-field bounds (D-10)
  → IPC  GOVERNANCE_DREP_ANCHOR_CHANNEL (same channel, widened payload)  ◄── S-1
  → renderer: GovernanceStore.anchorStateByDRepId : AnchorEnrichEntry    ◄── WIDENED (S-1)
       ├── DIRECT READ  → DRepDetailAnchorContent  — the five new fields, each
       │                    with a verified-off-chain label                (task-157, S-7)
       └── PROJECTION   → _applyVerifiedMetadata (hash-guarded, D-11)
                            ├── verifiedName → drepIndex → confirmation dialog (task-154, S-5)
                            └── doNotList    → AppDRepDirectoryEntry            (task-153)
                                                 ├── defaultCohort  : EXCLUDED
                                                 ├── showAllList    : untouched
                                                 ├── search index   : untouched
                                                 ├── drepIndex      : untouched
                                                 └── isStaleFavorite: recognised
```

Identity display is a parallel, anchor-independent path: `entry.drepId` (CIP-129)
→ `normalizeDRepIdentity` → `{ cip129, cip105 }` → `DRepIdDisplay` both-forms
mode (task-174). Nothing on this path touches the value handed to
`delegateVotes` — `chosenOption` stays `entry.drepId` byte-for-byte from
`VotingPowerDelegation.tsx:242-245` through `VotingStore.ts:424-425`.

## What anchor-2 Deliberately Does NOT Include

- **No bulk anchor prefetch.** `doNotList` and `verifiedName` stay session-scoped
  and lazily known (D-5). Bulk fetch would breach invariant 6's anchor budget and
  the no-prefetch rule at `drep-discovery-design.md:247`.
- **No `image` / `imageObject` render.** Deferred, recorded at `:218`; the
  detail view keeps the default avatar.
- **No verified-name search, sort or filter.** Deferred beyond v1; the search
  index stays ID-only.
- **No name or verified content on `DRepCard`.** `:216` and `:251-259` both
  forbid it; the source-label sweep must not become an excuse to add one.
- **No new `DRepStatus` member**, no `Retired` badge, no
  "Excluded from default cohort" badge (D-7, invariant 14).
- **No `CurrentVoteSummary` change** (D-3).
- **No new `DRepSourceLabel` variant** (S-6).
- **No second delegation backend.** `VotingStore` is touched only by spec
  assertions and still never reads `GovernanceStore` (invariant 4).
- **No `!!!` removal.** That is a release-end manual review (invariant 11).
- **No e2e tests** — out of scope for v1 per the feature README.
- **No live-network anchor fetch and no real SIPO vector bytes.** anchor-2
  inherits anchor-1 F-10 and F-13 unresolved; the widened parser is proven
  against fixtures only.
- **No search-pipeline type widening** (D-8).

## i18n Key Inventory

Namespaces follow the shipped conventions: `governance.drepDetail.*` for detail
chrome, `governance.drepDirectory.*` for directory chrome, and
**`voting.governance.confirmationDialog.*`** for the dialog. Names below are
indicative; the namespace placement and the `!!!` marker are binding.

Counts below are the per-task deltas the implementation guide specifies. Rows
are listed in **build order** (D-1), so the running totals are the numbers each
task's Verify block must print.

| # | task | keys | delta | `governance.*` after | catalog total after |
|---|---|---|---|---|---|
| 1 | task-157 | `governance.drepDetail.anchorContent.{objectives, motivations, qualifications}`; `…anchorContent.references.{title, links, identity, identityCaption, other}`; `…anchorContent.paymentAddress.{label, caption, copyButton, copyLabel, copiedToast}` | +13 | 110 | 1644 |
| 2 | task-153 | none — reuses `governance.drepFavorites.staleCaption` (`DRepCard.tsx:53-57`) | 0 | 110 | 1644 |
| 3 | task-174 | per-form copy-button labels and per-form aria labels for the CIP-129 and CIP-105 rows — new sibling keys, **not** edits to the shipped `governance.drepDirectory.copyButton` / `.copyId` / `governance.drepDetail.copyIdToast` (`DRepIdDisplay.tsx:10-26`) | +5 | 115 | 1649 |
| 4 | task-154 | `voting.governance.confirmationDialog.{verifiedName, verifiedNameSource}` — outside the `governance.*` namespace, so both **must also be added to `PRELIMINARY_CONFIRMATION_KEYS` (`preliminaryCopyMarkers.spec.ts:18-21`)**, otherwise their markers are unenforced | +2 | 115 | 1651 |
| 5 | task-155 | `governance.drepDetail.onchain.source` — the `<dt>` of the on-chain section's `Source` row (D-4) | +1 | 116 | 1652 |
| 6 | task-156 | none — the IA rationale is documentation, not in-app copy (D-15) | 0 | 116 | 1652 |

Baseline at `55e8985bf`, measured: **1631 keys in en-US, 1631 in ja-JP, 97
`governance.*` in each**, zero missing on either side, 97/97 `!!!`-marked. The
**delta is the contract**; the "after" columns are the cumulative expectation
only when every earlier row is on the branch, and each task re-measures rather
than assuming. Every task that adds copy runs `yarn i18n:manage` and then
surgically reverts any file it did not intend to change with `git restore` —
never `git stash`.

## Docs / Designs / Research / Workflows / Skills Consulted

Sources marked **(via brief)** were read through the verified, line-anchored
grounding brief produced for this slice rather than opened directly; the brief
re-verified every anchor against the worktree at `55e8985bf` and its corrections
are carried into **Corpus-vs-Repo Corrections** below.

- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  — the `anchor-2` phase object `:1792-1927` read directly: the six task rows,
  their descriptions and their `acceptanceCriteria` arrays, quoted verbatim
  above.
- `.agent/plans/governance/drep-discovery/prompt.md:144-149` — the locked slice
  order, read directly. Locked invariants, the non-autonomous set, the status
  rule, the convergence rule and the Definition of Done **(via brief)**.
- `.agent/plans/governance/drep-discovery/README.md` **(via brief)** — status
  vocabulary; one commit per task; the `!!!` rule; no e2e in v1; the CIP-119 test
  vectors.
- `governance-drep-discovery-plan.md` **(via brief)** — `:276`, `:293` slice
  scope; `:335` "hash verification proves only that the registrant authored the
  blob — which an impersonator satisfies exactly".
- `designs/shared-design-tokens.md` **(via brief)** — §1 status badges `:9-20`;
  §1a category badges `:24-39`; §2 source labels `:43-57` (the charter sentence
  `:45`, the verified row `:51`); §4 identity `:76`/`:78`; §7 confirmation
  identity `:108-139` (after-anchor-2 template `:124-131`, the security rule
  `:133`, block ownership `:135`, byte-equality `:137`, the release-blocking HW
  assertion `:139`); §11 `:241`/`:248`/`:250`.
- `designs/drep-discovery-design.md` **(via brief)** — detail wireframe `:84-85`;
  favorites empty state `:110`; stale favorites `:112`; the verified render list
  `:216`; the image deferral `:218`; the `references` `@type` rule `:220-224`;
  `paymentAddress` `:226`; show-all + `doNotList` reachability `:239`; the
  popularity guardrail `:241-245`; the no-prefetch rule `:247`; "Directory
  Identity: ID-Only in v1" `:251-259`.
- `designs/current-vote-display-design.md` **(via brief)** — `:59` and `:101`,
  which assign `CurrentVoteSummary`'s verified-name enrichment to the cv track
  (D-3) and whose field names are stale against the live store (C-6 below).
- `research/anchor-1-findings.md` **(via brief)** — F-5 (the fire-and-forget
  rejection that makes the renderer https gate load-bearing), F-10 (mocked
  transport only), F-13 (real SIPO vector bytes never fetched), F-16 (ja-JP
  overflow pass owed), F-17 (Primary badge vs "With metadata" filter divergence),
  F-9/F-15/F-20 (the `nix fmt` substitute churn decision).
- `research/cv-2-findings.md` **(via brief)** — F-31, the two-anchor
  sanitization re-proof rule.
- `research/slice-7-findings.md` **(via brief)** — F-1, the precedent for
  treating a task's declared `targetPath` as indicative when the live repo
  disagrees (C-1 below).
- `task-plans/anchor-1-PRD.md` — section skeleton, depth, decision-entry shape
  and the OWED convention, read directly at `:1-200`, `:292-366`, `:1210-1389`.
- Live code — every `path:line` in this PRD traces to the brief's re-verified
  seam inventory at `55e8985bf`.
- **Skills flagged for implementation, not invoked at planning:**
  `storybook-creation` (task-174 AC-5, task-157 detail stories),
  `i18n-messaging` (task-157, task-174, task-154), `evidence-rules` (binding on
  every doc in this slice), `git-commit-formatter` (one subject-only commit per
  task), `bech32-encoding-decoding` (task-174 AC-3 reference vectors).
  `e2e-test-creation` is **not applicable** (no e2e in v1). `cardano-cli-*`
  skills are not applicable — anchor-2 issues no CLI query.

## Locked Invariants Touched (inlined)

| # | invariant | tasks that touch it | how anchor-2 holds it |
|---|---|---|---|
| **1** | **Local-first.** Discovery data comes only from the local node via the main-process `GovernanceQueryService`. No hosted explorers, indexers, GovTool, Koios, Blockfrost or public governance APIs. | 157, 153 | The only outbound call remains the DRep's own registered anchor URL through the anchor-1 pipeline. anchor-2 adds no host, no aggregator, no second transport. |
| **2** | **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store payload — re-asserted via the task-111 spy suite in every slice. The task-168 DRep-state snapshot is the one documented exception. | **all six**, named explicitly in task-174 AC-4 | **Stressed hardest by this slice.** task-157 adds six new named payload fields, none currently in the redaction list (`logging.ts:44-63`) — S-3 lands them with matching floor cases, and `paymentAddress` is treated as sensitive-shaped. task-174 adds a second bech32 form to the render path while both clipboard warn payloads stay id-free (`DRepIdDisplay.tsx:52-54`, `:62-65`). Every task re-proves the floor by citing **both** `tests/jest/security/governance-sanitization.spec.ts` and the sibling logging suite together — citing one is a false green. |
| **3** | **Anchor transport-security floor**, never thinned: TLS on, redirects off, ≤10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF + DNS-rebinding mitigation, Blake2b-256 hash-verify before parse/cache/render, immutable hash-keyed cache. No anchor-derived content renders without verification AND a verified off-chain source label. Anchor URLs open only through the task-152 HTTPS-only path. | 157 (AC-1, AC-6, AC-7), 155 (AC-2), 153 | **The slice's charter.** S-2 relaxes only the *parse* requirement on `givenName`; the digest gate at `AnchorVerificationService.ts:85-92` is untouched and unverified bytes still never reach `JSON.parse`. Reference URIs route through `AppStore.openExternalLink` → `handleOpenExternalUrl` behind the shared https predicate (D-13); non-https URIs render inert. Every new field carries a verified-off-chain label at render (S-7), audited by task-155. |
| **4** | **No second delegation backend.** Selection supplies a DRep ID to the existing `delegateVotes` / `VotingStore` signing paths via React Router `location.state` only. `VotingStore` never reads `GovernanceStore` directly. | 154, 174 (AC-3), 157 (AC-2) | The verified name is resolved in `VotingGovernancePage.tsx:84-87` — a container that already holds `governance.drepIndex` — and passed down as a prop. `VotingStore.ts:424-425` is unchanged and imports nothing from `GovernanceStore`. |
| **5** | **Lovelace losslessness.** json-bigint lossless parse → decimal-string IPC → renderer `BigNumber` rehydration. Never route raw `JSONbig` objects across IPC or into observables. | 153 | The `doNotList` exclusion is inserted into `defaultCohort`'s eligibility filter (`GovernanceStore.ts:216-221`) and must not touch the `BigNumber` ranking comparator above it. |
| **6** | **CLI discipline.** Bulk `--all-dreps` once per refresh; per-DRep CLI invocations are forbidden. | 153 | The reason D-5 accepts session-scoped `doNotList` instead of prefetching. |
| **7** | **Default cohort is binding.** Exclude top 35 by voting power; up to the next 200 eligible, randomized. The default cohort IS the "Recommended" sort — no Recommended tab, no per-card Recommended badge. | **153** | `doNotList` is an **addition** to the eligibility predicate, never a replacement: `COHORT_TOP_EXCLUSION = 35`, `COHORT_MAX_SIZE = 200` and `COHORT_MIN_REMAINING_EPOCHS = 6` (`:81-83`) are untouched and never re-derived. |
| **8** | **Badges are informational only** — they never reorder, filter or override the cohort. | 153, 155 | Reinforced in live code at `helpers.ts:177-182` ("filter code must never import from the badge module"). The cohort filter reads `doNotList` off the entry; it never reads a badge or a category. |
| **9** | **No auto-delegation.** | 154, 157 | A verified name is display only; nothing in this slice initiates or pre-selects a delegation. `paymentAddress` never pre-populates a form field (task-157 AC-6). |
| **10** | **Byte-equality.** CIP-129, CIP-105 and the signed payload `vote.id` stay byte-equal through every identity-display change; the on-device DRep ID equals `vote.chosenOption`. | **154 (AC-2, AC-3), 174 (AC-3)** | **The release-blocking constraint of the slice.** `chosenOption` (`VotingPowerDelegation.tsx:242-245`) and the signed-payload string (`ConfirmationDialog:190`) are untouched; the reservation comment at `:163-165` states the rule and is deleted only when the name lands beside it. The HW assertion lives in `shelleyLedger.spec.ts` / `shelleyTrezor.spec.ts` against `parseVoteDelegation` (`shelleyLedger.ts:71`, `shelleyTrezor.ts:71`) and is release-blocking per `shared-design-tokens.md:139`. |
| **11** | **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!`. Removing `!!!` is a release-end manual review, never a per-slice task. | 157 (AC-8), 174 (AC-5), 154, 155, 156 | Binds all ~14 new strings. task-154's new `voting.governance.confirmationDialog.*` key must be added to `PRELIMINARY_CONFIRMATION_KEYS` (`preliminaryCopyMarkers.spec.ts:18-21`) or its marker is unenforced by the `governance.*`-prefixed rule at `:55-62`. **No task in this slice removes a `!!!`.** |
| **12** | **Favorites are per-device** via Electron local store — not per-wallet, not synced. | 153 (AC-6, AC-7) | `toggleFavorite` / `loadFavorites` (`GovernanceStore.ts:458-490`) are untouched; `doNotList` changes the caption, never the persisted set. A `doNotList` transition never purges a favorite. |
| **13** | **`Abstain` / `No Confidence` are form-only sentinels**, never DRep directory entries. | **156**, 154 | Live proof: the sentinel gate at `ConfirmationDialog:117-120` and the identity-block-free branch at `:201-210`. task-154 must not add a name to that branch. |
| **14** | **DRep status grounding.** Canonical on-chain status is `active \| inactive`; `expiring` is renderer-derived display state; `retired` is deferred. `DRepStatus` gains no new member. | **153 (AC-5, AC-7)** | The direct cause of D-7: `drep-discovery-design.md:112`'s `Retired` / `Excluded from default cohort` badge cannot be built, and the doc is struck rather than the union widened. |

## Dependencies

- **In-slice chain:** none. anchor-2 has zero intra-slice dependency edges; the
  canonical order is technical (D-1), not dependency-derived.
- **Cross-slice prerequisites, all twelve `complete`:** task-107 (bare directory
  list), task-115 (HW delegate path), task-116 (detail tree), task-118 (default
  cohort + seed), task-121 (search + filters), task-122 (favorites), task-129
  (`normalizeDRepIdentity`), task-151 (verified `givenName` render), task-152
  (HTTPS-only `openExternal`), task-172 (cohort-grounded category badge). Also
  landed and load-bearing though not listed as formal dependencies: task-175
  (the pre-anchor §7 identity block task-154 extends) and task-165 (the ID-only
  directory documentation that makes task-174 AC-6 already-satisfied).
- **What anchor-2 hands forward:** `slice-8` closes the feature. The residuals
  this slice cannot discharge — the live anchor fetch, the real SIPO vector
  bytes, the ja-JP visual pass, and the release-end `!!!` review — carry into it.
- **Runtime and tooling:** node v24.16.0, jest 27.5.1, prettier 2.1.2,
  TypeScript 4.9.5, Electron 41.3.0, React 16.14.0, MobX 5.15.7.
- **Environment.** `nix` is **absent**, so `nix fmt` cannot run and stays a
  pre-merge obligation the user owns; the substitute is
  `node_modules/.bin/prettier --write <explicit changed paths>` — never
  `yarn prettier`, whose package.json script embeds a repo-wide `"**/*.*"` glob
  and reformats ~250 unrelated files even when handed a path. Discard unwanted
  working-tree changes with `git restore` / `git checkout -- <paths>`, never
  `git stash` (the stash stack is shared across worktrees). `gh` and push
  credentials are absent, so work stays local. There is **no browser**, so the
  Storybook visual and ja-JP overflow passes cannot execute here;
  `yarn storybook:build` is red at HEAD for a pre-existing manager-webpack reason
  unrelated to any change, which makes `yarn check:all` red too — the usable
  floor is `yarn storybook`. There is **no network**, so no live anchor fetch
  happens in this container.

## Corpus-vs-Repo Corrections anchor-2 Inherits

Recorded here so no guide author re-derives them. **Live repo wins.**

| # | corpus claim | live repo at `55e8985bf` | disposition |
|---|---|---|---|
| C-1 | task-153's `targetPath` is `source/renderer/app/stores/` and task-157's is `source/renderer/app/components/` | Both require edits in `source/common/types/governance.types.ts:93-96` and `source/main/governance/AnchorVerificationService.ts:43-62` — the six fields are neither parsed nor on the wire today. Both are full-stack changes | `targetPath` is indicative, not binding (precedent: `research/slice-7-findings.md` F-1). The true file set is the Per-Task Contract above |
| C-2 | `DRepIdDisplay` is called from `DRepDetail.tsx:103` (task-174 AC-2) | `:103` is a `</div>` inside the not-found branch; the identity block is `:112-115` with `DRepIdDisplay` at `:113` | Cite `DRepDetail.tsx:113` |
| C-3 | The card identity line is `DRepCard.tsx:121` (task-174 AC-2) | `:121` is the `>` closing the favorite-toggle button's attribute list; `<DRepIdDisplay drepId={entry.drepId} />` is at `:126` | Cite `DRepCard.tsx:126` |
| C-4 | The shared row render is `DRepDirectoryList.tsx:84` (task-174 description) | `:84` is blank; the map is `:88-100` with `<DRepCard>` at `:89` | Cite `DRepDirectoryList.tsx:88-100` |
| C-5 | `buildDRepSearchIndex` is at `helpers.ts:71-83` (task-174 description) | Signature `:70`, body through `:84`, doc comment `:65-69` | Cite `helpers.ts:70-84` |
| C-6 | Search rows can pass through the CIP-105 form `buildDRepSearchIndex` derives (task-174 description) | `searchDRepsByIdPrefix` discards it at `:129` (`.map(({ entry }) => entry)`); `filterDReps` `:189-220` and `sortDReps` `:243-277` both use `AppDRepDirectoryEntry[]` | **False as written.** D-8 resolves it via `normalizeDRepIdentity` at render time |
| C-7 | §11's deduped-row rule is at `shared-design-tokens.md:244`; §4's rule at `:76` | `:244` is blank (rule at `:248`); `:76` is the §4 heading (rule at `:78`) | Cite `:248` and `:78` |
| C-8 | A stale favorite shows a `Retired` / `Excluded from default cohort` status badge (`drep-discovery-design.md:112`, shared tokens `:14-15`) | `DRepStatusBadge.tsx:26-29` is an exhaustive `Record<DRepStatus, string>` over `active`/`inactive`; "Excluded from default cohort" is implemented nowhere | **Live repo + AC-5 win** (D-7). The doc line is struck; the caption alone carries the signal |
| C-9 | Design-doc anchors `:228` (show-all), `:230-234` (sorts/guardrail), `:109` (favorites illustration), `:215` (render list), `:240-241` (card dual-ID) | All shifted by task-165's inserted section. Correct: `:239`, `:239` + `:241-245`, `:110`, `:216`, `:251-259` — **and the required content is already present at every one** | Re-target and downgrade to verify-and-record (D-6) |
| C-10 | The 80-char clamp is a generic CIP-119 string rule | `GIVEN_NAME_MAX_LENGTH = 80` (`AnchorVerificationService.ts:17`) is applied inside the generic `readCip119String:29-41`; CIP-119 caps only `givenName` | Per-field policy (D-10). Reusing the helper unchanged would silently clip long-form prose at 80 characters |
| C-11 | `verifiedName` is the app's verified-name channel | Written at `GovernanceStore.ts:521-538`, in the redaction list at `logging.ts:63` — but **read by no component**; the detail view reads `AnchorEnrichEntry.givenName` (`DRepDetailAnchorContent.tsx:66,79`) | One source per surface (D-9): detail keeps `givenName`; confirmation reads `verifiedName` via `drepIndex` |
| C-12 | `CurrentVoteSummary` reads `givenName` from `GovernanceStore.drepIndex[drepId]?.givenName`, entry type `DRepIndexEntry`, with an `anchorUrl` field (`current-vote-display-design.md:59`, `:101`) | The field is `verifiedName` (`GovernanceStore.ts:35`), the type is `AppDRepDirectoryEntry`, there is no `anchorUrl` (it is `anchor: { url, hash }` at `:33`), and `CurrentVoteSummary.tsx` renders no name at all — only `source="on-chain"` at `:90` | Live repo wins on all three names. The component stays out of scope (D-3) |
| C-13 | anchor-1 PRD cites `AppDRepDirectoryEntry` at `GovernanceStore.ts:20-31` | The interface is `:23-36` at `55e8985bf` — it grew by `verifiedName` | Record-only; cite `:23-36` |
| C-14 | The grounding brief cites `shared-design-tokens.md:250` twice with two different summaries — "verified-name search is deferred beyond v1" and "lazy per-detail fetch is the v1 model with bulk prefetch deferred" | Not re-read during this planning pass | **UNVERIFIED.** The guide author quotes `:250` directly before relying on either reading. D-5 rests on `drep-discovery-design.md:247` (the no-prefetch rule), which is independently attested |
| C-15 | `VotingPowerDelegationConfirmationDialog.tsx:163-165` is a normal source comment | It names a slice (`anchor-2`) — the one exception to the comment convention in the governance tree | task-154 **deletes** it when it fills the slot; it must not survive as change history |

## Risks and Open Questions

- **R-1 (high) — hash verification proves authorship, not identity.**
  `governance-drep-discovery-plan.md:335`: "Hash verification proves only that the
  registrant authored the blob — which an impersonator satisfies exactly — so it
  is not on its own a mitigation for a claimed identity." anchor-2 multiplies the
  surface: five more claimed fields, plus `references[@type=Identity]` which is
  literally a claim of external identity. *Mitigation:* the design's own rule at
  `drep-discovery-design.md:223` — an `Identity` entry must never inherit the
  plain `Link` treatment — plus AC-5's caption stating Daedalus has not verified
  it and telling the user to visit the URL and confirm the DRep ID is published
  there. **No copy in this slice may say "verified identity."**
- **R-2 (high) — `paymentAddress` is a scam vector rendered under a "verified"
  label.** A DRep can publish any address; hash verification makes it look
  endorsed. *Mitigation:* AC-6's copy states the address is the DRep's own claim
  and that delegating voting power requires no payment to any address; the value
  is read-only, never pre-populates a form, and never appears on a card; and
  D-10 rejects rather than truncates an over-long address so no partial,
  copyable address can ever render.
- **R-3 (medium) — `doNotList` is best-effort, not authoritative** (D-5). A
  reviewer reading AC-1 literally will find `doNotList` DReps in the cohort on a
  cold session. Recorded as a limitation, not a bug; the acceptance test must not
  hide it by injecting store state.
- **R-4 (medium) — the verified name may rarely appear at the confirmation
  step.** Because `verifiedName` is populated only by a per-detail-visit fetch,
  a user who types an ID straight into the delegation form sees no name. That is
  exactly what task-157 AC-2's "only when available" anticipates, but it means
  the §7 after-anchor-2 template is the exception path in practice, not the
  norm. Product-visible; flag at slice close.
- **R-5 (medium) — ja-JP overflow.** anchor-1 F-16's visual pass is still owed
  and named `!!!高価値` in the fixed-width card top row as a live risk. task-174
  adds a second `<code>` to search rows and task-157 adds five fields including
  long-form prose. No browser exists here.
- **R-6 (medium) — the sanitization floor is stressed by six new field names.**
  If S-3 does not land in the same commit as the parser widening, a redactable
  field ships unlisted and the floor is silently thinned. The two-anchor rule
  applies: cite both the security suite and the sibling logging suite together.
- **R-7 (low) — relaxing `parseVerifiedContent` changes anchor-1 behaviour.** A
  document that previously produced `unavailable` now produces `verified` with a
  null name. The renderer guard at `DRepDetailAnchorContent.tsx:66` keeps the
  visible result identical, but `verifiedMetadataIds` (`:285-291`) and therefore
  `cohortContext` (`:245-252`) and the task-172 category badge may shift.
  **Pinned, not open:** the implementation guide's task-157 §5 decision **D-H**
  states that a hash-matched parse counts as completed metadata regardless of
  `givenName` — `verifiedMetadataIds` keeps keying on `entry.state === 'verified'`
  and nothing else — with the consequence spelled out for cohort eligibility
  (invariant 7's "completed metadata when available" leg) and for the shipped
  High value badge. The proof is the second assertion of the guide's Step 10c
  test `keeps verifiedName null when the anchor carries no givenName`
  (`expect(store.verifiedMetadataIds.has(ANCHOR_DREP_ID)).toBe(true)`) in
  `tests/jest/governance/GovernanceStore.spec.ts`. The badge stays informational
  only (invariant 8), so the shift changes classification, never ordering.
- **R-8 (low) — `helpers.ts` and `normalizeDRepIdentity` keep two CIP-105
  derivations** with different failure modes (`Cardano.DRepID` throws and is
  caught at `helpers.ts:78-79`; `normalizeDRepIdentity` returns `null`). D-8
  scopes the duplication deliberately; a future row may unify them.

### OWED at slice close — nothing here may be reported green

1. `nix fmt` — unavailable in this devcontainer; the prettier substitute is not
   the mandated formatter. Pre-merge obligation, user-owned.
2. The Storybook visual pass and the ja-JP overflow check for the dual-ID search
   row and the five new detail fields — no browser here.
3. A live anchor fetch against a real host (inherits anchor-1 F-10).
4. The real SIPO / CIP-119 canonical vector bytes and their digest check
   (inherits anchor-1 F-13).
5. `yarn check:all` — red at HEAD for a pre-existing Storybook manager-webpack
   reason unrelated to any anchor-2 change.
6. The release-end `!!!` copy review — out of scope for every slice by
   invariant 11.

## Definition of Done

Per task, in the canonical order, each closed by its own single commit
(`<type>(gov): task-NNN <short imperative summary>` — one Conventional Commits
subject line, no body, no trailers):

1. Every acceptance criterion is discharged as written, or as annotated in this
   PRD (D-6 verify-and-record, D-7/D-8 rewrites, D-14 joint discharge), with the
   annotation reflected in the task's tracker `statusReason`.
2. `node_modules/.bin/tsc --noEmit` clean (preceded by
   `node_modules/.bin/typed-scss-modules source/renderer/app` when the
   `precompile` hook is skipped). `tsconfig.json` has no `include`, so this
   covers `source/`, `tests/` **and** `storybook/`.
3. `yarn lint` exits 0 with 0 errors.
4. Focused Jest runs
   (`node_modules/.bin/jest --testPathPattern=<p> --no-coverage --runInBand`)
   green for every suite the task touches, each recorded as a measured
   `baseline → expected` **delta**, not a total. `GovernanceCliArgvSmoke.spec.ts`
   self-skipping when `cardano-cli` is off PATH is expected, not a regression.
5. The sanitization floor re-proved by citing **both**
   `tests/jest/security/governance-sanitization.spec.ts` **and** the sibling
   logging suite in the same Verify block. Citing one is a false green.
6. i18n: every new key present in `en-US.json` and `ja-JP.json` with the leading
   `!!!`; key-set parity holds; `preliminaryCopyMarkers.spec.ts` green, including
   task-154's addition to `PRELIMINARY_CONFIRMATION_KEYS`. Any file
   `yarn i18n:manage` touched but the task did not intend to change is reverted
   with `git restore`.
7. Formatting via `node_modules/.bin/prettier --write <explicit changed paths>`
   only — never `yarn prettier`, never over the tasks tracker or the locale
   catalogs.
8. `yarn storybook` renders the touched stories; the dual-ID and new-field
   coverage uses the global English/Japanese toggle, with no local
   `IntlProvider` and no per-locale story exports, and the integrated
   `Voting / Governance > Connected flow` story still runs.
9. Byte-equality assertions green: `VotingGovernancePage.spec.tsx`'s prop
   contract, `VotingPowerDelegationConfirmationDialog.spec.tsx`, and the
   release-blocking on-device assertions in `shelleyLedger.spec.ts` /
   `shelleyTrezor.spec.ts`.
10. Doc reconciliations applied exactly once — Edit 1 and Edit 2 present, the
    five verify-and-record items confirmed at their corrected anchors with **no
    duplicate paragraph added**.
11. Tracker rows updated with value-only edits (D-16), including task-122's row
    in task-153's commit.
12. This PRD's **Final Outcome** section filled, with every OWED item restated
    and none reported green.

## Final Outcome

*(filled at slice close)*

## References

- Tasks tracker: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` — `anchor-2` phase `:1792-1927`
- Preceding slice: `task-plans/anchor-1-PRD.md`, `task-plans/anchor-1-implementation-guide.md`, `task-plans/anchor-1-code-review.md`, `research/anchor-1-findings.md`
- Designs: `designs/drep-discovery-design.md`, `designs/shared-design-tokens.md`, `designs/current-vote-display-design.md`
- Plan and charter: `governance-drep-discovery-plan.md`, `prompt.md`, `README.md`
- Successor slice: `slice-8` (feature close)
