# Slice-5 Findings — Default Cohort + Category Badges + BMVG Banner

> Durable findings from slice-5 (2026-07-24). Facts only; grounding anchors verified
> against slice head `9a17bc891` (worktree base `b6b94268e`).

---

## F-1 (R1, task-118) — The plan's "DRep query shape" row was stale: cohort ranking derives from the Phase-2 `drep-stake-distribution` enrichment, not inline `--include-stake` stake

The plan's Key-Decisions "DRep query shape" row
(`governance-drep-discovery-plan.md:139`) still suggested slice-5 "may derive top-35
from the inline `--include-stake` per-DRep stake instead of a second query" — a
slice-1 shape that ux-refinement's two-phase load replaced (ux-refinement findings
F-1; ratified as the "First-load shape" row, plan :162). As shipped, ranking consumes
only the enriched value: `fetchDRepList` paints Phase 1 (`GovernanceStore.ts:190-238`),
`_enrichVotingPower` merges the `governanceDRepStakeChannel` payload as
`new BigNumber(stake)` (:246-271, rehydration at :255), and
`compareByVotingPowerDesc` (:69-84) ranks by BigNumber `comparedTo` with null powers
last and a drepId tie-break — never coercing to `Number`. The plan row was amended at
slice close to state the Phase-2 derivation and cite this finding.

## F-2 (R2, task-118) — The cohort exists only at `votingPowerState === Loaded`; Loading/Failed fall back to the full list with no cohort claim

`isCohortActive` requires `votingPowerState === VotingPowerEnrichState.Loaded` plus a
non-empty list (`GovernanceStore.ts:143-148`); `defaultCohort` returns `null` unless
active (:157-158); `displayedDRepList` falls back `defaultCohort ?? drepList`
(:178-180). While Phase 2 is `Loading` the directory keeps the Phase-1 full-list
paint; on `Failed`, `isRankingUnavailable` (:138-140) drives the existing
`rankingUnavailable` banner and the full list renders. The banner's cohort line and
Reshuffle control are gated on the same `isCohortActive` prop
(`DRepDirectoryBanner.tsx:85-96`), so the banner can never claim a cohort the list is
not showing — one computed chain feeds both surfaces
(`DRepDirectoryPage.tsx:84,91,93`).

## F-3 (R3, task-119) — Interim metadata completeness is on-chain anchor presence, feeding badges only, until anchor-1 (task-151)

No verified-metadata flag exists yet, so `getDRepCategory` uses
`entry.anchor != null` as the completeness proxy: the 7–12 remaining-epoch window
wins outright, otherwise anchor presence decides Primary vs Non-metadata
(`DRepCategoryBadge.tsx:50-51,60-69`). The proxy feeds ONLY the badge: cohort
eligibility is `status === 'active'` AND `drepActivity > 6` and never reads `anchor`
(`GovernanceStore.ts:160-167`), and `GovernanceStore` never imports the badge module
(imports at `GovernanceStore.ts:1-14`) — invariant #8 holds structurally. anchor-1
(task-151) upgrades the proxy to the verified flag, and the entry snapshot
`getDRepCategory` takes today (`Pick<..., 'status' | 'drepActivity' | 'anchor'>`,
`DRepCategoryBadge.tsx:45-48`) does not survive that upgrade: the classifier needs an
explicit cohort-membership input plus the verified completeness flag in place of
`anchor`, so anchor-1 changes the signature rather than extending the rules within it.

**Tasked:** task-172 (anchor-1) — ground `getDRepCategory` in a store-owned
cohort-membership input, consume task-151's verified metadata-completeness flag in
place of `entry.anchor`, and activate the fourth `High value` category.

## F-4 (R4) — The "Expiring soon" status-badge variant is design drift with no owner; `DRepStatus` is unchanged

`shared-design-tokens.md` §1 (:13, :20) says "`Expiring soon` joins with the slice-5
`Threshold` category window", but no slice-5 task in the tasks JSON asks for a status
badge variant. As shipped, canonical `DRepStatus` stays `'active' | 'inactive'`
(`governance.types.ts:35`, invariant #14) and `DRepStatusBadge.tsx` is byte-identical
to base (absent from the slice diff). The 7–12-epoch window is displayed by the
Threshold **category** badge instead (`DRepCategoryBadge.tsx:60-67`). The
status-badge variant remains unbuilt and needs a future owner before the tokens-§1
staging claim is true.

**Considered and dropped (2026-07-27 sweep):** a 5h anchor-1 row for this variant and
its top-35 sibling (carried as slice-6 F-6, `slice-6-findings.md:73`) was weighed and
left below the cutoff — the expiry information is one click away
(`DRepDetailOnchainSection.tsx:102-113` renders `Expires in {n} epochs` for every
active entry with `drepActivity`, and the Threshold category badge covers the 7–12
window for cohort entries), and slice-6 had already declined both badges deliberately
(`slice-6-PRD.md:110` D-5, `:230` P-13). The drift stays unowned and
`drep-discovery-design.md:111` still names both badges for stale favorites; task-172's
cohort-membership input is the plumbing the top-35 half would need if it is ever
resurrected.

## F-5 (R5, task-118/120) — The cohort banner ships link-free; Show-all / Search land in slice-6

The shipped banner defines exactly three cohort keys — `cohortBanner`,
`cohortBanner.reshuffle`, `cohortBanner.source`
(`DRepDirectoryBanner.tsx:26-42`; `en-US.json:313-315`, `ja-JP.json:313-315`, all
`!!!`-prefixed) — deliberately diverging from the tokens-§5 sentence containing
`{ShowAllLink} or {SearchLink}`. `cohortBanner.showAll` is a separate §9 key that
slice-6 (task-121) will introduce with the search/show-all surfaces; until then
excluded/non-cohort DReps remain reachable only via `drepList`/`drepIndex` and the
detail route. The primary line's "up to 200" is static copy with no `{n}`
placeholder (PRD P-13).

## F-6 (R8, task-118) — Seeded-shuffle mechanics and the seed lifetime

`seededShuffle.ts` implements mulberry32 (:6-15) driving an input-non-mutating
Fisher–Yates (:21-29), with `generateCohortSeed()` = 32-bit `Math.random` value
(:17-19). Seed lifetime: created once per app session at store construction
(`GovernanceStore.ts:110`), preserved across `refresh()` (:274-277 — it only calls
`fetchDRepList`, which never touches `cohortSeed`), and replaced ONLY by
`reshuffleCohort()` (:284-287), whose body is a single seed assignment — zero CLI/IPC
(invariant #6, Jest-pinned via unchanged channel-mock call counts). Display order is
a pure function of (membership set, seed): the ≤200 eligible selection is re-sorted
by drepId ascending before the shuffle (:169-174), so a refresh that changes voting
powers without changing membership yields a byte-identical order. The seed is never
logged or persisted — the entire cohort path makes no logger/analytics/storage call
(invariant #2; floor suite 23/23 at close).
