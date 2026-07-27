# Slice-4 Findings — DRep Detail View + Route

> Durable findings from slice-4 (2026-07-24). Facts only; grounding anchors verified
> against worktree base `d5e3a03f2`.

---

## F-1 (task-116) — Current-epoch vote positions deferred: no `gov-state` query exists, and the plan conflicts with itself

Vote positions require a `gov-state` query that no main-process task owns — the
slice-1 `GovernanceQueryService` ships `drep-state` + `tip` +
`drep-stake-distribution` only. The plan's "DRep query shape" Key-Decisions row
(`governance-drep-discovery-plan.md` ~:138) is internally inconsistent: it defers
`gov-state` to "the slices that need them" while also stating "proposal vote positions
need gov-state in slice-4". Resolution (PRD D1): the detail renders a labeled
"Current votes" field with the graceful `!!!Vote positions are not available in this
version.` value; task-116's acceptance only requires the view to stay useful without
positions. Any future slice that adds vote positions must first land a `gov-state`
query task and reconcile the plan row.

**Not tasked:** the 2026-07-27 cross-slice planning sweep weighed a
plan-reconciliation row for this finding and left it below the cutoff — no pending
task is scoped to editing `governance-drep-discovery-plan.md`, so nobody builds from
the stale text today. The absorption it proposed, folding the correction into
task-125's release-verification prep, was considered and dropped with it: task-125
kept its single manual release-verification criterion and gained no documentation
edit. The residual to carry forward is that the feature Acceptance Criteria bullet at
`governance-drep-discovery-plan.md:112` still promises "current epoch vote positions
on active proposals" as a delivered detail field, so a release verifier signing off
against that bullet records a false failure — the shipped contract is the PRD D1
resolution above. The stale `--include-stake` / `gov-state` clauses in the
Key-Decisions row (`:139`) and the Technical Design bullet (`:209`) describe an
invocation no code makes (`grep -rn 'include-stake\|gov-state' source/ tests/` returns
zero) and ride whichever future task next opens the plan.

## F-2 (task-116) — The wireframe's "Registered: epoch N" row is dropped: `drep-state` has no registration epoch

`drep-state` output carries no registration epoch — the slice-1 parser reads
expiry/anchor/deposit only, and `DRepDirectoryEntry` (`governance.types.ts:51-62`) has
no registration field. The design wireframe (`drep-discovery-design.md:92`) drifts
from the data model (PRD D4). The row does not exist in the shipped detail; restoring
it would require a new on-chain data source, not a UI change.

**Tasked:** task-151 (anchor-1) — the wireframe correction rides the verified-givenName
edit: `drep-discovery-design.md:92` loses the `Registered: epoch N` row or is annotated
as having no local source, the adjacent `Current votes` row at `:93` is retained with
the shipped unavailable value, and the On-chain box is pinned to the four fields
`DRepDetailOnchainSection` renders (`DRepDetailOnchainSection.tsx:95-143` — Status,
Expires in, Voting power, Current votes).

## F-3 (task-116 → task-117) — `MatomoClient.getAnalyticsURL` is the single analytics URL-embedding boundary; the detail route requires masking

`MatomoClient.getAnalyticsURL` (`MatomoClient.ts:61-63`) embeds
`window.location.hash` into every `track()` payload for both `sendEvent` and
`sendPageNavigationEvent`, so an unmasked `/governance/dreps/:drepId` route would put
the DRep id into every analytics event fired while the detail is open — violating the
sanitization floor. It is the only such boundary (`TrackedRoute` uses the hash for
`matchPath` only; nothing is sent). Resolution (PRD D2): a pure `maskAnalyticsRoute`
helper applied inside `getAnalyticsURL`, with the floor suite extended 20 → 23. The
mask ships with the route in task-117; task-116 left `MatomoClient.ts`,
`Routes.tsx`, and `routes-config.ts` byte-identical to base.

## F-4 (task-117) — The directory route must gain `exact` when the detail route lands

`Routes.tsx:233-237` mounts `DRepDirectoryPage` without `exact`, and the
`<Governance>` children (:226-239) are not inside a `Switch` — adding the detail
route without `exact` on the directory route double-renders both pages on
`/governance/dreps/:drepId` (PRD D8). The Jest harness already models the fixed
shape (`VotingGovernancePage.spec.tsx:165-175`).

## F-5 (task-117) — `DetailRouteStub` migrates to the production detail page without weakening slice-2/3 pins

The slice-2 harness stub (`VotingGovernancePage.spec.tsx:74-99`, stub route at :174)
is replaced by the production `DRepDetailPage` under a nine-edit whitelist (PRD D10);
all other slice-2/3 pins stay byte-identical, and the two-hop test's final assertions
are unchanged. The pinned contract the production page satisfies: detail receives
`{ from, selectedWalletId, voteType }` from the directory's push and forwards it plus
`selectedDRepId` (byte-equal route param) via `pickDelegationFormReturnState`
(`delegationFormState.ts:50-62`) to `inherited?.from ?? ROUTES.VOTING.GOVERNANCE`.

## F-6 — `npx` is broken in this devcontainer; use `node_modules/.bin/<tool>` or `yarn <tool>`

npm 11.13.0 rejects the repo's string-form `package.json` `devEngines`
(`"node": ">=v22.0.0"`) with `npm error Invalid property "devEngines.node"` before
the tool runs, so every `npx tsc/eslint/jest/prettier` invocation fails. Verified
working substitutes: `node_modules/.bin/tsc` (4.9.5), `node_modules/.bin/eslint`
(8.13.0), `node_modules/.bin/jest` (27.5.1), `node_modules/.bin/prettier` (2.1.2),
and `yarn <tool>`.

## F-7 (planning drift, for future planners) — Stale anchors and counts in earlier-slice docs

Three earlier-slice documentation anchors had drifted by the time slice-4 was planned
and built:

- The harness route registrations live at `VotingGovernancePage.spec.tsx:165-175`,
  not the `:169-174` the planning brief cited.
- The sanitization floor suite was already 20 tests before this slice (slice-2 docs
  said 17 — slice-3 added 3). Task-117 extends it to 23 with the three
  `maskAnalyticsRoute`/`MatomoClient` masking tests; any future id-bearing route must
  extend `maskAnalyticsRoute` and this suite.
- Shared-design-tokens §9 pre-assigns no message id for the "On-chain anchor
  reference" label (§9 lists only `sourceLabel.onchain|verified|unverified|anchorUnavailable`);
  the new id is `governance.drepDetail.sourceLabel.anchorReference` (PRD P-1).
