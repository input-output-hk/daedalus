# DRep Discovery — UX Design Index

**Status:** Governance-section IA locked; current-vote and anchor follow-on docs remain draft by owning slice
**Date:** 2026-05-27
**Plan:** [governance-drep-discovery-plan.md](./governance-drep-discovery-plan.md)
**Tasks:** [governance-drep-discovery-plan-tasks.json](./governance-drep-discovery-plan-tasks.json)

This folder contains the chosen UX direction for the in-app DRep Discovery feature in Daedalus, plus shared design tokens and external research. Implementation (Storybook + React) is intentionally deferred to the sprint phases in the plan and tasks JSON.

## Working Conventions

- **Canonical status source.** Task status lives in [governance-drep-discovery-plan-tasks.json](./governance-drep-discovery-plan-tasks.json). The current status vocabulary is `pending`, `in_progress`, `partial`, `blocked`, `complete`, `verified`, `deferred`. `complete` means implementation + focused tests + code review are clean; `verified` additionally requires dedicated proof (an in-slice verification task, a targeted regression suite, or manual release verification) — never implementation evidence alone. Standing, event-triggered rows (the `standing` phase) take no extra status term: they rest at `pending` between firings and are reset to `pending` after each one, so recurrence lives in the phase description, not in the vocabulary.
- **Orchestration.** Remaining phases are driven by [prompt.md](./prompt.md) in the locked serial order `slice-2 → slice-3 → ux-refinement → slice-4…7 → cv-1 → cv-2 → anchor-1 → anchor-2 → slice-8`, with JSON `dependencies` always authoritative.
- **Slice-level docs.** Each phase gets `task-plans/<id>-PRD.md`, `<id>-implementation-guide.md`, and `<id>-code-review.md` (append-only), plus an optional `research/<id>-findings.md` — following the slice-1 precedent. No per-task doc file sets.
- **Commits.** One commit per task: `<type>(gov): task-NNN <short imperative summary>`, Conventional Commits, subject only. (Slice-1's single squashed commit predates this convention and stands as-is.)
- **Non-autonomous items.** task-125 (release verification incl. real-device HW QA), the task-166 remainder (mainnet fixture + latency capture), task-158 (fires only on a cardano-wallet pin bump), and the release-end `!!!` copy review are operator/user-owned; everything else runs autonomously.
- **Document precedence.** Use the plan + task tracker for scope and acceptance, slice PRDs for slice-level DoD, implementation guides for execution detail, and design docs for UX/IA/copy contracts.
- **Preliminary i18n copy.** Until the full DRep Discovery feature set is implemented and a final manual copy review is complete, all new en-US and ja-JP DRep Discovery strings must remain explicitly preliminary with `!!!` prefixed to the source text. Removing the `!!!` prefix is a release-end review task, not a per-slice task.
- **Terminology.** In product copy, `Abstain` and `No Confidence` are the built-in DRep options. In analytics docs, `drepOption` is the sanitized field name whose values remain `drep | abstain | no_confidence`.

## Documents

| File | Purpose |
|---|---|
| [external-research.md](./research/external-research.md) | Findings from GovTool, 1694.io, CIP-1694, CIP-119. Patterns to adopt and reject. |
| [research/ux-refinement-sync-and-load-research.md](./research/ux-refinement-sync-and-load-research.md) | Node-sync state, two-phase load, per-phase timeouts, ID-only v1 directory — basis for the `ux-refinement` plan phase. |
| [research/slice-1-final-pass-findings.md](./research/slice-1-final-pass-findings.md) | Verified technical findings from the slice-1 final pass: cardano-cli network-flag placement, bundled CLI version, IPC error transport, test-runner quirk, conway-retry heuristic, FP-11 LSM assessment. |
| [shared-design-tokens.md](./designs/shared-design-tokens.md) | Status badges, source labels, voting-power formatting, randomization indicator, refresh state. |
| [drep-discovery-design.md](./designs/drep-discovery-design.md) | Governance section via renamed `Voting` entry, with sub-routes for directory / detail / favorites — the chosen direction. |
| [current-vote-display-design.md](./designs/current-vote-display-design.md) | `cv-1` / `cv-2` tech design: data model, mapper rules, normalizer, `CurrentVoteSummary` component, sequence diagrams, sanitization. |
| [current-vote-display-ux.md](./designs/current-vote-display-ux.md) | `cv-1` / `cv-2` UX spec: wireframes, knob spec, i18n key inventory. |

## Feature Scope (binding)

- Local-first: data comes only from `cardano-cli` via main-process `GovernanceQueryService`. No external API calls for discovery.
- Default cohort: exclude top 35 by voting power, randomize next ≤200 eligible (active, remaining `drepActivity` > 6 epochs, completed metadata when verified anchor pipeline lands). The "200 with top-35 excluded" sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis (Phase-1 variant, BMVG 2026-05-19); BMVG is cited in user-facing copy and docs as the source of the default.
- Default cohort IS the "Recommended" sort. No separate Recommended tab and no per-card Recommended badge ship in this phase. The four-category badge set (High value / Primary / Threshold / Non-metadata) acts as the early per-DRep explanation surface and lays the groundwork for a future Recommended badge / section.
- Each DRep card and the DRep detail surface exactly one informational category badge (High value / Primary / Threshold / Non-metadata) with a tooltip explaining the rule that placed the DRep there. Badges never reorder the cohort or override the randomized default.
- `slice-4` ships on-chain fields + anchor presence only. `anchor-1` adds verified anchor metadata with explicit source labels.
- Selection only supplies a `DRep ID` to the existing delegation flow in `VotingPowerDelegation`. No second delegation backend.
- Favorites persist via Electron local store, per device, not per wallet, and are not synced.
- Confirmation dialog shows DRep ID only until verified anchor pipeline lands.
- Slice-5 ships Primary / Threshold / Non-metadata badges; High value activates in `anchor-1` once verified metadata exists.
- The `CurrentVoteSummary` panel renders the user's currently delegated DRep with its live active/inactive/expiring status badge so users can tell at a glance whether their existing delegation is still effective.
- No Cucumber / e2e coverage in v1: the Governance section ships no `.feature` files or step definitions. Browse → select → delegate stays covered by the Jest suites plus the operator-owned release verification (task-125) — a deliberate scope boundary, not an oversight.

## Direction

**Governance section via renamed `Voting` entry.** The existing `Voting` sidebar label becomes `Governance`. The governance delegation form, DRep directory, DRep detail, Favorites, and existing Catalyst voting share that single nav entry. Delegation handoff into `/voting/governance` uses React Router `location.state`.

**Single strongest reason:** It reuses the existing governance-related sidebar slot while still absorbing the out-of-scope but already-named future surfaces (proposals, constitution, committee, treasury, dashboard) called out in the plan's _Plan Boundary_ section. A second top-level governance destination would create an avoidable IA collision and another nav migration later.

Trade-off acknowledged: this direction still carries layout, localization, and screenshot churn, but the rename is cheaper and clearer than adding a second top-level nav item. The route-state handoff remains the main implementation risk surfaced in [drep-discovery-design.md](./drep-discovery-design.md).

## Locked Decisions (2026-06-03)

- **Navigation.** Rename the existing `Voting` sidebar entry to `Governance`; do not add a second top-level governance item.
- **Randomization seed.** The default cohort reseeds once per app session. Explicit refresh keeps the current seed; `Reshuffle` is the manual reseed control.
- **Favorites scope.** Favorites are per-device via Electron local store. They are not per-wallet, not synced, and do not follow wallet restore onto a new machine.

## Resolved by Stakeholder Discussion (2026-05-23 / 2026-05-25 thread)

- **Direction.** Phase 1 ships the Governance section through the renamed `Voting` sidebar entry. A future "Enhanced / Explorer" view (V3-style — filters always visible, persistent detail pane, comparison-friendly layout) is recorded under the plan's _Plan Boundary_ as a deferred Phase-2 enhancement, behind a toggle from the default view, once the Phase-1 flow is stable. An in-form embedded selector is not pursued.
- **Cohort sizing rationale.** The 200-DRep default with top-35 exclusion follows the BMVG Simplified Phase-1 analysis (BMVG 2026-05-19, shared with the project on 2026-05-19). BMVG is cited as the source in user-facing copy and docs.
- **Recommended framing.** The default cohort IS the "Recommended" sort. No separate Recommended tab, no per-card Recommended badge in this phase. The four-category badge set is the early per-DRep explanation surface; a simplified Recommended badge / dedicated section is considered for a future update.
- **Four-category labels surfaced.** High value / Primary / Threshold / Non-metadata appear as informational badges with tooltips on every DRep card and on the detail view.
- **Inactivity threshold.** The default cohort enforces a 6+ epoch remaining-`drepActivity` floor (mainnet `drepActivity` is currently 20 epochs). Mock fixtures that exposed "Expiring in 3 epochs" inside the default cohort are fixture-only and MUST NOT ship; production renders MUST respect the floor.
- **Current-delegation status visibility.** `CurrentVoteSummary` must include the delegated DRep's active/inactive/expiring status badge so the user knows whether their existing delegation is still effective and "recommended". The status badge token already exists in [shared-design-tokens.md](./shared-design-tokens.md) §1.
- **High Value badge staging.** Slice-5 ships Primary / Threshold / Non-metadata only. High value activates in `anchor-1` after verified metadata exists.

## Current Risks / Known Interim States

- **Accepted `doNotList` gap.** DReps with `doNotList=true` may still appear in the default cohort from `slice-5` until `anchor-2` lands. That interim state is accepted because the exclusion depends on verified metadata and is not security-critical.
- **Form-only voting sentinels.** `Abstain` and `No Confidence` remain delegation-form choices, not DRep directory entries. The directory IA and empty-state copy stay aligned with that boundary.
- **Refresh latency contract.** The browse surfaces use the `<700ms` skeleton, `700ms–10s` stale-with-spinner, and `>10s` timeout banner budget captured in [shared-design-tokens.md](./shared-design-tokens.md).
- **Confirmation identity follow-up.** The confirmation dialog remains DRep-ID-only until `anchor-2`, where verified identity is added without breaking byte-equality guarantees.
- **Network selector coverage.** Selfnode / no-sync unsupported states are owned by the empty-state copy and status treatments in [shared-design-tokens.md](./shared-design-tokens.md).

## References

- CIP-1694 (liquid democracy, vote delegation semantics, reward-withdrawal gate): https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md
- CIP-119 test vector spec: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/test-vector.md
- CIP-119 canonical example (`drep.jsonld`): https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/examples/drep.jsonld
- Mainnet test vector — SIPO: https://sipo.tokyo/drep/SIPO.jsonld
- Preprod test vector — Cardano Academy: https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld
