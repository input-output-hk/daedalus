# CV-1 Findings — Doc-vs-Repo Conflicts Resolved at Planning

> Durable findings from cv-1 planning (2026-07-27). Facts only; grounding anchors
> verified against the `wt/cv-1` working tree at base `b900b99b3`. Resolution rule
> applied throughout: prefer the live repo / the tasks JSON, record the conflict
> here, and reconcile the governing doc only where the planning mandate says to.

---

## F-1 — Plan Key Decisions row attributed the wire-literal fix to task-128; the tasks JSON assigns it to task-127 (JSON authoritative; plan reconciled)

The plan's Key Decisions table said the `'voting_and_delegating'` →
`'delegating_and_voting'` literal fix lands in "cv-1 (task-128)"
(`governance-drep-discovery-plan.md:152`, pre-reconciliation text: "the Daedalus
`'voting_and_delegating'` literal is a bug fixed in cv-1 (task-128) (constant
export name preserved)"). The tasks JSON assigns that work to **task-127** — id at
`governance-drep-discovery-plan-tasks.json:826`, title "Fix latent
delegating_and_voting literal mismatch" at `:827`, with `targetPath:
"source/renderer/app/api/wallets/types.ts"` at `:835` and the acceptance criterion
"WalletDelegationStatuses.VOTING_AND_DELEGATING === 'delegating_and_voting'
(constant name preserved)." at `:838`. task-128 in the JSON is a different task:
"Widen WalletDelegation and WalletNextDelegation with voting field" (`:844-845`).
The buggy literal is live in the repo at `source/renderer/app/domains/Wallet.ts:42`
(`VOTING_AND_DELEGATING: 'voting_and_delegating',`) and
`source/renderer/app/api/wallets/types.ts:84` (`| 'voting_and_delegating';`).

**Resolution:** tasks JSON is authoritative. The plan row at
`governance-drep-discovery-plan.md:152` was reconciled during cv-1 planning to
read "task-127" (smallest possible edit; nothing else in the file touched). The
cv-1 PRD scopes the fix under task-127.

## F-2 — Plan sequencing note says "58 tasks across 13 phases"; the tasks JSON metadata says 69 tasks across 14 phases (record-only)

`governance-drep-discovery-plan.md:265` reads: "The companion [task
tracker](governance-drep-discovery-plan-tasks.json) is the source of truth: 58
tasks across 13 phases (`slice-1`..`slice-8`, `cv-1`, `cv-2`, `anchor-1`,
`anchor-2`, `standing`)." The JSON it defers to says otherwise:
`governance-drep-discovery-plan-tasks.json:16` and `:1730` both carry
`"totalTasks": 69`, and `:1729` carries `"totalPhases": 14`. Independent counts
against the same file confirm the metadata: 69 `"id": "task-` entries and 14 phase
`"name"` entries. The drift comes from the `ux-refinement` phase being added after
the plan note was written.

**Resolution:** record-only. The plan sentence itself declares the JSON the source
of truth, so the JSON numbers (69 tasks / 14 phases) govern; the stale sentence is
left as-is because the cv-1 planning mandate confines plan edits to the
`:152` attribution fix.

## F-3 — Design-doc testing table uses stale `.test.ts` names and a `voting-sanitization` suite; the live convention is `.spec.ts` and the floor suite is `governance-sanitization.spec.ts` (record-only)

`designs/current-vote-display-design.md` names test files that do not match the
live tree: `tests/jest/api/createWalletFromServerData.test.ts` (`:248`),
`source/renderer/app/domains/__tests__/Wallet.test.ts` (`:250`), and
`tests/jest/security/voting-sanitization.test.ts` (`:253`). The live repo contains
no `*.test.ts*` file under `tests/` at all; the established convention is
`*.spec.ts` (e.g. `tests/jest/governance/GovernanceQueryService.spec.ts`,
`tests/jest/governance/GovernanceStore.spec.ts`), and the sanitization floor suite
that cv-1 must re-assert green is `tests/jest/security/governance-sanitization.spec.ts`
(exists on disk; there is no `voting-sanitization` suite).

**Resolution:** prefer the live repo. cv-1 test files (task-134) use the
`.spec.ts` / `.spec.tsx` convention and the sanitization floor is re-asserted via
`tests/jest/security/governance-sanitization.spec.ts`. The design doc is not
edited in this slice; its §12 table is recorded here as stale naming only — the
coverage rows themselves remain valid.

## F-4 — `.agent/system/api-endpoints.md:50` listed the stale `voting_and_delegating` status value as current fact (live wire value authoritative; doc reconciled)

Found during the task-127 build, not at planning. The system API reference
listed the delegation status enum under the heading "Delegation status values
in `delegation.active.status` include:" (`.agent/system/api-endpoints.md:46`)
and its fourth entry read `voting_and_delegating` (`:50`) — the same latent
bug F-1 tracks in the renderer source, mirrored into a system doc where it
read as an assertion about the cardano-wallet wire contract rather than as
prose about a known defect. Left alone it would have kept re-seeding the wrong
literal into future work after the code was fixed.

**Resolution:** prefer the live repo. The one in-repo artifact that claims
wire truth is the task-126 fixture
`tests/mocks/wallets/wallet-delegating-and-voting.json:25`
(`"status": "delegating_and_voting"`), and task-127 made
`source/renderer/app/domains/Wallet.ts:42` and
`source/renderer/app/api/wallets/types.ts:84` byte-match it. `:50` was changed
to `delegating_and_voting` — a single-line value swap, the only occurrence of
the enum in that file, with `:47-49` untouched. Residual, recorded rather than
resolved: the value still cannot be confirmed against the pinned
cardano-wallet v2026-05-11 swagger in this devcontainer (no vendored spec, no
live wallet), and the fixture was itself authored from the swagger *shape*
rather than captured live (`cv-1-PRD.md:58`), so fixture, source and doc now
agree by construction. Pre-merge: confirm `delegating_and_voting` against the
swagger `ApiWalletDelegationStatus` enum in an environment that has it. Note
this is a doc *value* fix only — the surviving `voting_and_delegating` strings
elsewhere in `.agent/` are prose narrating the bug (including task-127's own
tracker description and acceptance criteria at
`governance-drep-discovery-plan-tasks.json:837,846,849`) and were correctly
left in place.

## F-5 — `nix fmt` cannot run in this devcontainer; `node_modules/.bin/prettier` is the substitute (pre-merge obligation, carried forward)

Found during the task-126/task-127 builds. The repo's formatting gate is
`nix fmt`, but this devcontainer has no nix, so the gate cannot be executed
here at all. Every cv-1 build task to date has instead run the prettier 2.1.2
binary directly — `node_modules/.bin/prettier --check <paths>`, never
`yarn prettier` — which passed clean on all task-127 touched files
(`source/renderer/app/api/wallets/types.ts`,
`source/renderer/app/domains/Wallet.ts`,
`tests/jest/api/walletDelegationStatuses.spec.ts`,
`.agent/system/api-endpoints.md`,
`.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`).

**Resolution:** substitution accepted for in-devcontainer work; the `nix fmt`
pass is **owed pre-merge** and is not discharged by the prettier run. Two
related scope facts worth keeping: `package.json:43` scopes `yarn lint` to
source/storybook/utils, so new specs under `tests/` are outside lint's reach
by convention (matching the existing `tests/jest/security` spec), and
`npx jest` fails in this devcontainer with `npm error Invalid property
"devEngines.node"` — invoke `node_modules/.bin/jest` directly instead.

## F-6 — `WalletVotingTarget`'s `source` member is mandated by the design and the guide but named in no task-128 acceptance criterion (contract authoritative; criteria left as-is)

Found during the task-128 build. The `drep` variant of `WalletVotingTarget`
carries `source: 'verified' | 'unverified' | 'onchain'` — prescribed verbatim by
`designs/current-vote-display-design.md:89` and reproduced in the guide's Step 2
block at `cv-1-implementation-guide.md:617`. None of task-128's four acceptance
criteria (`governance-drep-discovery-plan-tasks.json:881-884`) mentions it: AC-2
constrains only the discriminator name and its three values, and AC-3/AC-4
constrain only `DRepIdentity`. Read criteria-first, the member looks like scope
creep; read contract-first, omitting it would silently drop the provenance
channel that the later status/badge work depends on.

**Resolution:** the design + guide are the build contract and the acceptance
criteria are a floor, not a ceiling — the member is in scope and was included
(live at `source/renderer/app/api/wallets/types.ts:90`). Adjudicated before
review, so no lens flagged it. Recorded because the same criteria-vs-contract
gap will recur wherever a design block is richer than the tracker's criteria;
the criteria themselves were deliberately not rewritten (cv-1 planning is
closed). Nothing in cv-1 yet *populates* `source` — that lands with the mapper
(task-130) and, for the `'verified' | 'unverified'` distinction, with the cv-2
status work.

## F-7 — `source/common/types/governance.types.ts` is not erasable: it exports a runtime enum, so renderer type imports from it must use `import type`

Found during task-128 review. The module is overwhelmingly type declarations,
but `:105` is `export enum GovernanceQueryErrorType {` — a real runtime value.
Both import forms are live in the repo against the same module:
`source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx:18`
takes the enum as a plain value import, while every type-only consumer uses
`import type` (e.g.
`source/renderer/app/containers/voting/VotingGovernancePage.tsx:12`,
`source/renderer/app/components/governance/_shared/DRepStatusBadge.tsx:4`,
`source/main/utils/setupLogging.ts:21`). Because the module is not erasable, a
plain `import { DRepIdentity } from '.../governance.types'` would create a real
runtime module edge — in task-128's case from `source/renderer/app/api/wallets/types.ts`,
which already sits in an import cycle with `source/renderer/app/domains/Wallet.ts`.

**Resolution:** task-128 used `import type` (`types.ts:6`) and adds no runtime
edge. The hazard is that dropping the `type` keyword is invisible in review —
it compiles, lints clean, and only shows up as a cycle or bundle change. Every
downstream task that pulls a type out of `governance.types.ts` (task-129's
normalizer, task-130's mapper, task-131's domain widening) must keep the
type-only form; `source/common/types/governance.types.ts` itself has zero
imports of any kind, so the cycle risk is entirely on the importing side.
