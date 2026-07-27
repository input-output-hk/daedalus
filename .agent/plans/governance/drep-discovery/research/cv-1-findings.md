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
