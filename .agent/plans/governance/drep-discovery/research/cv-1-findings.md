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

## F-8 — task-129's tracker description says the normalizer classifies `abstain` / `no_confidence`; the shipped normalizer rejects them (live repo + guide authoritative; tracker prose stale)

Found during the task-129 build and confirmed at review. The tracker's task-129
`description` (`governance-drep-discovery-plan-tasks.json:890`) reads that the
function "classifies it as drep (CIP-129) / drep_vkh (CIP-105 key) /
drep_script (CIP-105 script) / abstain / no_confidence". The shipped function
does not: `'abstain'` and `'no_confidence'` are not bech32, so they fail
`bech32.decode` and return `null` via the bare `catch` at
`source/renderer/app/utils/governance/normalizeDRepIdentity.ts:24-26` — pinned
by the rejection vectors at `tests/jest/governance/normalizeDRepIdentity.spec.ts:86-87`.
That is the intended behaviour, not an omission: the guide's resolved-judgment-calls
block states the sentinels "are NOT DRep ids — the normalizer rejects them; the
mapper (task-130) handles them BEFORE calling the normalizer"
(`cv-1-implementation-guide.md:926-928`), which is invariant 13 (sentinels are
form-only, never DRep directory entries).

**Resolution:** the live repo and the guide govern; the tracker sentence is the
stale side and was left unedited (task-129's scribe mandate confines JSON edits
to that task's status fields). Read description-first, a future implementer
would add a sentinel branch to the normalizer and quietly turn a form-only
sentinel into something that can flow toward the DRep directory. The sentinel
branch belongs in task-130's `parseVoting`, ahead of the normalizer call.
Same criteria-vs-contract shape as F-6, but inverted: here the tracker prose is
*wider* than the contract rather than narrower.

## F-9 — bech32 accepts all-uppercase ids, so `raw`/`cip129` and the derived `cip105` can differ in case; the same-vote comparator must key on `credentialHex` + `credentialType`

Found during task-129 review. BIP-173 permits all-uppercase bech32 and
`bech32` 2.0.0 lower-cases the HRP internally, so
`normalizeDRepIdentity('DREP1Y2SM9S75UHMQWXPF8F94CMT737G2RVKR6NJLVPCC9YAYKHQ23NMJY')`
decodes successfully (prefix `drep`, 29 bytes, header `0x22`) rather than being
rejected. Because invariant 10 requires `raw` to be returned byte-untouched —
no trim, no lower-casing (`normalizeDRepIdentity.ts:39`, `:54`) — an uppercase
input yields uppercase `raw`/`cip129` alongside a lowercase derived `cip105`.
Consequence: `normalize(x.cip105).cip129 !== x.cip129` for uppercase input, so
the CIP-129 → CIP-105 → CIP-129 round-trip is byte-exact only for lowercase
input. Mixed case is correctly rejected by the library (verified:
`'drep1y2SM9s75…'` → `null`).

**Resolution:** not a defect and explicitly not to be "fixed" in the
normalizer — case-folding `raw` would violate invariant 10 to satisfy a weaker
one. The durable consequence is downstream: the task-130/131 same-vote
comparator must compare on `credentialHex` + `credentialType` (`credentialHex`
comes from `toHex` at `normalizeDRepIdentity.ts:8-9` and is always lowercase,
hence case-stable) rather than on the bech32 strings, which can legitimately
differ in case between two representations of one identity. Note
`credentialHex` alone is insufficient — a key and a script DRep can share the
same 28 bytes (asserted at `normalizeDRepIdentity.spec.ts:77`), which is why
the pair is required. UNVERIFIED whether any upstream source actually emits
uppercase: every observed cardano-wallet fixture and cardano-cli output is
lowercase, so this is defensive rather than reactive.

**Tasked:** task-140 (cv-2) — the downstream constraint recorded here is owned
by the task-140 amendment: `isSameAsCurrent` compares on a case-stable key (the
`credentialHex` + `credentialType` pair, or a case-insensitive `cip129`), its
behaviour when `credentialHex` is absent is explicit, and the letter-case
regression vector is placed on task-147. Worth recording with the marker: this
finding prescribed the correct key at review time and two governing docs then
drifted from it — `designs/current-vote-display-design.md:95` still offers
"canonical CIP-129 string including the type-byte header" as an acceptable
comparison key, and `task-plans/cv-1-code-review.md:736-738` still offers
`cip129` alone. Both are corrected under the same amendment; the code-review
file is append-only (`README.md:14`), so its correction is appended rather than
edited in place.

## F-10 — two residual test gaps on `normalizeDRepIdentity`, both deliberately deferred to task-130

Found during task-129 review; both were raised as blockers and both were
refuted for task-129, so they are recorded here rather than fixed.

1. **Uncovered rejection branch.** The focused run reports 96.66% statements /
   94.11% branch / 100% functions for the module, with exactly one uncovered
   line: `normalizeDRepIdentity.ts:48`, the CIP-105 length-mismatch
   `return null` (a `drep_vkh` / `drep_script` payload that is not 28 bytes).
   The behaviour was verified directly against the transpiled module
   (`bech32.encode('drep_vkh', toWords(29 bytes))` → `null`;
   `bech32.encode('drep_script', toWords(27 bytes))` → `null`), and the
   mirror-image CIP-129 length check at `:28-30` *is* covered via the
   deprecated 28-byte `drep1` vector. AC-4 is met without it, and adding a
   vector would break the spec's byte-exactness with the approved guide block
   (`cv-1-implementation-guide.md:819-923`), which `diff -u` confirms is
   currently exact.
2. **The purity / no-logging floor is structural, not asserted.** Invariant 1
   holds today — `grep -n "logger\.\|console\."` over the module returns
   nothing — but no test would fail if a later edit added `logger.warn(raw)`
   inside the function. The boundary-based floor suite
   `tests/jest/security/governance-sanitization.spec.ts` would not catch it
   either: it does not import this module (imports at `:21-28`).

**Resolution:** deferred by design. The guide places the logger spy at task-130
(`cv-1-implementation-guide.md:2041-2062`, `:2165`), where the caller emits the
sanitized unknown-HRP warning; the floor assertion covering
`normalizeDRepIdentity` itself should land in the same task, and the `:48`
vector is a cheap add once the guide's byte-exactness constraint no longer
applies.

**Tasked:** task-134 (cv-1) — both gaps are owned by the task-134 amendment. It
adds a checksum-valid wrong-length `drep_vkh` / `drep_script` vector that
reaches the `:48` length guard rather than the decode catch, and it asserts
`expect(mockedWarn).not.toHaveBeenCalled()` in the valid-DRep mapper tests, so
the no-logging floor is pinned on the accepted-id path and not only on the
rejection paths the spec already covers. The amendment also records
`tests/jest/governance/normalizeDRepIdentity.spec.ts` as a MODIFIED file in the
guide's task-134 section, whose file-list header at
`cv-1-implementation-guide.md:1959` currently declares every file new. The
"deferred to task-130" wording in this finding's heading and resolution stands
as written history — F-12 below already records that it should be read as
task-134.

## F-11 — `AdaApi::getWallets` logs the raw `wallets` array, so `delegation.active.voting` reaches the log file unsanitized (pre-existing at HEAD, outside task-130's fence, unguarded by the floor suite)

Found during the task-130 review's invariant lens while auditing every sink the
new `votingTarget` could reach. `source/renderer/app/api/api.ts:379-383` reads
`logger.debug('AdaApi::getWallets success', { wallets, legacyWallets,
hwLocalData: filterLogData(hwLocalData) })` — only `hwLocalData` is sanitized.
The `wallets` array is the raw `GET /v2/wallets` payload, so from cv-1 onward
it carries `delegation.active.voting`, i.e. the CIP-129/CIP-105 DRep id or an
`abstain` / `no_confidence` sentinel literal, straight into the log file. There
is no global sanitizing transport to catch it: `source/renderer/app/utils/
logging.ts:26-43` forwards `data` untouched to `electronLog[level]`, and a grep
for electron-log hooks or custom transports finds none — `filterLogData` is
call-site-only. The floor suite `tests/jest/security/governance-sanitization.spec.ts`
pins the `delegateVotes` call boundary (`:203-240`) but has no `getWallets`
case, so nothing currently guards this line. Invariant 2 (sanitization floor)
is the invariant at stake.

**Resolution:** not a task-130 regression and deliberately not fixed there. The
line is byte-identical at HEAD — verified with `git show HEAD:source/renderer/
app/api/api.ts` — and task-130's scope fence covers only `parseVoting`, the
delegation switch and the constructor pass-through, so touching it would have
been scope creep. Task-130's own new sink is clean by contrast: the single
`logger.warn` at `api.ts:3025-3027` emits one bounded `hrp` token and nothing
else. The durable action is a follow-up floor task that either wraps `wallets`
in `filterLogData` at the call site or drops it from the payload, plus a
`getWallets` case in the floor suite so the gap cannot reopen. Sizing note: the
same shape may exist at other `logger.debug`/`logger.info` call sites in
`api.ts` that pass whole server payloads — the audit was scoped to the wallet
list, not exhaustive.

**Tasked:** task-170 (cv-1) — the follow-up floor task now exists as a row. It
wraps `wallets` / `legacyWallets` at `api.ts:379-383` and `wallet` at
`:458-460` in `filterLogData` (or reduces them to non-identifying fields) and
adds the `getWallets` call-boundary case to
`tests/jest/security/governance-sanitization.spec.ts`. The sizing note above is
discharged rather than carried forward: task-170's third acceptance criterion
makes the audit of every remaining whole-payload `logger.*` call site in
`api.ts` a deliverable, with the audit list recorded in the task evidence. This
finding also drives the tracker bookkeeping for the two slice-1 sanitization
tasks — task-109 records this call site and task-170 in its statusReason, and
task-111 keeps a statusReason caveat that its floor suite has no `getWallets`
case. One scope bound belongs with the marker: task-170's fence is `api.ts`, so
the neighbouring uncontrolled sink recorded at `slice-3-findings.md:71-74`
(`HardwareWalletsStore` logs raw `{ error }` across its `[HW-DEBUG]` calls)
stays open by design — it is a substring-inside-`error.message` class that
key-based `filterLogData` cannot reach at all (`slice-3-findings.md:65`), not a
call site task-170 declines.

## F-12 — task-130 discharges F-8 and populates F-6's `source` member, but ships with no test of its own by design: AC-1..AC-5 and its new sanitized warning are pinned only by task-134 (and F-10's resolution line mis-locates the logger spy at task-130)

Three prior findings changed state with the task-130 build.

**F-8 is discharged in code.** The sentinel branch now lives where invariant 13
requires it: `source/renderer/app/api/api.ts:3019` returns `{ kind: 'abstain' }`
and `:3020` returns `{ kind: 'no_confidence' }`, both *ahead* of
`normalizeDRepIdentity(voting)` at `:3021`. `'abstain'` and `'no_confidence'`
therefore never reach the bech32 decoder, never produce a `null`, and never
trigger the unknown-HRP warning. F-8's stale tracker sentence at
`governance-drep-discovery-plan-tasks.json` (task-129's `description`) was
again left unedited — each scribe's JSON mandate is confined to its own task's
status fields — so the prose stays stale while the code is now unambiguous.

**F-6's `source` member is consumed, half of it.** `api.ts:3030` returns
`{ kind: 'drep', drep, source: 'onchain' }`, so the provenance channel that
task-128 declared but nothing populated is now written on the only path that
produces a DRep target. The `'verified' | 'unverified'` arms remain unwritten
by any code in cv-1; they belong to the cv-2 status/badge work, exactly as F-6
predicted.

**F-10's two gaps stay open, and its anchor is wrong.** F-10's resolution says
"the guide places the logger spy at task-130 (`cv-1-implementation-guide.md:
2041-2062`, `:2165`)". Those anchors are inside the **task-134** section, which
runs from `cv-1-implementation-guide.md:1957`; they are the `jest.mock` of the
renderer logger and the unknown-HRP assertion in
`tests/jest/api/createWalletFromServerData.spec.ts`, a file task-134 creates.
The task-130 section (`:961-1204`) creates no test at all: `:1179-1184` states
plainly that behavioural assertions "are formally pinned by the task-134
specs", and this task's gate is "compile + lint + all existing suites + the
floor suite + the greps". So the `:48` coverage vector and the floor assertion
over `normalizeDRepIdentity` both roll forward to task-134, and task-130's own
`logger.warn` joins them as a third unasserted behaviour.

**Resolution:** the guide governs and the absence of a task-130 test is by
design, not an omission — it is why the tracker records task-130 as `complete`
and explicitly not `verified`. AC-1..AC-5 were adjudicated at review by static
reading plus hand-tracing the four task-126 fixtures (with both DRep vectors
independently decoded through the repo's bech32 2.0.0), which is evidence but
not an executing test. Task-134 is now the single point where the mapper's
behaviour, the sentinel branches, the unknown-HRP warning payload, the
`normalizeDRepIdentity` floor assertion and the `:48` vector all become
regression-proof; if task-134 is trimmed, cv-1 ships with the mapper untested.
F-10's task-130 attribution should be read as task-134.

## F-13 — cv-1 gate-recipe corrections: `jest tests/jest` runs only 7 of 82 suites, PRD R-4 (Node v24 `yarn compile` flakiness) is stale, and api.ts's prettier failure is proven pre-existing

Three gate facts established while running task-130's verification.

**The `tests/jest` path filter under-runs the tree by an order of magnitude.**
`node_modules/.bin/jest tests/jest` treats its argument as a path regex, and
`jest.config.js` collects from `roots: ['<rootDir>/tests', '<rootDir>/source']`
with `testMatch: ['**/?(*.)+(spec|test).[tj]s?(x)']`, so specs colocated under
`source/` and those under `tests/assets`, `tests/common` and `tests/wallets`
are all excluded by the filter. Measured on this build: the filtered run is
7 of 7 suites and 122 tests (110 passed, 12 skipped); the unfiltered
`node_modules/.bin/jest --runInBand --coverage=false` is 82 suites and 1050
tests (1038 passed, 12 skipped, 2 snapshots), exit 0 in both cases. The
"80 suites / 1030 tests" baseline quoted in the task-127 and task-128 tracker
entries is the *unfiltered* number; the small excess over it comes from suites
added earlier on the branch, and the skip count matches exactly at 12.

**PRD R-4 is stale.** `cv-1-PRD.md:407-411` rates "Node v24 gate flakiness" as
an open low risk on the grounds that `yarn compile` has previously failed for
environment reasons under Node v24.16.0. It did not fail here, and it did not
fail for task-129 either: `node_modules/.bin/typed-scss-modules
source/renderer/app` regenerated every gitignored `*.scss.d.ts` in a fresh
worktree with no errors, and `node_modules/.bin/tsc --noEmit` then exited 0
with zero diagnostics. Two consecutive cv-1 tasks have now cleared the gate on
Node v24.16.0.

**The api.ts prettier failure is pre-existing, and provably so.**
`node_modules/.bin/prettier --check source/renderer/app/api/api.ts` exits 1 on
the task-130 working copy — and exits 1 identically on the *HEAD blob* of the
same file extracted to a scratch path. Formatting each yields the same nine
drift hunks at offsets differing by exactly `+2`, the two added import lines:
`@@788/790`, `1035/1037`, `1637/1639`, `1655/1657`, `2162/2164`, `2187/2189`,
`2211/2213`, `2400/2402`, `2605/2607`. All nine are the known prettier-2.1.2
assignment-break drift, and the furthest sits ~400 lines above the new code at
`api.ts:3012+`. `Wallet.ts` is clean.

**Resolution:** for future cv-1 tasks, run the unfiltered `node_modules/.bin/
jest --runInBand` (optionally `--coverage=false`) as the tree-wide gate and use
a path filter only for focused suite runs — a filtered green proves 7 suites,
not 82, and a recipe that conflates them can report a false all-clear. Keep
`node_modules/.bin/tsc --noEmit` as the compile gate (it is the authoritative
one under F-5's no-nix constraint), but stop treating R-4 as an expected
failure; capture the error if it ever does recur rather than pre-assuming it.
For prettier, the F-5 rule stands and is now backed by a repeatable proof
technique: when `--check` fails on a file you modified, diff the HEAD blob's
formatting against the working copy's before touching anything — if the hunk
sets match modulo your line offsets, the drift is inherited and the file must
be left unformatted. `nix fmt` remains owed pre-merge.

**Tasked:** task-131 (cv-1) — the gate-recipe half is owned by the task-131
amendment: task-131's own verification runs the unfiltered
`yarn test:jest --runInBand` and reports suite and test counts in its
statusReason, and the two filtered recipes still attached to pending tasks are
rewritten to the unfiltered form — `cv-1-implementation-guide.md:1331` (inside
`## task-131`) and `:1740` (inside `## task-132`). The same recipe at `:547`,
`:674` and `:1172` belongs to task-127, task-128 and task-130, all complete, so
those three are historical and deliberately left; `:2424` is a deliberate
focused run whose comment makes no tree-wide claim. The PRD R-4 and prettier
halves of this finding stay record-only and have no owner.

## F-14 — nineteen feature-introduced ja-JP strings carry no `!!!` preliminary marker while their en-US counterparts do; invariant 11 binds both locales and nothing guards the gap

Found while sweeping the dossier for durable work, not during a cv-1 build.
Invariant 11 is explicit that the marker is a both-locale obligation: "Every new
en-US and ja-JP string keeps the leading `!!!` marker. Removing `!!!` is a
release-end manual review, never a per-slice task"
(`prompt.md:132-133`). The two locale files disagree. Measured at HEAD by
diffing them key-by-key: twenty keys exist in both files with an en-US value
starting `!!!` and a ja-JP value that does not. One is the pre-existing
non-feature exception
`wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement`
(`source/renderer/app/i18n/locales/ja-JP.json:1420`), which carries the same
asymmetry on `develop`. The other nineteen are this feature's: eighteen
`governance.*` keys (seventeen `governance.drepDirectory.*` plus
`governance.tabs.directory`) and `sidebar.categoryTooltip.governance` — e.g.
`ja-JP.json:359` (`"DRepディレクトリ"`) against `en-US.json:359`
(`"!!!DRep Directory"`), and `ja-JP.json:633` against `en-US.json:633`. All
nineteen were introduced by the single slice-1 commit `0f47402b6`
(`git log -S` per key against `ja-JP.json`), and the same diff run against
`develop` returns only the one pre-existing key.

The gap is confined to slice-1, and that is what makes it invisible: of the 87
branch-introduced keys that carry `!!!` in en-US and exist in ja-JP, 68 are
marked in both files and 19 are marked in en-US only, so every later slice
minted its pairs correctly and no reviewer had reason to look. Nothing enforces
it either — no Jest guard compares the two files — so the governance strings
still to be minted (task-135 in cv-1, task-146 in cv-2, and anchor-2 copy) can
reopen the gap one key at a time with nothing failing.

**Resolution:** the invariant governs and the ja-JP side is the wrong side. The
first surface a Japanese user sees — directory title, status badges, voting
power, pagination, refresh/retry, empty and error states, the copy-ID control
and the Governance sidebar tooltip — currently reads as final copy and would
bypass the release-end `!!!` review entirely. Restoring markers is the inverse
of the operation that review owns, so it does not encroach on it
(`README.md:16`, `:18`).

**Tasked:** task-171 (cv-1) — restores the leading `!!!` on the nineteen keys
and adds the Jest guard: for every key present in both files whose en-US value
starts with `!!!`, the ja-JP value must too, with a documented allow-list
holding only the one pre-existing exception. Phase placement is load-bearing
rather than bookkeeping here — a guard landing after the mints it is meant to
protect protects nothing. The guard fires only where the en-US value is marked,
so once the release-end review strips an en-US marker the assertion is simply
vacuous for that key; no allow-list maintenance is created for that review.

## F-15 — `filterLogData`'s key list is keyed to the wire shape, so the renderer-side `votingTarget` / `currentVote` names it does not know are unredacted; task-170 does not close this

Found during the task-131 review's sanitization lens, raised as a candidate
blocker and refuted 3-0 *as a task-131 defect* — kept here because the gap
itself is real, unrecorded and outlives the task. `filterLogData`'s
`sensitiveData` array (`source/common/utils/logging.ts:24-49`) gained its
governance entries in slice-1 and they are all wire key names: `'drepId'`,
`'dRepId'`, `'vote'`, `'voting'` (`:45-48`), with the adjacent comment naming
exactly the wire paths they cover — `delegation.active.voting`,
`delegation.next[*].voting`, `certificates[*].vote`. Since task-130/131 there
is a second, renderer-side shape carrying the same secrets under different
names: a `Wallet` instance holds an own enumerable `votingTarget` (the
constructor is `Object.assign(this, data)`, `domains/Wallet.ts:175-177`, fed by
`WalletProps.votingTarget` at `:130` and `api.ts:3153`) and exposes it as
`currentVote`; inside the value the identity members are `drep`, `raw`,
`cip129`, `cip105` and `credentialHex`. None of those six names is in the list,
so `filterLogData({ wallet })`, `filterLogData({ votingTarget })` or
`filterLogData({ currentVote })` returns the CIP-129/CIP-105 id — or an
`abstain` / `no_confidence` sentinel — verbatim. The floor suite does not catch
it either: every `filterLogData` redaction case is wire-keyed
(`tests/jest/security/governance-sanitization.spec.ts:58-136`) and the file
contains no `votingTarget` or `currentVote` occurrence at all.

The gap is latent, not live, and that is precisely why it is worth recording.
A repo-wide grep at this commit shows `currentVote` and `isVoting` have zero
consumers outside their own definitions (`Wallet.ts:255`, `:260-261`) and
`votingTarget` appears only in the mapper (`api.ts:3082`, `:3087`, `:3091`,
`:3097`, `:3153`) and `Wallet.ts`, so nothing hands a domain `Wallet` to a
logger today — the two sinks that take one were already walked in the task-130
review and neither serializes the instance. The two computeds are prototype
accessors, so they never appear in `Object.keys` and cannot leak through the
redactor's own recursion; the exposure is the `votingTarget` own property plus
any hand-written `{ currentVote }` payload. cv-2 is the first slice that gives
`currentVote` a consumer, and therefore the first that can turn this from
latent to live.

**Resolution:** correctly not fixed in task-131. The exposure pre-dates that
diff (the property was already on every instance at HEAD via `WalletProps` plus
`Object.assign`, so the diff adds no redactor surface), the guide fences
task-131 to two files (`cv-1-implementation-guide.md:1227-1232`), and closing
it means touching `source/common/utils/logging.ts` plus the floor suite, which
this task must leave green and unmodified. Nor does task-170 close it: F-11's
fix wraps the `api.ts` wallet-list payloads, whose vote key is `voting` and is
already in the list — it is the same secret reached by the other name. The
durable action belongs with the cv-2 work that creates the first consumer, and
there are two acceptable discharges: add `votingTarget` and `currentVote` to
`sensitiveData` with a matching domain-shaped case in the floor suite, or hold
the stricter line that a domain `Wallet` never enters a logger payload at all
(which is what `designs/current-vote-display-design.md:114` already implies for
storage) and assert *that* instead. Not tasked here — cv-1 planning is closed
and no cv-1 row owns the domain-object shape; recorded so the reviewer of the
first cv-2 store/component that reads `currentVote` has the anchor.

## F-16 — `typed-scss-modules` is not a precondition for `tsc --noEmit`: the ambient `declare module '*.scss'` already covers scss imports, and because it types them `any` a green compile proves nothing about class names

Found during the task-132 build, the first cv-1 task to add a `.scss` module.
Three governing docs state the generation step as a requirement:
`cv-1-implementation-guide.md:98-100` ("New `.scss` modules (task-132) **need**
generated type declarations: `node_modules/.bin/typed-scss-modules
source/renderer/app`"), `cv-1-PRD.md:465` and `:482` (the R-4 /
Definition-of-Done compile fallback is "`tsc --noEmit`, plus
`typed-scss-modules` for new scss modules"), and `cv-1-code-review.md:85` (the
verification contract names it "for the task-132 scss module"). F-13 above
repeats it as part of how task-129 cleared the gate (`:386-390`).

Measured on this build, in a fresh worktree with zero generated typings:
`find source -name '*.scss.d.ts' | wc -l` → **0**, and `node_modules/.bin/tsc
--noEmit` → **exit 0 with zero `error TS` lines**, with
`CurrentVoteSummary.tsx:9` importing `./CurrentVoteSummary.scss`.
`typed-scss-modules` was never run. The mechanism is the global wildcard at
`declaration.d.ts:11-14` — `declare module '*.scss' { const content: any;
export default content; }` — which the guide's own task-132 verify block
already describes correctly at `:1791-1793`, contradicting its own `:98-100`.

The durable consequence is the second half, not the first. Under the wildcard
`styles` is `any`, so `styles.<anything>` typechecks: a green `tsc` is **not**
evidence that the class names used in the `.tsx` exist in the `.scss`. Nothing
else in the compile gate covers that either — `tsconfig.json` sets
`"strict": false` and `"noImplicitAny": false`. Generating the typings (output
gitignored, `.gitignore:141`) is what narrows `styles` to the real key union;
that shadowing is the standard resolution rule (a relative import that resolves
to a real `.d.ts` beats the ambient wildcard) but was NOT exercised in this
build. The other guard is the test transform: `jest.config.js:203` maps
`.scss` through `jest-css-modules-transform`, not `identity-obj-proxy`, so an
unknown key evaluates to `undefined` and React drops the attribute — a stale
class name should therefore surface as a missing `className` in task-134's
snapshots (inferred from the transform choice; not measured here).

**Resolution:** the live repo governs and the three "need"/"plus" recipes are
overstated — a missing or failing `typed-scss-modules` run is not a gate
failure and must not be recorded as a blocker, and a `tsc` error must be read
before reaching for it. Run it when you want the class-name check (that is its
only value here), not as a compile precondition. The docs are left unedited:
cv-1 planning is closed and each scribe's mandate is confined to its own task.
Carried forward unchanged from F-5: `nix fmt` still cannot run in this
devcontainer, task-132 substituted `node_modules/.bin/prettier --check` on its
three created files (clean), and the `nix fmt` pass remains owed pre-merge.

## F-17 — `DRepSourceLabel` returns `null` for any `source` it lacks a message for, and under this repo's `noImplicitAny: false` widening its union without adding the message compiles clean — so reusing it for the current-vote labels fails as a silent blank

Found during the task-132 build. The component is narrower than its name
suggests:
`source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx:18`
declares `export type DRepSourceLabelVariant = 'on-chain' |
'on-chain-anchor-reference';` (the prop at `:21`), and `:31-32` is a plain
object index followed by `const message = messageBySource[source]; if
(!message) return null;`. An unsupported value renders **nothing** — no throw,
no console warning, no failing assertion unless a test asserts the visible
text. The guide already forbids widening the contract in cv-1
(`cv-1-implementation-guide.md:1459-1463`, the D-4 "No English-literal
fallback" invariant); what no doc records is *how* a widening would fail.

Verified rather than assumed: with `tsconfig.json`'s `"noImplicitAny": false`,
a reduced probe (union extended with a third member, `messageBySource` left at
two entries, indexed the same way) produced **zero** diagnostics under
`tsc --noEmit --strict false --noImplicitAny false`; the identical probe under
`--noImplicitAny true` produced `error TS7053: Element implicitly has an 'any'
type because expression of type 'Variant' can't be used to index type '{
'on-chain': ...; 'on-chain-anchor-reference': ... }'`. So the compiler that
would have caught the mistake is switched off tree-wide. A second reason not to
widen it: its two message ids are directory/detail-namespaced
(`governance.drepDirectory.source.onChain` at `:6`,
`governance.drepDetail.sourceLabel.anchorReference` at `:11`), so voting-page
copy routed through it would be filed under DRep-directory ids.

**Resolution:** task-132's AC-6 anticipated exactly this and its conditional
fires — `governance-drep-discovery-plan-tasks.json:1011` ("If DRepSourceLabel
cannot localize the new abstain / noConfidence / delegatedToDRep labels with
its existing prop contract, wrap it in a CurrentVoteSummary-local renderer that
consumes react-intl directly. Do NOT silently fall back to English literals").
The shipped component reuses `DRepSourceLabel` only where its contract already
fits — `source="on-chain"` on the DRep state (`CurrentVoteSummary.tsx:65`,
satisfying AC-1 at `:1006`) — and renders `delegatedToDRep` / `abstain` /
`noConfidence` from its own message set (`:63`, `:88-90`). The durable rule for
downstream work: `DRepSourceLabel` is a two-variant *provenance* label and
nothing else. task-136's live status badge must not be routed through it
either, and anyone who does extend `DRepSourceLabelVariant` must add the
`messageBySource` entry in the same edit — the compiler will not remind them.

## F-18 — design §9.1's `drep` rendering rule is the combined cv-1+cv-2 card: built from the design instead of the guide it pulls in `GovernanceStore.drepIndex`, `givenName` and anchor links that neither task-132 nor task-136 may render

Found during the task-132 build while reconciling the component against its
design section. `designs/current-vote-display-design.md:185` prescribes, in one
undivided bullet, a "current delegation card with DRep name, source label,
compact id display, the DRep's live active / inactive / expiring status badge …
in-app details link, and anchor URL link", closing with "The status badge is
**required**". `:189` sources that badge from
`GovernanceStore.drepIndex[drepId]` and `:191-193` ("Anchor metadata display")
adds the CIP-119 `body.givenName` and the `target="_blank"` anchor URL link.
The only signal that this spans two slices is the pair of task tags in the
heading at `:172` (`task-132`, `task-136`); `:185` carries no split, and `:189`
/ `:191-193` carry no task tag at all. Only the props block at `:176-180` is
task-132-accurate — it matches `CurrentVoteSummary.tsx:11-14` exactly.

The guide inherits the ambiguity by reference: its task-132 Context cites
"current-vote-display-design.md:170-187" and "four render rules (design
:182-185)" (`cv-1-implementation-guide.md:1424-1427`) and then excludes most of
what `:185` says, in its own invariants block — no live badge and nothing
reading `GovernanceStore` / `drepIndex` / `DRepStatusBadge` (`:1439-1442`), and
no `givenName`, no anchor URL, no view-details link (`:1466-1468`). The tracker
is where the split is unambiguous: task-132 AC-3
(`governance-drep-discovery-plan-tasks.json:1008`) puts `givenName` and the
external anchor link behind anchor-1/anchor-2 and the in-app details link
behind slice-4 (task-116), and AC-5 (`:1010`) defers the badge to task-136 —
whose own criteria cover the drepIndex badge and its captions and say nothing
about anchor metadata.

**Resolution:** the guide and tracker govern for task-132 and the design is not
edited (same handling as F-3). The shipped component is deliberately disjoint
from the design's `drep` paragraph, which the build's boundary grep proves
negatively: `GovernanceStore|drepIndex|DRepStatusBadge|givenName|anchorUrl|
logger\.|console\.|analytics` over `CurrentVoteSummary.tsx` returns no match.
Two directions of error this protects against, both live for the next person to
open the file. Reading design-first, a reviewer flags the absent badge and name
as omissions rather than as the deferrals they are. Building design-first,
task-136 — whose job *is* to add the `:185` / `:189` badge — would also pick up
`:191-193`, rendering an anchor-derived `givenName` before the hardened anchor
pipeline exists, which is precisely the unverified-identity display that AC-3
and the guide's no-anchor-display invariant forbid.

## F-19 — react-intl's missing-message path is `console.error`, not a warning, and it fires in **en-US** as well as ja-JP because neither `StoryWrapper` nor `App.tsx` sets `defaultLocale` — so task-133's AC-1 is unsatisfiable until task-135 seeds the catalogs

Found during the task-133 build. Four governing passages predict react-intl
noise while the 12 `voting.governance.currentVote.*` keys are unseeded, and all
four call it a *warning* and frame it around the Japanese toggle:
`cv-1-implementation-guide.md:1998-2000` ("before task-135 lands, react-intl
logs missing-message warnings … re-check after task-135 for real ja-JP copy and
NO missing-message warnings"), `:2094` ("react-intl logs missing-message
console noise until then; it does not fail the run"), `:2690` and `:3155`; the
task-132 review restates it at `cv-1-code-review.md:1535`. Read at face value
that says: en-US is quiet today, Japanese is noisy, and the noise is a warning.
All three halves are wrong, and the first two are what make task-133's AC-1 —
"Four core knob values … render **without console errors**"
(`governance-drep-discovery-plan-tasks.json:1026`) — impossible to satisfy in
en-US either.

The mechanism, read out of the installed package (`react-intl` 2.9.0,
`node_modules/react-intl/lib/index.js`). `formatMessage` looks the id up at
`:813` (`var message = messages && messages[id];`). With no message it reaches
`:837`:

```js
if (!defaultMessage || locale && locale.toLowerCase() !== defaultLocale.toLowerCase()) {
  onError(createError('Missing message: "' + id + '" ...'));
}
```

The comment above that guard says it exists to prevent console noise "when no
`messages` are passed into the `<IntlProvider>` for the **default locale**". The
guard is therefore satisfied — and the error emitted — whenever the active
locale differs from `defaultLocale`, *even though a `defaultMessage` exists and
is about to be used as the fallback*. `defaultLocale` is not passed anywhere in
this repo, so it stays at the library default `'en'` (`:903`), while both
providers pass a **region-tagged** locale: `StoryWrapper.tsx:70-76` spreads
`{ locale, key: locale, messages: translations[locale] }` with `locale` drawn
from `LOCALES` — `'en-US' | 'ja-JP'` (`source/common/types/locales.types.ts:1-8`)
— and `App.tsx:76-82` does exactly the same in the production renderer.
`'en-us' !== 'en'`, so the branch fires for **every** missing key in **both**
locales. `onError` defaults to `defaultErrorHandler` (`:906`), which is
`console.error` outside production (`:523-527`). So it is an error, not a
warning, and en-US is not exempt.

Measured for this task: 12 `voting.governance.currentVote.*` keys in
`CurrentVoteSummary.messages.ts`, **0** in `source/renderer/app/i18n/locales/
en-US.json` and **0** in `ja-JP.json`. Every one of the four knob values
therefore emits `[React Intl] Missing message: "voting.governance.currentVote.…"
for locale: "en-US", using default message as fallback.` through
`console.error`, and the same again under `ja-JP`. Nothing in the story causes
it and nothing in the story can suppress it.

One nuance decides whether a future check sees the noise at all, and it is a
trap for the re-verification: the whole path is `NODE_ENV`-gated twice. At
`:818` `formatMessage` returns `message || defaultMessage || id` early whenever
there are no interpolation values **and** `NODE_ENV === 'production'`, and even
past that `defaultErrorHandler` is a no-op in production. `yarn storybook`
(`start-storybook`, package.json:55) serves a development bundle and shows the
errors; a production static bundle would show none. So AC-1 must be re-verified
in the **dev** Storybook — checking it against a served `yarn storybook:build`
output would report a false green. (Code-path reasoning plus the two script
definitions; not measured end to end here, because `storybook:build` aborts on
the unrelated manager-side loader gap recorded in the task-133 review.)

**Resolution:** not a defect in task-133 and explicitly not fixable there. The
three "fixes" available at story level are all wrong: seeding the keys is
task-135's row, inlining literals breaks D-4 and invariant 11, and adding
`defaultLocale` to `StoryWrapper` would silence the en-US half of a real
signal — every genuinely missing en-US key across the whole app — while leaving
ja-JP untouched, and belongs to nobody's current row. The dependency chain is
already correct in the tracker and is the thing to follow: **task-171** (restore
the `!!!` markers and add the guard, `dependencies: []`) → **task-135** (seed
the 12 keys in both catalogs, `dependencies: ["task-132","task-171"]`) →
task-133's AC-1 becomes checkable. Until then AC-1's console-error clause is
recorded as owed in the task-133 row and must never be reported green. Two
consequences for adjacent work. task-134's snapshot spec renders with `messages`
from `en-US.json` and will emit the same `console.error` per key — the guide's
`:2091-2094` note is right that the `!!!` `defaultMessage` fallback keeps the
snapshots stable across the task boundary, but a spec that fails on console
output would break on it. And after task-135 the correct assertion is
zero missing-message output **in both locales**, not just Japanese.

---

## F-20 — `yarn storybook:build` has been red at HEAD since before cv-1 (the manager webpack has no JSX loader), but `yarn storybook` serves fine and its preview compile is the automated floor that actually works

Measured, not inferred, while closing task-133. A second worktree was checked out
detached at the pre-task commit `23f443b76` with no changes at all and
`node_modules` symlinked from the main checkout; `yarn storybook:build` there
exits 1 with the identical failure seen on the task branch —
`=> Failed to build the manager`, `Module parse failed: Unexpected token (12:18)`,
pointing at `storybook/addons/DaedalusMenu/register.tsx:12`
(`render: () => <DaedalusMenu api={api} />`). The break predates every cv-1 row
and is unreachable from any story file.

The cause is a module-graph split in `storybook/main.ts`. `:8` puts
`../storybook/stories/index.ts` under `stories:` (the **preview** graph) while
`:13` puts `require.resolve('./addons/DaedalusMenu/register.tsx')` under
`addons:` (the **manager** graph). The `swc-loader` rule for `/\.tsx?$/` is
registered at `:71`, inside the `webpackFinal` hook opened at `:16` — and in
Storybook 6.4 `webpackFinal` configures the preview only, never the manager. So
the manager webpack genuinely has no loader for that addon's JSX.

The two runners diverge on how fatal that is, and the difference decides which
command is a usable gate:

- `build-storybook` (`package.json:56`) treats the manager failure as fatal and
  exits 1 **before** the preview result is reported — so it yields no evidence
  about stories at all, pass or fail.
- `start-storybook` (`package.json:55`) does not. Measured at clean `23f443b76`:
  `manager (webpack 5.72.0) compiled with 1 error`, yet the server still came up
  (`Storybook 6.4.22 for React started`, `Local: http://localhost:6006/`) and
  `webpack built preview` succeeded. Re-run on the task branch with the new
  story present, the preview built clean in 35.9 s with zero `ERROR in`,
  `Module not found` or `Failed to compile` lines.

**Resolution:** three consequences, none fixable inside cv-1. First, the
automated floor nominated for Storybook rows in the cv-1 guide
(`:1990-1993`) — `yarn storybook:build` compiling — cannot ever be green here;
the substitute that carries real signal is a `yarn storybook` run whose preview
compile is clean, because the preview graph is the one stories live in. Task-133
was closed on that basis and its story is confirmed to link into the live
preview bundle, not merely to transpile. Second, `yarn check:all`
(`package.json:17`) chains `yarn storybook:build` last and is therefore red at
HEAD for reasons unrelated to any governance work — do not read a `check:all`
failure as a regression without first checking whether the only error is the
manager one. Third, the owed AC-2 visual pass is **not** blocked by this: the
dev server serves, so the en-US/ja-JP toggle pass can be performed in the main
checkout as soon as a browser is available (after task-135, per F-19). Repairing
the manager loader means moving the `.tsx` rule out of `webpackFinal` into
`managerWebpack`, which is repo-infrastructure work outside this plan.

---

## F-21 — the guide's task-134 Step-1 block carries the never-logged assertion on only two of the four accepted-target cases tracker AC-7 names, and its prose and its own acceptance bullet repeat the narrow reading (tracker authoritative; code correct, guide stale in three places)

Found during the task-134 build, and the only conflict in that build that
changed what was typed.

**What the tracker demands.** AC-7 of the task-134 row, verbatim at
`governance-drep-discovery-plan-tasks.json:1061`: "The accepted-target mapper
cases (voting-only DRep, delegating_and_voting, abstain, no_confidence) assert
`expect(mockedWarn).not.toHaveBeenCalled()`, pinning the never-logs floor on
the accepted-id path and not only on the rejection paths." Four cases, each
named individually.

**What the guide supplies.** The Step-1 code block
(`cv-1-implementation-guide.md:2099-2279`) carries the assertion on two of
them: `:2173` closes "maps a voting-only DRep wallet: votingTarget populated,
pool id null" (`:2156`) and `:2193` closes "maps delegating_and_voting: pool
target AND votingTarget populated" (`:2176`). The abstain case (`:2196-2202`)
and the no_confidence case (`:2204-2210`) both end at their `toEqual` and never
mention `mockedWarn`. The block's third occurrence, `:2230`, closes "yields
votingTarget null for status voting without active.voting and never parses
active.target" (`:2221`) — a case whose target is *rejected*, not accepted, so
it is not one of AC-7's four.

**Root cause, and it is exact.** Commit `2ee5f74cf` ("docs(gov): fold findings
audit into tracker rows and plan docs", 2026-07-27) is the commit that wrote
AC-7 into the tracker and the assertions into the guide, and its guide diff
contains exactly two `+    expect(mockedWarn).not.toHaveBeenCalled();` hunks —
one after the CIP-129 `source: 'onchain'` expectation, one after the
delegating_and_voting one — where four were owed.
`git show 2ee5f74cf^:…/cv-1-implementation-guide.md | grep -c
"not.toHaveBeenCalled"` returns `1`, the pre-existing `:2230` line, so the
commit took the guide from one occurrence to three and never to five. Two
single-line hunks were dropped in the same edit that made them mandatory.

**Two further places carry the narrow reading.** Beyond the code block, the
guide prose at `:2511-2512` reads "Together with the two
`expect(mockedWarn).not.toHaveBeenCalled()` assertions in the Step-1 valid-DRep
cases", and the task-134 acceptance checklist at `:2546-2547` reads "The
valid-DRep mapper cases assert `mockedWarn` was never called, so the no-logging
floor is pinned on the accepted-id path too (Step 1)". Both sentences are true
of two cases and silent about the two sentinels. A future reader working from
the guide alone — transcribing the block, then ticking the guide's own
checklist — under-implements AC-7 and is told by the document that the work is
done. That is why this is worth a finding rather than a nit: the guide's
checklist agrees with the guide's too-narrow block, so nothing inside the guide
can catch the omission.

**Resolution:** the tracker wins, per this file's standing resolution rule and
F-1's precedent, and the build followed it. Both lines were added, so the
assertion now sits at `tests/jest/api/createWalletFromServerData.spec.ts:70`,
`:90`, `:99` and `:108` — AC-7's four accepted-target cases — plus `:129` for
the rejected-target case the guide already had. It is a strengthening, not a
behaviour change, and it cost nothing at the gate: the task-134 verifier
measured `tests/jest/api` at 3 suites / 18 tests passed, exit 0, and code review
approved with the delta recorded as guide drift, not code drift. The code is
right; the guide is stale in three places, and this is the reproducible kind of
staleness.

**Tasked:** unowned. No pending row's acceptance criteria name these lines, and
the task-134 row deliberately did not edit them — its `statusReason` records the
drift and says the fix was "left for a later guide touch-up rather than edited
from this row". A doc-repair pass owns three edits, all in
`cv-1-implementation-guide.md`: append
`expect(mockedWarn).not.toHaveBeenCalled();` to the abstain and no_confidence
cases in the Step-1 block (`:2196-2210`); change "the two … assertions in the
Step-1 valid-DRep cases" at `:2511-2512` to the four accepted-target cases; and
rewrite the acceptance bullet at `:2546-2547` to AC-7's own wording. Until that
lands, `governance-drep-discovery-plan-tasks.json:1061` is the only correct
statement of AC-7 and the guide must not be read as an independent authority on
it.

## F-22 — the findings-audit review entry says the cv-1 PRD and guide "are not rewritten by this entry"; the commit carrying that entry rewrote the guide by 531 lines and the PRD by 109, and created task-134's Step 5 outright (record-only)

**The claim.** `cv-1-code-review.md:1134` opens the entry
"## Planner: 2026-07-27 — findings audit outcome (six new rows, nine
amendments)", and at `:1141-1143` that entry states: "the cv-1 PRD and
implementation guide are not rewritten by this entry — the guide lines that
change are named inside the amendments' own acceptance criteria."

**The diff.** The entry landed as commit `2ee5f74cf`, whose `git show --stat`
reads in part `task-plans/cv-1-implementation-guide.md | 531 ++++`,
`task-plans/cv-1-PRD.md | 109 ++++`, `task-plans/cv-1-code-review.md | 104 ++++`
(the entry itself), `governance-drep-discovery-plan-tasks.json | 245 ++++`, and
totals "18 files changed, 1289 insertions(+), 111 deletions(-)". The guide is
the largest single edit in the commit that says the guide was not rewritten.

**Why this is worth recording rather than shrugging at.** The rewrite is not
cosmetic and it lands inside task-134. Before the commit the task-134 section
began at `:1957` and ran four content steps, closing with `#### Step 5: Verify`
at `:2420`. On disk today it begins at `:2017` and runs five content steps, with
`#### Step 5: Extend the task-129 normalizer spec` at `:2488` and
`#### Step 6: Verify` at `:2516`. That new Step 5 is the whole of AC-5 — the
checksum-valid wrong-length `drep_vkh` vector that reaches the CIP-105 length
guard and closes `normalizeDRepIdentity.ts`'s uncovered line 48 — and it exists
only because of the entry that says nothing in the guide changed. A reviewer
taking the sentence at face value would conclude the guide has no
normalizer-extension step and that AC-5 is unguided, and would re-plan or defer
work that is already written.

**A second, quieter consequence: rotted line anchors.** The same commit's +109
lines to the PRD moved risk R-4 from `cv-1-PRD.md:407` (verified by
`git show 2ee5f74cf^:…/cv-1-PRD.md | grep -n "R-4 (low)"`) to `:462-466` on
disk. F-13 cites the old anchor at `cv-1-findings.md:383` ("`cv-1-PRD.md:
407-411`") and the task-134 tracker `statusReason` repeats it. The R-4 text is
unchanged and still findable by `grep -n "R-4 (low)"`; only the numbers rotted.

**Resolution:** record-only, and it resolves in the safe direction — the guide
on disk is current and complete for task-134, which is why the build proceeded
from it without harm. Two durable rules come out of it. First, a log entry's own
statement of its blast radius is not evidence about the tree; `git show --stat`
on the commit that carried the entry is, and it takes one command. Second, when
a doc commit inserts lines, re-`grep` any anchor into that doc rather than
trusting a citation written before the insert — every anchor in this findings
file that predates `2ee5f74cf` and points into `cv-1-PRD.md` or
`cv-1-implementation-guide.md` is suspect by construction, not by suspicion.

**Tasked:** no owner and no row. The code-review log is append-only history, so
the `:1141-1143` sentence stays as written and this finding is the correction of
record. The stale `cv-1-PRD.md:407-411` anchor in F-13 (`:383`) is likewise left
in place — findings are not rewritten — and reads as `:462-466` today.

## F-23 — refreshed cv-1 whole-tree baseline (85 suites / 1071 tests / 6 snapshots) and PRD R-4 closed rather than flagged: `yarn compile` itself is green on Node v24.16.0 across five cv-1 rows, and its `precompile` hook already IS the documented fallback

**The new baseline.** Measured by the task-134 verifier with the unfiltered
runner F-13 mandates (`node_modules/.bin/jest --runInBand --coverage=false`, no
path argument), verbatim:

```
Test Suites: 1 skipped, 84 passed, 84 of 85 total
Tests:       12 skipped, 1059 passed, 1071 total
Snapshots:   6 passed, 6 total
```

exit 0 in 42.02 s. This supersedes F-13's "82 suites and 1050 tests (1038
passed, 12 skipped, 2 snapshots)" (`cv-1-findings.md:376-379`) as the figure
every cv-1 row after task-134 must clear.

**The delta reconciles to the last test, which is what makes it usable as a
baseline rather than just a number.** +3 suites are exactly the three files
task-134 created (`tests/jest/api/createWalletFromServerData.spec.ts`,
`tests/jest/api/walletVotingComputeds.spec.ts`,
`source/renderer/app/components/voting/voting-governance/
CurrentVoteSummary.spec.tsx`). +21 tests are 9 + 7 + 4 + 1: nine mapper cases,
seven Wallet-computed cases, four snapshot cases, and the ninth
`normalizeDRepIdentity` case added in place to the task-129 spec. +4 snapshots
are `CurrentVoteSummary`'s four. The skip count is unchanged at 12, so nothing
was silently disabled to buy a green. One number differs from the pre-run
arithmetic and it is an overshoot, never a shortfall: the run brief predicted
eight mapper cases and +20 overall, while the guide's Step-1 block prescribes
nine `it()` blocks (`cv-1-implementation-guide.md:2156`, `:2176`, `:2196`,
`:2204`, `:2212`, `:2221`, `:2233`, `:2248`, `:2263`) and the delivered file
carries those nine one-for-one.

**Lint and the focused runs, for completeness.** `yarn lint` exit 0 at exactly
5591 warnings — zero delta from the standing baseline, no warning attributable
to any added file. Focused: `tests/jest/api` 3 suites / 18 tests;
`tests/jest/governance/normalizeDRepIdentity.spec.ts` 1 suite / 9 tests, its
coverage row now `100 | 100 | 100 | 100` with an **empty** "Uncovered Line #s"
column where the baseline showed `48`; `CurrentVoteSummary.spec.tsx` 1 suite /
4 tests / 4 snapshots; the sanitization floor
`tests/jest/security/governance-sanitization.spec.ts` unchanged at 23 of 23.

**R-4 is not merely stale; it is closeable.** `cv-1-PRD.md:462-466` (moved from
`:407`, see F-22) still rates "Node v24 gate flakiness" an open low risk because
"`yarn compile` has previously failed for environment reasons under Node
v24.16.0", and the guide propagates that framing in a heading and a bullet
(`cv-1-implementation-guide.md:91`, `:93-97`) plus ten byte-identical copies of
the inline comment `yarn compile   # Node v24 fallback: node_modules/.bin/tsc
--noEmit` (`:564`, `:690`, `:955`, `:1189`, `:1370`, `:1790`, `:1984`, `:2519`,
`:2910`, `:3070`) and a reworded eleventh in the cross-cutting block (`:3111`).
Two things now stand against it. First, evidence: the
task-134 verifier ran `yarn compile` — the wrapper script, not the fallback —
to exit 0 with zero diagnostics, "Done in 29.61s.", and a scan of every
`statusReason` in the tracker finds `yarn compile exit 0` recorded for task-127,
task-128, task-129, task-133 and task-134, with **no** cv-1 row recording a
compile failure of any kind. F-13's two-task count (`:389-390`) understated it
because it was measuring `node_modules/.bin/tsc --noEmit` directly rather than
the script. Second, mechanism: the "fallback" and the gate are the same two
commands in the same order. `package.json:46` defines
`"precompile": "yarn typedef:sass"` and `:73` defines
`"typedef:sass": "typed-scss-modules source/renderer/app"`, which yarn runs
before `:45`'s `"compile": "tsc --noEmit"` — so `yarn compile` already performs
"`node_modules/.bin/tsc --noEmit`, plus `typed-scss-modules`", the exact
fallback PRD `:481-483` prescribes. The task-133 row states the same mechanism
from observation: "the precompile typedef:sass hook regenerated the 316
gitignored *.scss.d.ts this fresh worktree lacked, then tsc --noEmit reported
zero diagnostics".

**Resolution:** adopt 85 suites / 1071 tests / 6 snapshots (12 tests skipped,
1 suite skipped) as the cv-1 baseline from task-135 onward, and report measured
counts in each `statusReason` rather than re-quoting a constant — the constant
is what went stale here in the first place. Treat R-4 as closed, not as a
standing caveat: a row that hits a `yarn compile` failure should capture the
error and open a fresh finding, exactly as F-13 already directed, but no row
should pre-announce the fallback as expected. Keeping the fallback comment is
harmless; keeping the *risk* open is not, because it invites a green
`tsc --noEmit` to be reported in place of an unrun `yarn compile`, which is a
weaker gate by one command (the scss typings regeneration).

**Tasked:** unowned, like F-21's and F-22's doc repairs. The concrete edits are
three: retire R-4 at `cv-1-PRD.md:462-466` (and the "Node v24 fallback per R-4"
clause at `:481-483`); correct the three surviving "all 82 suites stay green"
comments that sit in still-pending sections —
`cv-1-implementation-guide.md:2913` (task-170), `:3077` (task-171) and `:3114`
(Cross-Cutting Acceptance) — noting that the preamble figure at `:110` ("the
unfiltered tree is 82 suites, `tests/jest` selects 7 of them") is a dated
HEAD-of-cv-1 measurement and may stand as history; and leave `:1376` (task-131)
and `:1800` (task-132) alone as the historical record of completed rows, per
F-13's precedent for the filtered-recipe lines.
