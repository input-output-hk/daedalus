# UX-Refinement Code Review Log

## Planner: 2026-07-23 — ux-refinement planning complete (status: in_review)

**Scope planned:** the full `ux-refinement` phase (task-159 … task-169, 11 tasks;
task-166 already `partial` with a locked `manual_execution` remainder), implementing
the four ratified plan Key Decisions: soft-warning sync banner + `noSync` fallback +
clear-on-sync refetch (159/160), two-phase registration/stake load with a second IPC
channel and store-side merge (161/162), per-phase CLI timeout budgets (163), and the
documented ID-only v1 limitation (165) — plus the phase's i18n copy (164),
consolidated behavior Jest (167), the DRep-state snapshot support-log file (168), and
the CLI robustness pass: structured era-retry signal + real-binary argv smoke test
(169).

**Deliverables:** the PRD was authored FIRST
([ux-refinement-PRD.md](./ux-refinement-PRD.md), 2026-07-23 — Per-Task Contract,
PD-1 … PD-12, FR-1 … FR-17, NFR-1 … NFR-8), then the implementation guide in two
parts against the same verified anchor base (`wt/ux-refinement`, base `a463c31d0`):
PART 1 = Steps 1-6 (task-159 … task-164) with cross-cutting notes and pinned IPC
contract; PART 2 = Steps 7-13 (task-165 … task-169, full verification matrix,
tracker/docs close-out). Every pre-edit anchor was spot-checked against the live
worktree on 2026-07-23.

**Binding decisions carried through (resolved during planning — not to be
re-litigated in review):**

1. **PD-1 — build only this phase's variants.** `DRepEmptyState` and
   `DRepErrorBanner` do not exist in the live tree (grep-verified; the tracker's
   "existing"/"already-designed" wording is aspirational). They are created at the
   design's component seams (`drep-discovery-design.md:184-185`, live `_shared/`
   naming per PD-2) with variant-union props ready for future slices, implementing
   only `noSync` (Step 2) and `rankingUnavailable` (Step 4); `noResults`/`selfnode`
   and the refresh-failed banner stay with their owning slices.
2. **PD-3 — two-phase IPC = one new request/response channel, store-sequenced.**
   `GOVERNANCE_DREP_LIST_CHANNEL` stays Phase 1 (registrations, `votingPower: null`);
   new `GOVERNANCE_DREP_STAKE_CHANNEL` carries
   `DRepStakeQueryPayload { stakeByDRepId: Record<DRepId, Lovelace>; fetchedAt }`
   (decimal-string lovelace keyed by the CIP-129 id derived through the same
   `cip129FromCredential` path as the list payload); `GovernanceStore` sequences the
   two requests and merges by `drepId` string equality. No streaming/push additions,
   no second delegation backend.
3. **PD-4 — task-162 Reshuffle AC scoped to Manual Refresh.** Reshuffle/seed are
   slice-5 task-118 territory and do not exist; the phase fully satisfies "Manual
   Refresh re-runs both phases", and the Reshuffle half is forward-compatibility
   only, with the exact truthful `statusReason` wording pinned in guide Step 13a.
4. **task-166 — autonomous portion only.** The guide's Step 8 records/extends the
   deferred follow-up note (plan Risks row `:345` names all three remaining manual
   items); the mainnet capture + p50/p95 latency measurement stay
   `manual_execution`, must not be attempted, and the task's status stays `partial`.
5. **task-169 PART B self-skips.** The real-binary argv smoke test gates on
   `cardano-cli` being on PATH (`describe.skip` otherwise); it is absent in this
   devcontainer, so the suite must report **skipped, never failed** — recorded as
   verification debt (Nix-shell positive run) in the PRD and Step 13.
6. **PD-7 — per-phase timeouts threaded per call.** `CLI_TIMEOUT_MS` is replaced by
   `REGISTRATION_TIMEOUT_MS = 10_000` / `STAKE_TIMEOUT_MS = 30_000` passed into
   `_runCliQueryWithEraFallback` → `_runCliQuery` as a parameter (the timeout lives
   in the shared spawn helper, so a static cannot express two budgets); the 30 s
   value stays provisional pending the task-166 manual measurement.

**Invariant posture:** local-first (no external calls anywhere); sanitization floor
held at 20/20 with `{ errorType }`-only phase-failure logging, and task-168's
snapshot file planned as the ONE documented `filterLogData` bypass (public directory
payload only, structurally no user vote/delegation, boundary documented as tokens
§12); lovelace losslessness json-bigint → decimal-string IPC → BigNumber, never
`Number`, never silent 0; CLI discipline (bulk `--all-dreps` per phase, network flag
from node config appended after the subcommand, socket via `spawn.env`, era
`latest`→`conway` fallback on every query — hardened by task-169's structured
`UsageError` signal per PD-8).

**Known environment constraints baked into the plan:** no `nix` (prettier scoped to
changed `.ts/.tsx` substitutes for `nix fmt`), no `gh`/push credentials (all commits
local to `wt/ux-refinement`), `yarn compile` unreliable under Node v24 (tsc direct),
`yarn i18n:manage` previously unexercised here (honest-report + manual-grep fallback
pinned in Step 6c).

**Status:** planning complete; PRD + two-part guide + this log submitted for
critique. Next expected entry: Critiquer review of the PRD/guide pair.

## Critiquer: 2026-07-23 — review of ux-refinement planning

**Scope reviewed:** `ux-refinement-PRD.md` + `ux-refinement-implementation-guide.md`
(both parts, Steps 1-13) against the 11 tracker task entries (acceptance criteria +
dependency edges), the 14 locked invariants in `prompt.md`, the design contracts
(`shared-design-tokens.md` §1/§3/§6/§9, `drep-discovery-design.md` component map +
state table), `research/ux-refinement-sync-and-load-research.md`, and the grounding
brief — one broad pass with live-code anchor spot-checks.

**Anchor spot-check (sample, all verified exact against the worktree):**
`GovernanceQueryService.ts` (`:52` CLI_TIMEOUT_MS, `:63-64`, `:131-137`, `:139`,
`:171`, `:187-196` include-stake Promise.all, `:223-236`, `:238-254` substring gate,
`:261`, `:263-271`, `:323-331`, `:333-346`, `:380-394`, `:450-454`, `:476-511`);
`GovernanceStore.ts` (`:29-35`, `:37-41`, `:59-60`, `:84-86`, `:114`, `:125`,
`:143-145` FP-10, `:153-161`); `DRepDirectory.tsx` (`:48-56`, `:58-66`, `:67-68`,
`:80`, `:132-137`, `:147-156`); `DRepDirectoryPage.tsx` (`:20-34`, `:46-61`,
`@inject('stores')`); `DRepCard.tsx` (`:12-23`, `:25-29`, `:31-42`, `:44`,
`:55-57`, the `Voting power:` colon the Step-9a assertions depend on);
`DRepDirectoryList.tsx` (`:6`, `:29-35`, `:65-71`); spec/story helpers
(`DRepDirectory.spec.tsx:39-65`, 12 tests; stories `:5`, `:25`, `:123-146`,
`:305-311`); `governance.types.ts` (`:14`, `:47`, `:87-94`, enum `:98-106`);
`api.ts` (`:85`, `:657-659`); both `governanceChannel.ts` files;
`GovernanceQueryService.spec.ts` (26 tests, 28 `service.fetchDRepList()`
occurrences, `:34`, `:68-69`, `:71-81`, `:195-225`, `:254-267`, `:296-306`, `:547`,
`:564`, `createNeverClosingChildProcess:133`, `setNetwork('mainnet')` in
beforeEach); `GovernanceStore.spec.ts` (8 tests, `:10-12`, `:14`, store constructed
`new GovernanceStore({} as any, {} as any, {} as any)`); `setupLogging.ts` (`:6`,
`:13-19`, `:120`, `:169-176`, `constructMessageBody` applies no filtering at
`common/utils/logging.ts:86-112`); `config.ts:137-145`; `get-logs.ts` ALLOWED_LOGS[0]
+ `isFileAllowed` name-membership; `environment.ts` network/os/platformVersion/version;
`en-US.json:284-302` (byte-identical to the guide quote) + `ja-JP.json:284`;
`NetworkStatusStore.ts:96/:119/:608-611`; `stores/index.ts:69`;
`declaration.d.ts:1` global `*.scss`; `VotingGovernancePage.spec.tsx` (`:26`,
`:113-119`, `:124`, 7 tests); floor suite 20 tests; plan `:161-165` + `:345`;
design seam `:236-238`; tokens §6 already two-phase (89-104), §9 ids/copy exact.
The unused wrong-shaped stake mock (bech32-keyed `{ stake }`) confirmed — PD-6's
rewrite is correct; renderer logger is a named-export object so the Step-9b spy works.

**Task/AC coverage:** every AC of tasks 159-169 maps to a concrete step and a named
proof (guide Step-12 AC↔proof table); the two ACs not fully provable here (168 AC-2
bundle appearance, 169 PART B positive run) are honestly declared verification debt
deferred to task-125 / the Nix shell, not silently claimed. task-162's Reshuffle AC
half is correctly scoped via PD-4 with the pinned statusReason wording. task-163 AC-2
is satisfied by confirmation because §6 is already two-phase (verified). Dependency
serialization 159→…→169 honors every tracker edge; all external deps point at
task-103 (`complete`).

**Invariants:** #1/#2/#5/#6/#11/#13/#14 are inlined into the steps with the exact
mechanics (index-only parse errors, `{ errorType }`-only phase catches, sentinel
skip, storeAsString → decimal-string → BigNumber, per-phase bulk `--all-dreps`,
network flag from `setNetwork` appended after subcommand, socket via `spawn.env`,
`!!!` on all 10 locale strings). The task-168 snapshot is handled strictly as the
one documented `filterLogData` bypass with a structural payload-type guard, §12 doc,
and a floor re-run. No invariant conflicts found.

**Manual boundary:** the only manual item is the task-166 remainder, guarded loudly
in Step 8 (do-not-attempt, status stays `partial`). No hidden manual checkpoint is
mislabeled autonomous. Step-13a status vocabulary (`complete`, never `verified`)
matches the prompt status rule.

**Small-model bar:** met. Verbatim pre/post quotes for every edit, full file
contents for every CREATE, exhaustive touched-file tables per part, exact
verification commands with expected counts — and the arithmetic checks out
(26−1+9=34 → +1=35 → +3=38 service; 12+7=19 component; 8+5=13 store; 3 container;
7 flow; 4 snapshot; 12 skipped smoke; floor 20/20). No judgment calls are left to
the implementer; PD-1…PD-12 pre-resolve the ambiguities the tracker wording
("existing DRepEmptyState", "already-designed banner") would otherwise create.

**Blockers:** none.

**Notes (non-blocking, no fix pass required):**
1. Renderer `governanceChannel.ts` is 11 lines, not "12 lines pre-edit" (Step 3e);
   harmless — the step is a full replacement.
2. The comment at `GovernanceQueryService.spec.ts:519` ("fetchDRepList should emit
   SocketUnavailable") survives the scoped call rename in Step 3g item 2 as a stale
   name; cosmetic only.
3. Step 2e's reaction expression `stores?.networkStatus.isNodeInSync` optional-chains
   `stores` but not `networkStatus`; safe in practice (`networkStatus` is always in
   `StoresMap`, and the Step-1b render guard bails first), but
   `stores?.networkStatus?.isNodeInSync` would be belt-and-braces.
4. Step 3f: the mock's `23137980123456` is below `Number.MAX_SAFE_INTEGER`, so
   json-bigint yields a JS number for it (only `9007199254740993` becomes a string);
   the parser's accept-string-or-number branch already covers this — no change needed.

**Decision: approved**

## Code Review: task-159 — round 1 (2026-07-23)

**Scope reviewed:** the uncommitted diff (4 files: `DRepDirectory.tsx`,
`DRepDirectoryPage.tsx`, `DRepDirectory.spec.tsx`, `DRepDirectory.stories.tsx`)
against guide Step 1 (1a-1e) and PRD FR-1 / task-159 contract row.

**Findings:**

1. **Guide conformance — exact.** All four edits match the Step-1 post-edit quotes
   verbatim: `isNodeInSync: boolean` / `syncProgress: number | null` added to the
   `Props` interface (`DRepDirectory.tsx:53-54`) and deliberately NOT destructured
   in the component body (grep confirms only the two interface lines — the
   lint-trap avoidance the guide mandates); container reads
   `stores?.networkStatus`, guards `!governanceStore || !networkStatus`, and passes
   both observables in `render()` (`DRepDirectoryPage.tsx:49-60`); spec helper
   gains `isNodeInSync = true` / `syncProgress = 100` defaults with matching
   optional types; stories gain `DirectorySyncState` + `DEFAULT_SYNC_STATE` and
   thread `syncState` through both render helpers.
2. **Store boundary (AC-2 / FR-1) held.**
   `grep -n "NetworkStatus" source/renderer/app/stores/GovernanceStore.ts` prints
   nothing (exit 1). Container remains the integration point per research R1.
3. **Scope clean.** `git status --short` shows exactly the four Step-1 files;
   nothing else touched. No banner rendering (task-160 territory), no store
   changes, no new tests (PD-11 — task-167 owns them), no i18n/IPC/CLI changes,
   so the sanitization floor, local-first, lovelace, and CLI invariants are
   untouched by construction — and the floor suite was re-run anyway (below).
4. **Incidental reformat (note, not a finding):** `componentDidMount`'s
   `this.props.stores\n  ?.governance` line-break shift in `DRepDirectoryPage.tsx`
   is prettier 2.1.2's own output from the mandated Step-1e format pass;
   semantically identical.
5. **Pre-existing lint warnings (note):** eslint on the four files reports
   0 errors / 7 warnings (`observer`/`inject` decorator false positives at
   `DRepDirectoryPage.tsx:2`, `drepId` function-type-param at
   `DRepDirectory.tsx:56`); all attach to lines the diff did not introduce.

**Verification (re-run by reviewer, not taken on faith):**

- `node_modules/.bin/tsc --noEmit` → exit 0 (yarn compile unreliable under
  Node v24 per standing note; tsc run directly).
- `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
  → 12/12 pass (matches the guide's expected count; defaults keep behavior
  unchanged).
- `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → 20/20
  pass (floor intact).
- `node_modules/.bin/eslint <4 changed files>` → 0 errors.
- `node_modules/.bin/prettier --check <4 changed files>` → all clean (the bare
  `yarn prettier --check` script globs the whole repo and trips on pre-existing
  drift in untouched files — not attributable to this task).
- `NetworkStatusStore.ts:96/:119/:608-611` and `stores/index.ts:69` re-verified:
  prop types `boolean` / `number | null` are faithful to the observables.

**Blockers:** none.

**Decision: approved**

## Code Review: task-160 — round 1 (2026-07-23)

**Scope reviewed:** the uncommitted diff (4 edits: `DRepDirectory.tsx`,
`DRepDirectory.scss`, `DRepDirectoryPage.tsx`, `DRepDirectory.stories.tsx`; 2
creates: `_shared/DRepEmptyState.tsx`, `_shared/DRepEmptyState.scss`) against guide
Step 2 (2a-2f) and PRD FR-2/FR-3/FR-4 + the task-160 contract row.

**Findings:**

1. **Guide conformance — exact.** All six files match the Step-2 post-edit quotes
   and full-file CREATE contents verbatim: syncing banner (icon + text + live `%`,
   `role="status"`, `aria-hidden` SVG, `--badge-warning-*` token slots with the
   `DRepStatusBadge`-pattern fallbacks, `Math.floor(syncProgress ?? 0)` pinning the
   fractional/null cases); `DRepEmptyState` ships only the `noSync` variant (PD-1)
   with the canonical `!!!`-prefixed §9 copy; `showNoSyncFallback` predicate covers
   Loaded-empty and Failed-non-selfnode exactly as specified, placed after the
   Loading case so the spinner and retained-list behaviors are preserved; container
   `reaction` on `stores?.networkStatus.isNodeInSync` refetches on the false→true
   edge and is disposed in `componentWillUnmount`; both `Node syncing` stories use
   the task-159 `syncState` override arg with the ranged `number` knob and the
   global locale toggle (no local IntlProvider).
2. **FR coverage.** FR-2 (persistent banner, no dismiss control), FR-3 (`noSync`
   fallback replacing the empty/error branch while `!isNodeInSync`), FR-4
   (refetch-once reaction + disposal) all present. New behavior tests are
   deliberately deferred to task-167 per PD-11/Step 2g — not a gap in this round.
3. **Invariants held by construction.** No IPC/store/CLI/logging changes, so
   local-first, lovelace, CLI discipline, and the IPC contract are untouched;
   both new strings carry `!!!` (i18n locale keys land in task-164 per the guide —
   `yarn i18n:manage` correctly not run this step); no status vocabulary additions;
   floor suite re-run green (below). The generated `DRepEmptyState.scss.d.ts` from
   the compile pass is gitignored (`.gitignore:141`), consistent with PD-12 (global
   `*.scss` declaration at `source/renderer/declaration.d.ts:1` covers tsc).
4. **Scope clean.** `git status --short` shows exactly the six Step-2 files —
   matching the guide's file table rows 1/2/4/5/6/7. (The close-out section's
   "seven Step-2 files" at guide `:4020` is a miscount in the guide, not a diff
   problem: `DRepDirectory.spec.tsx` belongs to Steps 1/4 only.)
5. **Pre-existing lint warnings (note):** eslint on the four ts/tsx files reports
   0 errors / 5 warnings — the known `observer`/`inject` decorator false positives
   (`DRepDirectoryPage.tsx:2`) and the `drepId` function-type-param
   (`DRepDirectory.tsx:64`); none introduced by this diff.
6. **Optional-chain note (carried from Critiquer note 3, still non-blocking):**
   the reaction data fn chains `stores?.` but not `networkStatus?.`; safe in
   practice since `networkStatus` is always in `StoresMap` and mobx reports
   reaction-body errors without crashing the tree.

**Verification (re-run by reviewer, not taken on faith):**

- `yarn compile` → exit 0 (tsc clean; typed-scss-modules generation succeeded,
  including the new `DRepEmptyState.scss`), and `node_modules/.bin/tsc --noEmit`
  independently → exit 0.
- `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
  → 12/12 pass (defaults render in-sync; banner absent, fallback never triggers).
- `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → 20/20
  pass (floor intact).
- `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → 8/8 pass;
  `yarn test:jest source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
  → 7/7 pass (container change breaks no existing mount).
- `node_modules/.bin/eslint <4 changed ts/tsx files>` → 0 errors / 5 pre-existing
  warnings.
- `node_modules/.bin/prettier --check <4 changed ts/tsx files>` → all clean.

**Blockers:** none.

**Decision: approved**

## Code Review: task-161 — round 1 (2026-07-23)

**Scope reviewed:** the uncommitted diff (7 files: `governance.types.ts`,
`common/ipc/api.ts`, `GovernanceQueryService.ts`, main + renderer
`governanceChannel.ts`, `GovernanceQueryService.spec.ts`,
`tests/mocks/governance/drep-stake-distribution.json`) against guide Step 3
(3a-3h) and PRD FR-5 / FR-6 / FR-7 plus the task-161 contract row.

**Findings:**

1. **Guide conformance — exact.** Every edit matches the Step-3 post-edit quotes:
   `DRepStakeQueryPayload` added after `DRepListQueryPayload`
   (`governance.types.ts:96-101`); `GOVERNANCE_DREP_STAKE_CHANNEL` + request/
   response types appended to the governance block (`api.ts:664-666`);
   `fetchDRepList` split into `fetchDRepRegistrations` (no `--include-stake`,
   `votingPower` hard-null at `GovernanceQueryService.ts:504-505`) and
   `fetchDRepStake` with per-phase in-flight dedup fields and `reset()` clearing
   both; shared `_assertQueryable()` guard; `_parseStakeDistribution` accepts both
   the object-map and array-of-pairs container shapes, skips the two voting
   sentinels, and derives ids through the same `_credentialToDRepId` CIP-129 path
   the list payload uses. Main + renderer channel files are the guide's full
   replacements (the `__governanceError` plain-object contract now lives in a
   shared `toGovernanceIpcError` helper used by both handlers — FR-7 held).
2. **Contract shapes pinned.** Channel name, `void` request, and payload types
   match the guide verbatim; no drift. `main/ipc/index.ts` needed no change
   (`handleGovernanceRequests()` already registered) and none was made.
3. **Invariants held.** Local-first: no fetch/HTTP anywhere in the diff — both
   phases spawn `cardano-cli` against the local socket. Sanitization floor: every
   new `GovernanceQueryError` message identifies stake entries by index only
   (`GovernanceQueryService.ts:560-604`); floor suite re-run at 20/20. Lovelace
   losslessness: `JSONBig({ storeAsString: true })` parse → decimal-string map →
   wire; the oversized `9007199254740993` round-trips intact in both the fixture
   and the dedicated json-bigint test. CLI discipline: one bulk `--all-dreps`
   spawn per phase (`toHaveBeenCalledTimes(1)` asserted), era `latest`→`conway`
   fallback proven for the stake query, network flag trailing in the exact-argv
   test, socket handling untouched. Status vocabulary untouched; no new user copy
   in this task (logger strings are not copy), so no `!!!`/i18n work applies.
4. **Guardrails respected.** No store changes (`GovernanceStore.ts` untouched —
   store spec still 8/8 against the unchanged list channel), no timeout changes
   (`CLI_TIMEOUT_MS` intact for task-163), no streaming/push IPC, no per-DRep
   calls. `git status --short` shows exactly the seven Step-3 files.
5. **Spec updated per 3g, mock rewritten per 3f.** All 8 mechanical changes
   applied (rename ×28, fixture stake line dropped, argv arrays trimmed, old
   oversized-stake test deleted with its constant, fixture read as a raw string
   via `fs.readFileSync`); the new stake-phase describe block carries all 9 tests
   from the guide, ids derived through `Cardano.DRepID.cip129FromCredential` so
   merge-key alignment is proven, not assumed. Mock JSON matches the canonical
   object-map shape byte-for-byte and was not prettier-formatted.
6. **Incidental reformat (note, not a finding):** prettier 2.1.2's mandated
   format pass wrapped three long string properties in the spec fixtures
   (`url:`/`hash:`/`href:` onto their own lines); semantically identical.
7. **Pre-existing prettier drift in `api.ts` (note, not a finding):**
   `prettier --check source/common/ipc/api.ts` fails on 6 long type-alias hunks
   (`:343`, `:392`, `:405`, `:412`, `:465`, `:485`-area) that this diff did not
   touch — the HEAD content fails the same check under repo config (verified via
   a temp in-repo copy). The implementer correctly left them alone rather than
   flooding the diff; none of the task's added lines are flagged.

**Verification (re-run by reviewer, not taken on faith):**

- `yarn compile` → exit 0 (tsc clean, typed-scss-modules regeneration clean, tree
  unchanged after), and `node_modules/.bin/tsc --noEmit` independently → exit 0.
- `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts` →
  **34/34** pass — exactly the guide's predicted 26 − 1 + 9.
- `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → 8/8 pass
  (store untouched, list channel unchanged).
- `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → 20/20
  pass (floor intact).
- `yarn lint` → exit 0 (warnings only, repo-wide and pre-existing).
- `node_modules/.bin/prettier --check <6 changed .ts files>` → clean except the
  pre-existing `api.ts` drift documented in finding 7.
- `grep fetchDRepList|inFlightRefresh` across `source/main` + `source/common` →
  no stale references.

**Blockers:** none.

**Decision: approved**
