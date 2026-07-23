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

## Code Review: task-162 — round 1 (2026-07-23)

**Scope reviewed:** the uncommitted diff (8 edits: `GovernanceStore.ts`,
`DRepDirectory.tsx`, `DRepDirectoryList.tsx`, `DRepCard.tsx`,
`DRepDirectoryPage.tsx`, `DRepDirectory.spec.tsx`, `DRepDirectory.stories.tsx`,
`GovernanceStore.spec.ts`; 2 creates: `_shared/DRepErrorBanner.tsx`,
`_shared/DRepErrorBanner.scss`) against guide Step 4 (4a-4h) and PRD FR-8 / FR-9 +
US-UX.2 acceptance + the task-162 contract row.

**Findings:**

1. **Guide conformance — exact.** All ten files match the Step-4 post-edit quotes
   and full-file CREATE contents verbatim: `VotingPowerEnrichState` enum +
   `votingPowerState` observable + `isRankingUnavailable` computed; `fetchDRepList`
   sequences Phase 1 (paint at `Loaded`, `votingPowerState → Loading` inside the
   same `runInAction`) then awaits `_enrichVotingPower()`; the dedup guard covers
   the enrich window (`votingPowerState === Loading` short-circuits re-entry); the
   Phase-1 catch `return`s before Phase 2, so a stale-while-refresh list keeps its
   OLD voting power exactly as the guide's invariant note requires;
   `DRepErrorBanner` ships only the `rankingUnavailable` variant (PD-1) with
   `role="status"`, `aria-hidden` SVG, and `--badge-warning-*` fallback slots;
   `DRepCard` derives loading-vs-unavailable tooltip from the enrich state and
   attaches it as `title` + `aria-label`; `formatVotingPower` untouched (the
   `—`-for-null seam preserved per the contract's guardrail).
2. **US-UX.2 acceptance covered.** Phase 1 paints with `—` + loading tooltip;
   Phase 2 merges `new BigNumber(stake)` by `drepId` string equality against the
   `stakeByDRepId` decimal-string map; a DRep absent from the map stays `null`
   (`—` + unavailable tooltip, never a silent 0); Phase-2 failure keeps the list,
   sets `Failed`, and `DRepDirectory` renders the banner between the refreshing
   badge and the list; Manual Refresh re-runs both phases (`refresh()` →
   `fetchDRepList()`, verified at `GovernanceStore.ts:199-201`). Reshuffle
   correctly not built (PD-4 — forward-compat only).
3. **Invariants held.** Local-first: Phase 2 arrives via the existing
   `governanceDRepStakeChannel` IPC only — no network calls. Sanitization: both
   phase catches now log `{ errorType: normalized.type }` only (the diff also
   upgrades the previously-raw `{ error: err }` Phase-1 log — an improvement the
   guide mandates); floor suite re-run at 20/20. Lovelace: decimal-string →
   `BigNumber`, `votingPower: BigNumber | null`, never `Number`. CLI discipline:
   main-process files untouched. Copy: all three new `defineMessages` strings
   (`error.rankingUnavailable`, `votingPower.loadingTooltip`,
   `votingPower.unavailableTooltip`) carry `!!!` and use the exact §9 ids the
   task-164 locale keys will pair with (locale JSONs correctly untouched — task-164
   owns them, `yarn i18n:manage` correctly not run this step). No status
   vocabulary additions.
4. **Contract drift:** none. The store consumes
   `DRepStakeQueryPayload.stakeByDRepId` exactly as pinned
   (`governance.types.ts:96-101`); no channel/payload shape was modified.
5. **Scope clean.** `git status --short` shows exactly the ten Step-4 files. The
   `DRepErrorBanner.scss.d.ts` is generated by `yarn compile`'s typed-scss-modules
   pass and gitignored (`.gitignore:141`) — consistent with PD-12 and the
   task-160/161 precedent; correctly absent from the diff.
6. **Tests.** The spec changes are exactly the guide's: the stake-channel mock line
   keeps `GovernanceStore.spec.ts` green at 8/8 (the never-resolving dedup request
   holds the store in `Loading`, so Phase 2 never fires), and the component spec's
   `votingPowerState = Loaded` default keeps 12/12. The two-phase transition
   matrix, enrich-window dedup, and `{ errorType }`-spy tests land in task-167 by
   design (PD-11 / guide `:2191-2194`) — not a gap in this round.
7. **Story placement (note):** the optional `Ranking unavailable` story is present,
   placed after `Node syncing — empty fallback` as specified, uses the global
   locale toggle (no local IntlProvider), and nulls every entry's voting power so
   the unavailable tooltips are visible alongside the banner.
8. **Pre-existing lint warnings (note):** eslint on the five core files reports
   0 errors / 20 warnings — all the known TS-enum `no-shadow`/`no-unused-vars`
   false-positive pattern (the pre-existing `GovernanceRefreshState` enum shows
   the identical set) plus decorator/type-param positions; the new enum inherits
   the same config artifact. Nothing behavioral.

**Verification (re-run by reviewer, not taken on faith):**

- `yarn compile` → exit 0 (typed-scss-modules regenerated cleanly, including the
  new `DRepErrorBanner.scss`), and `node_modules/.bin/tsc --noEmit` independently
  → exit 0.
- `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → 8/8 pass.
- `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
  → 12/12 pass.
- `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → 20/20
  pass (NFR-4 checkpoint intact).
- `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts` → 34/34
  pass; `yarn test:jest source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
  → 7/7 pass (no regression from the container prop).
- `yarn lint` → exit 0 (warnings only, pre-existing baseline);
  `node_modules/.bin/eslint <9 changed ts/tsx files>` → 0 errors.
- `node_modules/.bin/prettier --check <10 changed files>` → all clean.

**Blockers:** none.

**Decision: approved**

## Code Review: task-163 — round 1 (2026-07-23)

**Scope reviewed:** uncommitted diff on `wt/ux-refinement` (base `32938d0da`,
task-162): `source/main/governance/GovernanceQueryService.ts` and
`tests/jest/governance/GovernanceQueryService.spec.ts` — exactly the two files
the guide's Step 5 (`ux-refinement-implementation-guide.md:2211-2433`) names.

**Findings:**

1. **Matches the approved step verbatim.** The static `CLI_TIMEOUT_MS` is
   replaced by `REGISTRATION_TIMEOUT_MS = 10_000` / `STAKE_TIMEOUT_MS = 30_000`
   with the guide's exact doc comment (5a); the budget is threaded as a
   parameter through `_runCliQueryWithEraFallback` → `_runCliQuery` per PD-7
   (5b/5c) and reused on the conway retry, so the fallback keeps the same
   phase budget; both registration call sites (`drep-state`, `tip`) pass the
   10 s budget and the stake call site passes 30 s (5d). The timeout message
   interpolates the per-call `${timeoutMs}` (FR-10 satisfied).
2. **Grep gate met.** `grep -rn "CLI_TIMEOUT_MS" source/ tests/` → empty, as
   the guide requires (`:2356-2357`, test-matrix row `:3958`).
3. **Spec updates are the guide's exactly.** First timeout test renamed to the
   10 s-registration title; the static-pin test now pins both budgets; the new
   30 s stake test asserts the promise is still pending at 10 001 ms (double
   `Promise.resolve()` microtask flush per the Jest 27 note) and rejects with
   `GovernanceQueryErrorType.Timeout` after the full 30 s. Suite is 35 (34 + 1
   net new), matching the guide's expected count.
4. **Design tokens confirmed, not rewritten.** `shared-design-tokens.md` is
   untouched in the diff; §6 already states phase-1 ≤10 s / phase-2 ≤30 s —
   the constants agree with the tokens. The 30 s value is carried as
   provisional (comment + PRD defer to the task-166 manual measurement); no
   re-derivation attempted, per the constraint.
5. **Invariants held.** Local-first: no network paths touched. Sanitization:
   the only log payload in the changed code is the pre-existing `{ args }` on
   the conway-retry warn (CLI flags only, no DRep ids); floor suite re-run at
   20/20. Lovelace: parse/IPC paths untouched. CLI discipline: bulk
   `--all-dreps`, era `latest`→`conway` fallback, env-only socket, and
   config-derived network flag all unchanged. No new copy (no `!!!`/i18n
   surface), no status-vocabulary changes, no IPC/payload drift — channels and
   shapes untouched.
6. **Scope clean.** Exactly the two named files changed; no doc, locale, or
   renderer files touched. No unnecessary complexity — the diff is the
   smallest change that threads the budgets per call.
7. **Pre-existing lint warnings (note):** eslint on
   `GovernanceQueryService.ts` reports 0 errors / 4 warnings
   (`no-non-null-assertion` ×2, `no-explicit-any` ×2); the identical set
   exists at HEAD (verified by stash/compare) — nothing introduced by this
   diff. The spec file is outside the eslint `lint` script's roots
   (`source storybook utils`), consistent with all prior suites.

**Verification (re-run by reviewer, not taken on faith):**

- `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts` →
  35/35 pass.
- `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` →
  20/20 pass (NFR-4 checkpoint intact).
- `yarn compile` (`tsc --noEmit`) → exit 0.
- `node_modules/.bin/eslint` on both changed files → 0 errors (4 pre-existing
  warnings, unchanged from HEAD).
- `node_modules/.bin/prettier --check` on both changed files → clean.
- `grep -rn "CLI_TIMEOUT_MS" source/ tests/` → empty.

**Blockers:** none.

**Decision: approved**

## Code Review: task-164 — round 1 (2026-07-23)

**Scope reviewed:** uncommitted diff on `wt/ux-refinement` (base `6328ac0f9`,
task-163): `source/renderer/app/i18n/locales/en-US.json`,
`source/renderer/app/i18n/locales/ja-JP.json`, plus the two files
`yarn i18n:manage` rewrote (`source/renderer/app/i18n/locales/defaultMessages.json`,
`translations/messages.json`) — exactly the file set the guide's Step 6
(`ux-refinement-implementation-guide.md:2437-2551`) and commit note (`:4025`)
sanction. 80 insertions, zero deletions, no other files touched.

**Findings:**

1. **Matches the approved step verbatim.** All five keys land at the exact
   alphabetical positions the guide pins (6a/6b): `empty.noSync` (en-US:288,
   ja-JP:288), `error.rankingUnavailable` (:290), `syncing` (:302),
   `votingPower.loadingTooltip` (:305), `votingPower.unavailableTooltip`
   (:306). The en strings are byte-identical to the guide's block and to the
   component `defaultMessage`s from Steps 2/4 (`DRepEmptyState.tsx:9`,
   `DRepErrorBanner.tsx:9`, `DRepCard.tsx:26,31`, `DRepDirectory.tsx:53`);
   the ja-JP strings match the guide's four insert snippets exactly, and the
   `{progress}` placeholder name matches across locales (FR-11 satisfied).
2. **`!!!` invariant held.** All ten new strings (5 × 2 locales) carry the
   leading `!!!`; the diff is insertion-only, so no existing marker was
   removed. No copy beyond this phase's surfaces (`empty.selfnode`,
   `error.refresh` absent, per the PRD constraint row `:84`).
3. **`yarn i18n:manage` re-run by reviewer → exit 0** (no environment debt
   needed) **and idempotent**: the working tree is byte-identical after the
   re-run, confirming the committed `defaultMessages.json`/`messages.json`
   rewrites are exactly what the manager produces. The generated-file diffs
   contain only the five new descriptors.
4. **ja-JP render test green, missing-key warnings gone.**
   `DRepDirectory.spec.tsx:190` renders the directory under `ja-JP` against
   the real locale JSON; suite 12/12 with no missing-message console output
   in the run (Step 6c-3 satisfied).
5. **Invariants held.** No code paths touched: local-first, lovelace
   losslessness, CLI discipline, and IPC channel/payload shapes are all
   untouched by a locale-only diff. Status vocabulary unchanged
   (`status.active`/`status.inactive` only). Sanitization floor suite re-run:
   20/20. New copy contains no DRep ids or bech32 strings.
6. **Scope and formatting clean.** Locale JSONs were hand-edited (two-space
   indentation matches neighbors, no reformat noise elsewhere in either
   file); prettier was correctly not run (JSON-only step). Vocabulary note:
   the `syncing` string's `({progress}%)` is the guide's pinned canonical
   copy for this surface, so the Mithril-era "sync-% dropped" convention does
   not apply here.

**Verification (re-run by reviewer, not taken on faith):**

- `yarn i18n:manage` → exit 0; working tree unchanged afterwards
  (idempotent).
- Step 6c grep → exactly 5 hits per locale file at the pinned positions.
- `yarn compile` (`tsc --noEmit`) → exit 0.
- `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
  → 12/12 pass, no missing-key warnings.
- `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` →
  20/20 pass (NFR-4 checkpoint intact).
- `yarn lint` not applicable — the diff is JSON-only, outside ESLint's
  targets.

**Blockers:** none.

**Decision: approved**
