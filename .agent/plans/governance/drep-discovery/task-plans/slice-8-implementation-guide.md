# Slice-8 Implementation Guide: Refresh-latency, Selfnode & Release Verification

> **Planning anchor:** HEAD `0cdcab581` (clean tree). Every `path:line` below was
> re-opened and re-verified against the working tree while this guide was written.
> **Upstream:** [slice-8-PRD.md](./slice-8-PRD.md) — decisions D-1…D-18 are binding.
> **Review log:** [slice-8-code-review.md](./slice-8-code-review.md) (append-only).

---

## How to read this guide

This guide is the **only** document you need. Do not read the PRD, the tasks JSON
or the design docs to implement a step — every decision, every invariant, every
exact string and every line anchor you need is reproduced here inline.

- Work **one task at a time, in the build order below**.
- Inside a task, work the numbered steps **in order**. Steps are numbered
  per-task (task-123 restarts at Step 1; task-124 restarts at Step 1).
- Each step names the **exact file**, quotes the **exact code being replaced**,
  and gives the **exact replacement**.
- Line numbers shift as you edit. **Re-open the file and match on the quoted
  code**, not on the line number. The anchors are there so you do not have to
  hunt, not so you can edit blind.
- When a step says "unchanged", leave the code byte-identical.

---

## Build order (binding)

```
task-123  →  task-124  →  task-125
```

- **123 before 124** — both edit the `switch (true)` inside
  `DRepDirectory.renderContent()`. task-123's edits are structural (one arm
  replaced, one banner block replaced, one badge deleted); task-124 inserts one
  new arm. Landing the structural edit first means the insertion goes into a
  settled file. task-123 also owns the only widening of the IPC payload types,
  which task-124 must not re-open.
- **125 last** — it is `manual_execution` and has **no build steps**. See its
  section at the end.

---

## Shared conventions every task inherits

### Code comments

Default is **no comment**. Add one only when the logic is not self-evident and a
better name cannot fix it. When warranted: 1–3 lines, plain sentence case,
stating the invariant / constraint / reason — never the *what*, never change
history ("was removed", "now does X"), never a defence of correctness.

**Never** put a task id, a `CAT-*` / `CP-*` label, a plan name or a PR number in
a source comment **or in a test name**. **Never** use ALL-CAPS emphasis (MUST,
NEVER) inside a code comment.

In test files the bar is higher: the `describe` / `it` name carries the intent —
comment only a non-obvious fixture or mock constraint.

Live examples that match the house style: `DRepDirectory.tsx:231-232`,
`GovernanceStore.ts:313-314`, `GovernanceQueryService.ts:60-64`.

### Commit messages

Not your job — see the appendix. (One subject-only commit per task is created by
a later pipeline stage.)

### Formatting

`nix fmt` is the mandated formatter and **`nix` does not exist in this
container**. The accepted substitute is the binary on **explicit paths**:

```bash
/home/node/.claude/jobs/3bad97d1/wt-slice-8/node_modules/.bin/prettier --write <path> <path> ...
```

- **Never** run `yarn prettier` / `yarn prettier:check` — the package.json script
  globs `"**/*.*"` and would reformat ~240 unrelated files.
- **Never** prettier-format tool-managed JSON: the tasks tracker,
  `source/renderer/app/i18n/locales/*.json`, `source/renderer/app/i18n/locales/defaultMessages.json`,
  `translations/messages.json`.
- **Prettier's scope** (`.prettierignore`, measured): `.js` / `.ts` / `.tsx` /
  `.scss` / `.json` under `source/`, `storybook/`, `tests/`, `features/`,
  `hardware-wallet-tests/`. **`.scss` is in scope** — a new stylesheet must be in
  the `--write` list. **`.md` is in scope nowhere** — running prettier on a
  planning or design document silently does nothing; do not put one in the list.
- **Format the files you create, plus the edited files that are prettier-clean at
  HEAD.** Clean at HEAD, measured, so safe to `--write`:
  `source/common/types/governance.types.ts`,
  `.../_shared/DRepErrorBanner.tsx`, `.../_shared/DRepErrorBanner.scss`,
  `.../_shared/DRepEmptyState.tsx`, `.../_shared/DRepEmptyState.scss`,
  `.../drep-directory/DRepDirectory.scss`,
  `.../drep-directory/DRepDirectoryBanner.tsx`,
  `.../drep-directory/DRepDirectoryBanner.scss`,
  `.../drep-directory/DRepDirectory.spec.tsx`,
  `.../drep-directory/DRepDirectoryBanner.spec.tsx`,
  `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx`,
  `tests/jest/governance/logDRepStateSnapshot.spec.ts`.
- **Exactly five edited files carry pre-existing drift at HEAD** and must **not**
  be `--write`-ten — hand-match the surrounding style instead:
  `source/common/ipc/api.ts`, `source/main/governance/GovernanceQueryService.ts`,
  `.../drep-directory/DRepDirectory.tsx`,
  `storybook/stories/governance/DRepDirectory.stories.tsx`,
  `tests/jest/governance/GovernanceQueryService.spec.ts`. Running `--write` on one
  produces hundreds of unrelated reformatted lines inside the task diff.

### Discarding unwanted changes

Use `git restore <paths>` or `git checkout -- <paths>`.
**Never `git stash`** — the stash stack is shared across worktrees and concurrent
sessions.

### Storybook conventions

- **Never** wrap a story in its own `IntlProvider`.
- **Never** create per-locale `(en-US)` / `(ja-JP)` story variants.
  `storybook/preview.tsx` applies a global `StoryWrapper` decorator driven by the
  English/Japanese toggle. The live guard comment is at
  `storybook/stories/governance/DRepDirectory.stories.tsx:172-174`.
- Add states to the **existing** `DIRECTORY_STATE_OPTIONS` knob, never a parallel
  `storiesOf` block or a second decorator.
- Story names: sentence case, em-dash for sub-variants
  (`'Refresh failed — retained snapshot'`).
- The integrated `Connected flow` story (`:303-305`) must still run after both
  tasks; it is the in-file mirror of the
  `Voting / Governance > Connected flow` exemplar at
  `storybook/stories/voting/Governance.stories.tsx:305`.

### i18n conventions

- Every **new** en-US and ja-JP string keeps the leading `!!!` preliminary
  marker. Removing a `!!!` is a release-end manual review, never a per-task
  action.
- Every new key goes into **both** `source/renderer/app/i18n/locales/en-US.json`
  and `ja-JP.json`.
- Both catalogs are **strictly `Array.prototype.sort()` ordered** — verified:
  1652 keys, 0 mismatches. Insert each new key at its exact sorted position (the
  steps give it).
- `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:57-64` fails any `governance.*`
  key missing `!!!` in **either** locale; `:39-46` fails any key present in only
  one catalog.

### Environment facts (given — do not re-derive)

| Fact | Value |
|---|---|
| Worktree root | `/home/node/.claude/jobs/3bad97d1/wt-slice-8` |
| Node / jest / prettier / TS | v24.16.0 / 27.5.1 / 2.1.2 / 4.9.5 |
| React / MobX / react-intl | 16.14.0 / 5.15.7 / **2.9.0** (`injectIntl`, `intlShape`, `FormattedMessage` — no hooks, no `FormattedRelativeTime`) |
| `yarn compile` at HEAD | **exit 0**, 25.9 s. Its `precompile` hook runs `yarn typedef:sass`, regenerating the **gitignored** `*.scss.d.ts` (`.gitignore:141`). A new `.scss` file gets its `.d.ts` from this hook — do not hand-write one, do not commit one. |
| `yarn lint` at HEAD | **exit 0**, ~54 s, 0 errors / ~5635 warnings. **Errors are the gate; warnings are not.** `react/no-array-index-key` is configured as `warn`. |
| `yarn storybook:build` at HEAD | **exit 0**, ~84 s. Green — run it, do not waive it. |
| `yarn stylelint` at HEAD | **RED, 118 errors**, every one `order/properties-alphabetical-order`, every one in this feature's own governance SCSS. **Out of scope for slice-8** (see the per-task stylelint contract). |
| `yarn check:all` | RED transitively (`prettier:check` + `stylelint`). **Not a gate for this slice.** |
| `nix`, `gh`, `git push` | absent. Work stays local. |
| `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` | self-skips (1 suite / 12 tests) without `cardano-cli` on PATH — expected, not a regression. |
| `jest tests/jest` alone | ~8 % of the suite. Never report it as "the suite". |

### Measured baselines at HEAD `0cdcab581`

Run once, all green (8 suites / **219** tests / **1** snapshot, 5.6 s):

| Suite | tests at HEAD |
|---|---|
| `tests/jest/governance/GovernanceQueryService.spec.ts` | 38 |
| `tests/jest/governance/GovernanceStore.spec.ts` | 56 |
| `tests/jest/governance/logDRepStateSnapshot.spec.ts` | 5 |
| `tests/jest/security/governance-sanitization.spec.ts` | 39 |
| `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` | 5 |
| `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | 60 (+1 snapshot) |
| `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx` | 7 |
| `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` | 9 |

The single snapshot is `__snapshots__/DRepDirectory.spec.tsx.snap` and contains
**only the category-badge `<span>`** (19 lines). Nothing in slice-8 touches it.
**It must stay byte-identical — never run jest with `-u`.**

Per-file stylelint baselines (needed by the SCSS steps):

| File | errors at HEAD |
|---|---|
| `.../drep-directory/DRepDirectory.scss` | 19 |
| `.../drep-directory/DRepDirectoryBanner.scss` | 10 |
| `.../_shared/DRepEmptyState.scss` | 3 |
| `.../_shared/DRepErrorBanner.scss` | 4 |
| **repo total** | **118** |

---

# task-123 — Define refresh latency budget and stale-while-refresh visual contract

**Interaction mode:** `autonomous`. **Commit type:** `feat`.

## 1. What this task is

Ship the *visual* half of the refresh contract the design has specified since
slice-1 but the code only half-implements, and add one observational scalar to
the wire:

1. `DRepListQueryPayload` and `DRepStakeQueryPayload` each gain one plain-`number`
   `elapsedMs`, measured around the phase's CLI work in the main process.
2. The refresh-latency budget is documented in the IPC contract comment block.
3. The first-load `LoadingSpinner` is replaced by a **real DRep-card skeleton
   list** (new component).
4. `DRepErrorBanner` gains a `'refreshFailed'` variant carrying the designed copy,
   and it replaces the hand-rolled retained-data banner that currently prints raw
   main-process strings to the user.
5. The stale-while-refresh spinner badge **moves** out of the list area and sits
   beside the "Last updated {time}" line in the directory header.
6. Tests cover fresh-load, stale-with-spinner and the timeout banner.

## 2. What this task is NOT — read this before you start

- **No second timeout authority.** `_runCliQuery`'s `setTimeout`
  (`GovernanceQueryService.ts:382-390`) stays the only enforcement.
  `REGISTRATION_TIMEOUT_MS` / `STAKE_TIMEOUT_MS` (`:57-58`) are **not renamed,
  not moved, not re-derived**, and the test that pins them
  (`GovernanceQueryService.spec.ts:472-477`) is **not edited**.
- **No renderer timer of any kind.** Not a timeout, not a 700 ms delay before
  painting the skeleton. §6's "≤700 ms before skeleton" is a *paint-latency
  budget*, not a delay to schedule; a static data-free render satisfies it by
  construction. **slice-8 adds zero `setTimeout` / `setInterval` calls to the
  renderer** — that is a grep-checkable property the reviewer will check.
- **No main-process refresh-state machine.** `GovernanceRefreshState`
  (`GovernanceStore.ts:62-68`) stays the single refresh-state authority. The wire
  carries `elapsedMs` and nothing else.
- **No IPC consumer for `getLastSuccessfulData()`** (`GovernanceQueryService.ts:202-204`).
  It stays unconsumed. The banner's `{time}` comes from the renderer's own
  `lastFetchedAt`, because the store is the only place that knows what is
  actually on screen.
- **No probe or warm-up CLI query** to measure latency. `elapsedMs` is measured
  around the query that already runs.
- **The no-cached-data `Failed` arm (`DRepDirectory.tsx:253-269`) is untouched**
  and keeps `governance.drepDirectory.error`. Its copy is out of scope: the new
  banner sentence promises "the last successful snapshot", which is false when
  there has never been one.
- **No new IPC channel, no new npm dependency, no new `GovernanceQueryErrorType`
  member.** The enum already carries `Timeout` and `SelfnodeCliUnsupported`.
- **No stylelint sweep**, no `stylelint --fix`, no reordering of any existing
  declaration.
- **No re-derivation of the 30 s stake budget.** It is provisional pending a
  synced-node measurement that is not this task's.

## 3. Locked invariants this change must not break (inlined — do not look them up)

1. **Local-first.** Discovery data comes only from the local node via the
   main-process `GovernanceQueryService`. No hosted explorers, indexers, GovTool,
   Koios, Blockfrost or public governance APIs.
2. **Sanitization floor.** No DRep id, no `abstain` / `no_confidence` literal, no
   CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store
   payload. **This task stresses it:** `elapsedMs` rides `DRepListQueryPayload`,
   which `source/main/ipc/governanceChannel.ts:56` hands wholesale to
   `logDRepStateSnapshot`, which serialises it into `Logs/pub/DRep-state-snapshot.json`
   and **deliberately bypasses `filterLogData`** (`source/main/utils/setupLogging.ts:183-204`).
   `elapsedMs` is admissible **because** it is a non-identifying millisecond
   integer — it names no DRep, encodes no bech32 string and reveals no vote. That
   is the reason, not "the payload is a free-for-all". Re-run
   `tests/jest/governance/logDRepStateSnapshot.spec.ts` **and**
   `tests/jest/security/governance-sanitization.spec.ts`.
3. **CLI discipline.** Bulk `--all-dreps` once per refresh; per-DRep invocations
   are forbidden. The network flag (`--mainnet` / `--testnet-magic <N>`) derives
   from node config only, never from renderer/IPC input. The socket goes through
   `CARDANO_NODE_SOCKET_PATH` in `spawn.env`, never argv.
4. **Lovelace losslessness.** `json-bigint` lossless parse → decimal-string IPC →
   renderer `BigNumber` rehydration. Never route raw `JSONbig` objects across IPC
   or into observables. **`elapsedMs` is a plain `number` of milliseconds** — it
   is never a lovelace value and never a decimal string.
5. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!`
   marker.
6. **DRep status grounding.** Canonical on-chain status is `active | inactive`.
   Do not widen that union. (task-123 touches nothing here; listed so you do not
   drift into it while editing shared components.)
7. **Smallest truthful change.** Reuse existing seams — `RendererIpcChannel`,
   `GovernanceQueryService` + `governanceChannel`, the `_shared` governance
   components — over new abstractions. The **one** new component in this task is
   the skeleton list, and it exists only because the spinner does not satisfy the
   designed contract.
8. **No second delegation backend / no auto-delegation.** Untouched here.

## 4. Verified line anchors (re-verified at `0cdcab581`)

| Anchor | What is there |
|---|---|
| `source/common/types/governance.types.ts:139-146` | `DRepListQueryPayload { dreps; fetchedAt; epoch }` |
| `source/common/types/governance.types.ts:148-153` | `DRepStakeQueryPayload { stakeByDRepId; fetchedAt }` |
| `source/common/ipc/api.ts:656-660` | the `GOVERNANCE IPC CHANNELS` comment block |
| `source/main/governance/GovernanceQueryService.ts:224-258` | `_doFetchDRepRegistrations()` |
| `source/main/governance/GovernanceQueryService.ts:260-284` | `_doFetchDRepStake()` |
| `.../drep-directory/DRepDirectory.tsx:40-44` | the `loading` message entry |
| `.../drep-directory/DRepDirectory.tsx:60-64` | the `refreshing` message entry |
| `.../drep-directory/DRepDirectory.tsx:240-248` | `renderContent()` opening + the `Loading` arm |
| `.../drep-directory/DRepDirectory.tsx:324-341` | the hand-rolled retained-data `errorBanner` div |
| `.../drep-directory/DRepDirectory.tsx:342-347` | the `refreshingBadge` div |
| `.../drep-directory/DRepDirectoryBanner.tsx:103-109` | the `lastUpdated` paragraph |
| `.../_shared/DRepErrorBanner.tsx:14-16` | the guard comment + `DRepErrorBannerVariant` |
| `.../_shared/DRepErrorBanner.tsx:24-26` | `messageByVariant` |
| `.../drep-directory/DRepDirectoryList.tsx:14` | `const CARDS_PER_PAGE = 25;` |
| `storybook/stories/governance/DRepDirectory.stories.tsx:242-290` | `DIRECTORY_STATE_OPTIONS` + `resolveDirectoryState` |

## 5. Decisions already made — implement them, do not re-derive

- **The skeleton is a real component, not a spinner.** It is static, data-free,
  holds no state, takes no props beyond a count, runs no timer, and paints
  immediately on `Loading`.
- **Skeleton card count is 25**, mirroring the directory list's page size
  (`CARDS_PER_PAGE = 25`), so the first paint holds the height the real page will
  occupy. It is a `count` prop with `25` as the default so stories can shrink it.
- **The skeleton mints no copy.** It reuses the existing
  `governance.drepDirectory.loading` id as its accessible label. Re-declaring an
  existing id in a second `defineMessages` block is established repo practice
  (`governance.drepDirectory.title` is declared in both `DRepDirectory.tsx:36`
  and `DRepDirectoryBanner.tsx:12`).
- **`DRepErrorBanner` receives the already-formatted retry label and the handler
  as props** rather than re-declaring `governance.drepDirectory.retry`. It
  declares only its own new `refreshFailed` message.
- **The `{time}` slot is formatted inside `DRepErrorBanner`** with
  `moment(lastFetchedAt).fromNow()`, mirroring `DRepDirectoryBanner.tsx:90`. The
  banner renders only where retained data exists, so `lastFetchedAt` is non-null
  on that path by construction.
- **Exactly one new i18n key in this task:** `governance.drepDirectory.error.refresh`.
  `retry`, `refreshing` and `loading` are reused, not re-minted.

### Stylelint contract for this task (read it before you touch any `.scss`)

The 118 pre-existing errors are **out of scope**: no `--fix`, no reordering of
any existing declaration, no cleanup sweep.

Two things follow:

- Every declaration you **add** to an existing stylesheet, and every declaration
  in the **new** stylesheet, must be in alphabetical order inside its block, so
  the count never grows.
- Steps 8 and 9 **delete** two selector blocks that become dead code
  (`.errorBanner` and `.refreshingBadge` in `DRepDirectory.scss`). Deleting a
  block also deletes the errors it contained. **Measured:** those two blocks hold
  7 of `DRepDirectory.scss`'s 19 errors, so the repo total moves
  **118 → 111** in this task, purely as a consequence of removing dead code —
  **not** from any sweep or reorder. That is the expected, recorded number. If
  you measure anything other than 111, something else changed and you must find
  it. Record the delta; do not "fix" it back.

---

## Step 1 — Add `elapsedMs` to both query payload types

**File:** `source/common/types/governance.types.ts`

Replace the block at `:139-153`:

```ts
export interface DRepListQueryPayload {
  /** All DRep entries from the ledger state. */
  dreps: DRepDirectoryEntry[];
  /** Unix timestamp (ms) when the data was fetched. */
  fetchedAt: number;
  /** Current epoch number returned by `query tip`; nullable for compatibility. */
  epoch: number | null;
}

export interface DRepStakeQueryPayload {
  /** Voting power in lovelace (decimal string) keyed by CIP-129 DRep id. */
  stakeByDRepId: Record<DRepId, Lovelace>;
  /** Unix timestamp (ms) when the stake distribution was fetched. */
  fetchedAt: number;
}
```

with:

```ts
export interface DRepListQueryPayload {
  /** All DRep entries from the ledger state. */
  dreps: DRepDirectoryEntry[];
  /** Unix timestamp (ms) when the data was fetched. */
  fetchedAt: number;
  /** Current epoch number returned by `query tip`; nullable for compatibility. */
  epoch: number | null;
  /**
   * Milliseconds the phase-1 registration query took. Observational only: the
   * CLI timeout in the main process stays the sole enforcement, and no consumer
   * may schedule a timer from this value.
   */
  elapsedMs: number;
}

export interface DRepStakeQueryPayload {
  /** Voting power in lovelace (decimal string) keyed by CIP-129 DRep id. */
  stakeByDRepId: Record<DRepId, Lovelace>;
  /** Unix timestamp (ms) when the stake distribution was fetched. */
  fetchedAt: number;
  /** Milliseconds the phase-2 stake query took. Observational only, like the phase-1 field. */
  elapsedMs: number;
}
```

Both fields are **required**, not optional. The only other typed construction
site in the repo is `tests/jest/governance/logDRepStateSnapshot.spec.ts:38`,
fixed in Step 13. (`tests/jest/governance/GovernanceStore.spec.ts`'s
`phase1Payload()` is an unannotated object literal and needs no change.)

## Step 2 — Measure the elapsed time in the main process

**File:** `source/main/governance/GovernanceQueryService.ts`

**2a.** In `_doFetchDRepRegistrations()` (opens at `:224`), the current body is:

```ts
  private async _doFetchDRepRegistrations(): Promise<DRepListQueryPayload> {
    this._assertQueryable();

    try {
      const [drepStateStdout, tipStdout] = await Promise.all([
```

Insert the start sample **after** `_assertQueryable()` so a selfnode or
socket-unavailable throw performs no measurement:

```ts
  private async _doFetchDRepRegistrations(): Promise<DRepListQueryPayload> {
    this._assertQueryable();

    const startedAt = Date.now();

    try {
      const [drepStateStdout, tipStdout] = await Promise.all([
```

Then replace the return at `:242-246`:

```ts
      return {
        dreps,
        fetchedAt: Date.now(),
        epoch: currentEpoch,
      };
```

with:

```ts
      const fetchedAt = Date.now();

      return {
        dreps,
        fetchedAt,
        epoch: currentEpoch,
        elapsedMs: fetchedAt - startedAt,
      };
```

**2b.** In `_doFetchDRepStake()` (opens at `:260`), same shape. Current:

```ts
  private async _doFetchDRepStake(): Promise<DRepStakeQueryPayload> {
    this._assertQueryable();

    try {
      const stakeStdout = await this._runCliQueryWithEraFallback(
        ['query', 'drep-stake-distribution', '--all-dreps', '--output-json'],
        GovernanceQueryService.STAKE_TIMEOUT_MS
      );

      return {
        stakeByDRepId: this._parseStakeDistribution(stakeStdout),
        fetchedAt: Date.now(),
      };
```

becomes:

```ts
  private async _doFetchDRepStake(): Promise<DRepStakeQueryPayload> {
    this._assertQueryable();

    const startedAt = Date.now();

    try {
      const stakeStdout = await this._runCliQueryWithEraFallback(
        ['query', 'drep-stake-distribution', '--all-dreps', '--output-json'],
        GovernanceQueryService.STAKE_TIMEOUT_MS
      );
      const fetchedAt = Date.now();

      return {
        stakeByDRepId: this._parseStakeDistribution(stakeStdout),
        fetchedAt,
        elapsedMs: fetchedAt - startedAt,
      };
```

**Do not** touch `REGISTRATION_TIMEOUT_MS` / `STAKE_TIMEOUT_MS` (`:57-58`),
`_runCliQuery` (`:320-417`), the in-flight dedup wrappers (`:164-178`, `:187-199`),
`_assertQueryable()` (`:208-222`) or `reset()` (`:146-153`). Nothing new is
cached, so `reset()` needs no change.

`GovernanceQueryService.ts` is **pre-drifted for prettier** — hand-match the
surrounding style, do not run `--write` on it.

## Step 3 — Document the refresh-latency budget in the IPC contract

**File:** `source/common/ipc/api.ts`

Replace the comment block at `:656-660`:

```ts
/**
 * ====================== GOVERNANCE IPC CHANNELS ======================
 * Channels for DRep Discovery and governance data queries.
 * =====================================================================
 */
```

with:

```ts
/**
 * ====================== GOVERNANCE IPC CHANNELS ======================
 * Channels for DRep Discovery and governance data queries.
 *
 * Refresh-latency budget (shared-design-tokens "Refresh State"): phase 1
 * `drep-state --all-dreps` paints the list within 10s; phase 2
 * `drep-stake-distribution --all-dreps` enriches voting power within 30s. Both
 * budgets are enforced only by the main-process CLI timeouts in
 * GovernanceQueryService. The `elapsedMs` field on each payload reports the
 * measured duration of a completed query; it is observational and no consumer
 * may schedule a timer from it.
 * =====================================================================
 */
```

The channel triads at `:662-672` are **unchanged** — no new channel, no changed
request/response type name. `source/renderer/app/ipc/governanceChannel.ts` needs
no edit: its `RendererIpcChannel` types flow from `api.ts`.

`api.ts` is **pre-drifted for prettier** — do not run `--write` on it.

**Then confirm the design-token half of the same criterion — no edit.** The
refresh budget table already exists at
`.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md:95-102`,
and the service already cites it by name in its doc comment at
`source/main/governance/GovernanceQueryService.ts:52-56`. Open both, confirm they
are present, and record that in the task report. **Do not add a second budget
table, and do not edit either file for this criterion** — with the comment block
above, the criterion is then discharged on both halves.

## Step 4 — Create the skeleton component

**New file:** `source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.tsx`

```tsx
import React from 'react';
import classNames from 'classnames';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './DRepDirectorySkeleton.scss';

const messages = defineMessages({
  loading: {
    id: 'governance.drepDirectory.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Accessible label of the first-load skeleton list',
  },
});

// Mirrors the directory list page size so the first paint holds the height the
// loaded page will occupy.
const SKELETON_CARD_COUNT = 25;

interface Props {
  count?: number;
  intl: intlShape.isRequired;
}

function DRepDirectorySkeleton({ count = SKELETON_CARD_COUNT, intl }: Props) {
  return (
    <div
      className={styles.skeletonList}
      role="status"
      aria-busy="true"
      aria-label={intl.formatMessage(messages.loading)}
    >
      {Array.from({ length: count }, (_, index) => (
        <div className={styles.skeletonCard} key={index} aria-hidden="true">
          <div className={styles.topRow}>
            <span className={classNames(styles.block, styles.badge)} />
            <span className={classNames(styles.block, styles.badge)} />
            <span className={classNames(styles.block, styles.id)} />
          </div>
          <div className={styles.bottomRow}>
            <span className={classNames(styles.block, styles.label)} />
            <span className={classNames(styles.block, styles.value)} />
          </div>
          <div className={styles.actionsRow}>
            <span className={classNames(styles.block, styles.action)} />
            <span className={classNames(styles.block, styles.action)} />
          </div>
        </div>
      ))}
    </div>
  );
}

export default injectIntl(DRepDirectorySkeleton);
```

Notes bound by decisions above — do not "improve" them:

- No state, no `useEffect`, no `setTimeout`, no store, no observable.
- The `loading` message re-declares the **existing** id
  `governance.drepDirectory.loading` with the **byte-identical** default message
  `'!!!Loading DRep data…'` (note the single-character ellipsis `…`, not `...`).
  It mints no catalog key.
- `key={index}` triggers the `react/no-array-index-key` rule, which this repo
  configures as **`warn`**. Warnings are not the lint gate. The placeholders are
  identical and orderless, so an index key is correct here.

## Step 5 — Create the skeleton stylesheet

**New file:** `source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.scss`

Every block below is already in alphabetical property order. This file must be
**stylelint-clean at birth** — verify with
`node_modules/.bin/stylelint source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.scss`
(expect **0** errors).

```scss
.skeletonList {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.skeletonCard {
  background: var(--theme-card-bg, #ffffff);
  border: 1px solid var(--theme-separator, #e0e0e0);
  border-radius: 8px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 16px;
}

.topRow {
  align-items: center;
  display: flex;
  gap: 12px;
}

.bottomRow {
  align-items: center;
  display: flex;
  gap: 6px;
}

.actionsRow {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
}

.block {
  animation: skeleton-pulse 1.4s ease-in-out infinite;
  background: var(--theme-separator, #e0e0e0);
  border-radius: 4px;
  display: inline-block;
  height: 12px;
}

.badge {
  width: 72px;
}

.id {
  width: 260px;
}

.label {
  width: 88px;
}

.value {
  width: 120px;
}

.action {
  height: 32px;
  width: 128px;
}

@keyframes skeleton-pulse {
  0%,
  100% {
    opacity: 1;
  }

  50% {
    opacity: 0.45;
  }
}

@media (prefers-reduced-motion: reduce) {
  .block {
    animation: none;
  }
}
```

A plain (non-`:global`) `@keyframes` in a component stylesheet is the established
repo pattern — e.g. `source/renderer/app/components/wallet/tokens/wallet-token/WalletToken.scss:27`.

**Do not** hand-write `DRepDirectorySkeleton.scss.d.ts`. `yarn compile`'s
`precompile` hook (`yarn typedef:sass`) generates it, and `.gitignore:141`
excludes it.

## Step 6 — Render the skeleton from the `Loading` arm

**File:** `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`

**6a.** Add the import next to the other directory imports (after the
`DRepDirectoryFilters` import at `:8`):

```tsx
import DRepDirectorySkeleton from './DRepDirectorySkeleton';
```

**6b.** Replace the `Loading` arm at `:242-248`:

```tsx
      case refreshState === GovernanceRefreshState.Loading:
        return (
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        );
```

with:

```tsx
      case refreshState === GovernanceRefreshState.Loading:
        return <DRepDirectorySkeleton />;
```

**6c.** Delete the now-unused `loading` message entry at `:40-44`:

```tsx
  loading: {
    id: 'governance.drepDirectory.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Loading state message',
  },
```

The id survives — the skeleton declares it now.

**Do not** remove the `LoadingSpinner` import yet; Step 9 removes its last use.

`DRepDirectory.tsx` is **pre-drifted for prettier** — hand-match the surrounding
style, do not run `--write` on it.

## Step 7 — Add the `refreshFailed` variant to `DRepErrorBanner`

**File:** `source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx`

Replace lines `:1-26` (imports, `defineMessages`, the guard comment, the type,
`Props`, and the `messageByVariant` map) with:

```tsx
import React from 'react';
import moment from 'moment';
import {
  FormattedMessage,
  defineMessages,
  injectIntl,
  intlShape,
} from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './DRepErrorBanner.scss';

const messages = defineMessages({
  rankingUnavailable: {
    id: 'governance.drepDirectory.error.rankingUnavailable',
    defaultMessage:
      '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.',
    description: 'Non-blocking banner when the stake phase fails',
  },
  refreshFailed: {
    id: 'governance.drepDirectory.error.refresh',
    defaultMessage:
      "!!!Couldn't refresh DRep data. {Retry}. Showing last successful snapshot from {time}.",
    description:
      'Non-blocking banner when a refresh fails while a retained snapshot is on screen',
  },
});

export type DRepErrorBannerVariant = 'rankingUnavailable' | 'refreshFailed';

interface Props {
  variant: DRepErrorBannerVariant;
  retryLabel?: string;
  onRetry?: () => void;
  lastFetchedAt?: number | null;
  intl: intlShape.isRequired;
}

function DRepErrorBanner({
  variant,
  retryLabel = '',
  onRetry,
  lastFetchedAt = null,
  intl,
}: Props) {
  const messageByVariant = {
    rankingUnavailable: messages.rankingUnavailable,
    refreshFailed: messages.refreshFailed,
  };

  const body =
    variant === 'refreshFailed' ? (
      <FormattedMessage
        {...messageByVariant.refreshFailed}
        values={{
          Retry: (
            <Link
              className={styles.retryLink}
              label={retryLabel}
              hasIconAfter={false}
              onClick={onRetry}
              skin={LinkSkin}
            />
          ),
          time: lastFetchedAt ? moment(lastFetchedAt).fromNow() : '',
        }}
      />
    ) : (
      intl.formatMessage(messageByVariant[variant])
    );
```

Then, further down, replace only the message `<span>` at `:47-49`:

```tsx
      <span className={styles.message}>
        {intl.formatMessage(messageByVariant[variant])}
      </span>
```

with:

```tsx
      <span className={styles.message}>{body}</span>
```

Everything else in the file is **unchanged**: the `<div className={styles.banner} role="status" data-variant={variant}>` wrapper at `:29`, the inline
warning-triangle `<svg>` at `:30-46`, and `export default injectIntl(DRepErrorBanner);`.

The two-line guard comment at `:14-15` is **deleted** by the replacement above —
leaving it would be change-history, which the comment convention forbids.

Copy notes:

- The `defaultMessage` uses **double quotes** because the string contains an
  apostrophe (`Couldn't`). Precedent: `DRepDirectoryBanner.tsx:52`. In ICU, an
  apostrophe not followed by `{`, `}` or `#` is a literal apostrophe — this
  string is safe as written.
- The copy is contract. Do not reword, do not drop the `!!!`, do not rename the
  `{Retry}` / `{time}` placeholders.

**SCSS addition** — append to
`source/renderer/app/components/governance/_shared/DRepErrorBanner.scss`
(alphabetical inside the block; 0 new stylelint errors — the file stays at 4):

```scss
.retryLink {
  font-size: inherit;
  white-space: nowrap;
}
```

Do **not** add a height, a `text-overflow: ellipsis` or a line clamp to `.banner`
or `.message`: the ja-JP rendering of this string expands 30–60 % and must be
allowed to wrap onto ≥2 lines and reflow vertically.

## Step 8 — Replace the hand-rolled retained-data banner

**File:** `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`

Replace the block at `:324-341`:

```tsx
            {showErrorBanner && error && (
              <div className={styles.errorBanner}>
                <div>
                  <p className={styles.errorMessage}>
                    {intl.formatMessage(messages.error)}
                  </p>
                  <p className={styles.errorDetails}>{error.message}</p>
                  {error.details && (
                    <p className={styles.errorDetails}>{error.details}</p>
                  )}
                </div>
                <Button
                  label={intl.formatMessage(messages.retry)}
                  onClick={onRefresh}
                  skin={ButtonSkin}
                />
              </div>
            )}
```

with:

```tsx
            {showErrorBanner && (
              <DRepErrorBanner
                variant="refreshFailed"
                retryLabel={intl.formatMessage(messages.retry)}
                onRetry={onRefresh}
                lastFetchedAt={lastFetchedAt}
              />
            )}
```

`DRepErrorBanner` is already imported at `:10`. `messages.retry` and
`messages.error` both stay declared — `messages.error` is still used by the
`Failed` arm at `:257`, which is **untouched**.

**SCSS:** delete the now-dead `.errorBanner` block from
`source/renderer/app/components/governance/drep-directory/DRepDirectory.scss`
(`:31-40`):

```scss
.errorBanner {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 16px;
  padding: 16px;
  border: 1px solid var(--theme-error-color, #d32f2f);
  border-radius: 8px;
  background: rgba(211, 47, 47, 0.06);
}
```

Delete the whole block and the blank line that followed it. Reorder nothing else
in the file.

## Step 9 — Move the stale-while-refresh badge into the directory header

**9a. Delete** the badge block from `DRepDirectory.tsx` (`:342-347`):

```tsx
            {refreshState === GovernanceRefreshState.Refreshing && (
              <div className={styles.refreshingBadge}>
                <LoadingSpinner />
                {intl.formatMessage(messages.refreshing)}
              </div>
            )}
```

**9b.** Delete the now-unused `refreshing` message entry from
`DRepDirectory.tsx` (`:60-64`):

```tsx
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Refreshing state badge label',
  },
```

**9c.** Delete the now-unused `LoadingSpinner` import from `DRepDirectory.tsx`
(`:11`):

```tsx
import LoadingSpinner from '../../widgets/LoadingSpinner';
```

An unused import is an eslint **error**, so this deletion is mandatory. Confirm
with `grep -n LoadingSpinner` on the file — it must return nothing.

**9d.** Delete the now-dead `.refreshingBadge` block from `DRepDirectory.scss`
(`:42-49`):

```scss
.refreshingBadge {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  color: var(--theme-text-secondary, #6b7384);
  padding: 4px 0;
}
```

**9e.** **File:** `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx`

Add the spinner import after the `LinkSkin` import at `:7`:

```tsx
import LoadingSpinner from '../../widgets/LoadingSpinner';
```

Add a `refreshing` message to the `defineMessages` block, immediately after the
`lastUpdated` entry (`:21-25`):

```tsx
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Accessible label of the refresh-in-flight badge',
  },
```

Replace the `lastUpdated` paragraph at `:103-109`:

```tsx
      {lastFetchedAt && timeAgo !== null && (
        <p className={styles.lastUpdated}>
          {intl.formatMessage(messages.lastUpdated, {
            time: timeAgo,
          })}
        </p>
      )}
```

with:

```tsx
      {lastFetchedAt && timeAgo !== null && (
        // A refresh only reaches this component with retained data, so the
        // timestamp the badge annotates is always present on that path.
        <div className={styles.lastUpdated}>
          {intl.formatMessage(messages.lastUpdated, {
            time: timeAgo,
          })}
          {isRefreshing && (
            <span className={styles.refreshingBadge} role="status">
              <LoadingSpinner />
              {intl.formatMessage(messages.refreshing)}
            </span>
          )}
        </div>
      )}
```

The element changes from `<p>` to `<div>` because `LoadingSpinner` renders a
`<div>`, which cannot legally nest inside a `<p>`. `.lastUpdated`'s existing
declarations already work for a `div`; **do not edit that block**.

`isRefreshing` is already a declared prop (`:61`, destructured at `:80`) — no
prop-signature change.

**9f. SCSS:** append to
`source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss`
(alphabetical inside the block; 0 new stylelint errors — the file stays at 10):

```scss
.refreshingBadge {
  align-items: center;
  display: inline-flex;
  gap: 6px;
  margin-left: 8px;
  vertical-align: middle;
}
```

Use `<LoadingSpinner />` bare, exactly as the deleted block did — do not add a
sizing class. Its shared sizing is unchanged by this move; visual polish is a
Storybook pass that cannot run in this container.

## Step 10 — Add the new copy to both locale catalogs

**Files:** `source/renderer/app/i18n/locales/en-US.json` and
`source/renderer/app/i18n/locales/ja-JP.json`

**One new key.** Both catalogs are strictly sorted, so the insert position is
exact.

> **Anchor correction.** The PRD's i18n inventory says this key "sorts after
> `governance.drepDirectory.error`". That is wrong under `Array.prototype.sort()`:
> `...error.rankingUnavailable` < `...error.refresh`, because `a` < `e` at the
> first differing character. The verified position is **immediately after
> `governance.drepDirectory.error.rankingUnavailable`** (line `357` in both
> catalogs) and **before `governance.drepDirectory.filter.active`** (line `358`).

**en-US.json** — after line `357`, so the region reads:

```json
  "governance.drepDirectory.error": "!!!Could not load DRep data.",
  "governance.drepDirectory.error.rankingUnavailable": "!!!Voting power data unavailable this refresh. Ranking-based filters disabled.",
  "governance.drepDirectory.error.refresh": "!!!Couldn't refresh DRep data. {Retry}. Showing last successful snapshot from {time}.",
  "governance.drepDirectory.filter.active": "!!!Status",
```

**ja-JP.json** — same position, so the region reads:

```json
  "governance.drepDirectory.error": "!!!DRepデータを読み込めませんでした。",
  "governance.drepDirectory.error.rankingUnavailable": "!!!今回の更新では投票権データを利用できません。ランキングに基づくフィルターは無効になります。",
  "governance.drepDirectory.error.refresh": "!!!DRepデータを更新できませんでした。{Retry}。{time}時点の最後に成功したスナップショットを表示しています。",
  "governance.drepDirectory.filter.active": "!!!ステータス",
```

Rules for this step:

- Both strings keep the leading `!!!`. The ja-JP string is a slice-level
  placeholder and stays marked until the release-end manual copy review.
- The `{Retry}` and `{time}` placeholders appear in **both** locales, spelled
  identically.
- **Never** run prettier on either catalog.
- After editing, run `yarn i18n:manage` (it is `i18n:extract && i18n:check`).
  It **writes** `translations/messages.json` and may touch
  `source/renderer/app/i18n/locales/defaultMessages.json` — those writes are
  expected output, not drift. Then check `git status`: if it modified anything
  else, revert that path with `git restore <path>` (never `git stash`).

## Step 11 — Storybook

**File:** `storybook/stories/governance/DRepDirectory.stories.tsx`
(**pre-drifted for prettier** — hand-match style, do not run `--write`.)

**11a.** Add a timeout fixture next to `REFRESH_ERROR` (`:115-119`):

```tsx
const TIMEOUT_ERROR: DirectoryError = {
  message: 'DRep registration query timed out.',
  type: 'TIMEOUT',
};
```

**11b.** Add one key to `DIRECTORY_STATE_OPTIONS` (`:242-248`), keeping the
existing keys and their values untouched:

```tsx
const DIRECTORY_STATE_OPTIONS = {
  Loaded: 'loaded',
  Empty: 'empty',
  Loading: 'loading',
  Refreshing: 'refreshing',
  'Refresh failed': 'refreshFailed',
  Error: 'error',
};
```

**11c.** Add the matching case to `resolveDirectoryState` (`:257-289`),
immediately after the `'refreshing'` case:

```tsx
    case 'refreshFailed':
      return {
        refreshState: GovernanceRefreshState.Loaded,
        entries: baseEntries,
        error: TIMEOUT_ERROR,
      };
```

**11d.** Add exactly **one** discrete story, immediately after the existing
`'Refreshing'` story (which ends at `:432`):

```tsx
  .add('Refresh failed — retained snapshot', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      baseEntries,
      TIMEOUT_ERROR,
      DEFAULT_SYNC_STATE,
      true
    )
  )
```

No other story is added or renamed:

- the existing `'Loading'` story (`:423`) now exercises the skeleton list;
- the existing `'Refreshing'` story (`:424-432`) now exercises the relocated
  badge in the header;
- the `'Connected flow'` integrated story (`:303-409`) is untouched and must
  still run.

No local `IntlProvider`, no per-locale variant, no second decorator, no second
`storiesOf` block.

## Step 12 — `tests/jest/governance/GovernanceQueryService.spec.ts`

(**Pre-drifted for prettier** — hand-match style, do not run `--write`.)

**Do not edit** `it('pins the per-phase budgets to the design-token contract')`
(`:472-477`) or either timer-driven test (`:451-470`, `:479-504`).

**12a.** Add one test to `describe('successful tuple parsing')`, immediately
after `it('caches lastSuccessfulData after a successful fetch', …)` (ends `:324`;
the enclosing `describe` closes at `:325`, so the new test goes above that line):

```ts
    it('reports the measured registration duration as a plain millisecond number', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepRegistrations();

      expect(typeof result.elapsedMs).toBe('number');
      expect(result.elapsedMs).toBeGreaterThanOrEqual(0);
      expect(result.elapsedMs).toBeLessThan(
        (GovernanceQueryService as any).REGISTRATION_TIMEOUT_MS
      );
    });
```

The `as any` reach into the private budget constant mirrors the existing
budget-pinning test at `:473-476` — it reads the constant, it does not change it.

**12b.** Add one test to `describe('stake distribution phase')`, immediately
after `it('skips the two voting sentinels without creating entries', …)`
(ends `:617`):

```ts
    it('reports the measured stake duration as a plain millisecond number', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
      );

      const result = await service.fetchDRepStake();

      expect(typeof result.elapsedMs).toBe('number');
      expect(result.elapsedMs).toBeGreaterThanOrEqual(0);
    });
```

**Expected:** 38 → **40** tests, all green.

## Step 13 — `tests/jest/governance/logDRepStateSnapshot.spec.ts`

**13a.** The fixture at `:38-54` is annotated `: DRepListQueryPayload` and will
fail `yarn compile` without the new field. Add it after `fetchedAt`:

```ts
  epoch: 512,
  fetchedAt: 1_750_000_000_000,
  elapsedMs: 1_234,
};
```

**13b.** Add one test to the `describe('logDRepStateSnapshot')` block,
immediately after `it('writes the public directory payload with drepIds retained', …)`:

```ts
  it('carries the query duration into the snapshot as a plain number', () => {
    logDRepStateSnapshot(publicPayload);

    const parsed = JSON.parse(fs.readFileSync(SNAPSHOT_PATH, 'utf-8'));

    expect(typeof parsed.data.elapsedMs).toBe('number');
    expect(parsed.data.elapsedMs).toBe(1_234);
  });
```

Match the existing tests' read/parse idiom in this file exactly — if they read
the file through a helper, use the helper instead of the inline `readFileSync`
above.

**Expected:** 5 → **6** tests, all green. The existing
`it('never contains user vote or delegation fields', …)` must stay green
unmodified — that is the proof `elapsedMs` did not widen the payload into
anything identifying.

## Step 14 — `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`

Three tests are rewritten in place and one is added. **Do not touch the snapshot
test** (`renders exactly one category badge per card (snapshot)`) and **never run
jest with `-u`**.

**14a.** Replace `it('renders a loading indicator when in Loading refresh state', …)`
(`:313-320`) with:

```tsx
  it('renders the first-load skeleton list instead of a directory row', () => {
    const { container } = renderComponent({
      drepList: [],
      refreshState: GovernanceRefreshState.Loading,
    });

    expect(screen.getByLabelText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(container.querySelectorAll('.skeletonCard')).toHaveLength(25);
    expect(screen.queryByText('!!!Voting power:')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!No DReps found on-chain.')).not.toBeInTheDocument();
  });
```

`jest-css-modules-transform` maps a CSS-module class to its literal local name,
so `.skeletonCard` is a valid query (the existing snapshot shows
`class="badge threshold"`).

**14b.** Replace `it('keeps the retained list visible with a non-blocking error banner after refresh failure', …)`
(`:251-270`) with:

```tsx
  it('replaces the retained-data banner text with the snapshot-age copy', () => {
    renderComponent({
      error: {
        message:
          'Showing the last successful directory snapshot while refresh retries.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(screen.getByText(/Couldn't refresh DRep data/)).toBeInTheDocument();
    expect(
      screen.getByText(/Showing last successful snapshot from a minute ago/)
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Retry')).toBeInTheDocument();
    expect(
      screen.queryByText('!!!Could not load DRep data.')
    ).not.toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });
```

`renderComponent` passes `lastFetchedAt={Date.now() - 60_000}` (`:165`), which
`moment().fromNow()` renders as `a minute ago`. Match the **whole banner
sentence**, not `/a minute ago/` alone: the header line
`!!!Last updated a minute ago` (`DRepDirectoryBanner.tsx:103-109`) renders in this
state too, so the bare fragment matches two elements and `getByText` throws.

**14c.** Replace `it('surfaces actionable error details in the non-blocking error banner', …)`
(`:272-288`) with its inverse — under the new contract raw CLI text must **not**
reach the user on the retained path:

```tsx
  it('keeps raw query text out of the retained-snapshot banner', () => {
    renderComponent({
      error: {
        details: 'Missing: --mainnet | --testnet-magic NATURAL',
        message:
          'Showing the last successful directory snapshot while refresh retries.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(
      screen.queryByText('Missing: --mainnet | --testnet-magic NATURAL')
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        'Showing the last successful directory snapshot while refresh retries.'
      )
    ).not.toBeInTheDocument();
    expect(screen.getByText(/Couldn't refresh DRep data/)).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });
```

**14d.** Add one test immediately after 14c — the timeout leg of AC-3. The banner
is driven by the arriving `Timeout` error, **not** by any renderer clock, so this
test uses no timers:

```tsx
  it('shows the retained-snapshot banner when the refresh times out', () => {
    renderComponent({
      error: {
        message: 'DRep registration query timed out.',
        type: 'TIMEOUT',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(screen.getByText(/Couldn't refresh DRep data/)).toBeInTheDocument();
    expect(
      screen.queryByText('DRep registration query timed out.')
    ).not.toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });
```

The two `Failed`-arm tests (`:211-227`, `:229-249`) still assert `error.message`
and `error.details` render — that arm is unchanged and those tests must stay
green **unmodified**.

**Expected:** 60 → **61** tests, 1 snapshot unchanged.

## Step 15 — `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx`

No existing test asserts the refreshing badge anywhere in the repo (verified), so
this is a net coverage addition, not a move.

**15a.** Thread `isRefreshing` through the `renderBanner` helper (`:9-43`): add
`isRefreshing = false` to the destructured defaults, `isRefreshing?: boolean;` to
its inline type, and replace the hard-coded `isRefreshing={false}` at `:31` with
`isRefreshing={isRefreshing}`.

**15b.** Add two tests to the `describe('DRepDirectoryBanner')` block:

```tsx
  it('renders the refreshing badge beside the last-updated timestamp', () => {
    renderBanner({ isRefreshing: true });

    expect(screen.getByText(/Last updated/)).toBeInTheDocument();
    expect(screen.getByText('!!!Refreshing…')).toBeInTheDocument();
  });

  it('renders no refreshing badge while no refresh is in flight', () => {
    renderBanner();

    expect(screen.getByText(/Last updated/)).toBeInTheDocument();
    expect(screen.queryByText('!!!Refreshing…')).not.toBeInTheDocument();
  });
```

**Expected:** 7 → **9** tests, all green.

---

## Verification for task-123

Run from `/home/node/.claude/jobs/3bad97d1/wt-slice-8`. Report **measured
`baseline → actual` deltas**, never totals alone, and never report green on a
command you did not run.

### Per-shard checks (run the row for the steps you just finished)

| Steps | Command |
|---|---|
| 1–3 | `yarn compile` &nbsp;·&nbsp; `node_modules/.bin/jest --no-coverage --runInBand tests/jest/governance/GovernanceQueryService.spec.ts` (expect 38 unchanged **at this point**, since Step 12 has not run yet) |
| 4–9 | `yarn compile` &nbsp;·&nbsp; `yarn lint` &nbsp;·&nbsp; `node_modules/.bin/stylelint source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.scss` (expect **0**) |
| 10–11 | `yarn i18n:manage` &nbsp;·&nbsp; `node_modules/.bin/jest --no-coverage --runInBand tests/jest/i18n/preliminaryCopyMarkers.spec.ts` &nbsp;·&nbsp; `yarn storybook:build` |
| 12–15 | the full matrix below |

### Full matrix (before the task is called done)

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-slice-8

# 1. Typecheck. The precompile hook regenerates the gitignored *.scss.d.ts,
#    including the new DRepDirectorySkeleton.scss.d.ts.
yarn compile
#    expect: exit 0. `git status` must stay clean of *.scss.d.ts (gitignored).

# 2. Main-process query service.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/governance/GovernanceQueryService.spec.ts
#    baseline 38 -> expect 40 (+2, Step 12). The budget-pinning test is untouched.

# 3. Snapshot writer — the payload widened, so this file is in the blast radius.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/governance/logDRepStateSnapshot.spec.ts
#    baseline 5 -> expect 6 (+1, Step 13).

# 4. Directory component + its single snapshot.
node_modules/.bin/jest --no-coverage --runInBand source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx
#    baseline 60 tests / 1 snapshot -> expect 61 tests / 1 snapshot UNCHANGED.

# 5. Directory header.
node_modules/.bin/jest --no-coverage --runInBand source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx
#    baseline 7 -> expect 9 (+2, Step 15).

# 6. Untouched neighbours that must stay green.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/governance/GovernanceStore.spec.ts source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx
#    expect 56 and 9, unchanged.

# 7. Sanitization floor — mandatory in every slice-8 task.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/security/governance-sanitization.spec.ts
#    expect 39, unchanged, green.

# 8. i18n markers and both-catalog parity.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/i18n/preliminaryCopyMarkers.spec.ts
#    expect 5, unchanged, green. governance.drepDirectory.* goes 66 -> 67 per catalog.

# 9. i18n pipeline. WRITES translations/messages.json — expected output.
yarn i18n:manage
#    expect exit 0. Inspect `git status`; `git restore` anything unintended.

# 10. Lint. Errors are the gate; warnings are not.
yarn lint
#    expect exit 0, 0 errors. The warning count rises because a new file landed
#    under source/ (DRepDirectorySkeleton.tsx) — that is not a regression.

# 11. Stylelint. NOT a gate for this slice; measure and record only.
yarn stylelint
#    baseline 118 -> expect 111. The -7 is exactly the two dead selector blocks
#    deleted in Steps 8 and 9d. No --fix was run and no declaration was reordered.
node_modules/.bin/stylelint source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.scss
#    expect 0 — the new stylesheet is clean at birth.

# 12. Storybook — green at HEAD, so run it rather than waive it.
yarn storybook:build
#    expect exit 0.

# 13. Zero-renderer-timer property. Both commands must return NOTHING.
#     The recursive scan is the load-bearing one: `git diff` sees only tracked
#     content, and DRepDirectorySkeleton.{tsx,scss} are still untracked here.
grep -rn 'setTimeout\|setInterval' source/renderer/app/components/governance source/renderer/app/containers/governance storybook/stories/governance
git diff HEAD -- source/renderer storybook | grep -nE '^\+.*(setTimeout|setInterval)'
```

Also confirm by inspection, and state it in the task report:

- the shared-design-tokens refresh budget table (`:95-102`) is present and
  unedited, and `GovernanceQueryService.ts:52-56` still cites it — the
  design-token half of the timing-budget criterion (Step 3);
- no renderer file gained a `setTimeout` / `setInterval`;
- `REGISTRATION_TIMEOUT_MS` / `STAKE_TIMEOUT_MS` and `_runCliQuery`'s `setTimeout`
  are byte-identical to HEAD;
- `getLastSuccessfulData()` still has no IPC consumer.

## Files this task edits

```
source/common/types/governance.types.ts
source/common/ipc/api.ts
source/main/governance/GovernanceQueryService.ts
source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.tsx   (new)
source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.scss  (new)
source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx
source/renderer/app/components/governance/drep-directory/DRepDirectory.scss
source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx
source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss
source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx
source/renderer/app/components/governance/_shared/DRepErrorBanner.scss
source/renderer/app/i18n/locales/en-US.json
source/renderer/app/i18n/locales/ja-JP.json
storybook/stories/governance/DRepDirectory.stories.tsx
tests/jest/governance/GovernanceQueryService.spec.ts
tests/jest/governance/logDRepStateSnapshot.spec.ts
source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx
source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx
translations/messages.json                      (written by yarn i18n:manage)
source/renderer/app/i18n/locales/defaultMessages.json  (written by yarn i18n:manage, if it changes)
```

Prettier-format the two new files **and** the edited files that are
prettier-clean at HEAD (`.scss` **is** in scope — `.prettierignore` re-includes
`!*.scss` under `source/`):

```bash
/home/node/.claude/jobs/3bad97d1/wt-slice-8/node_modules/.bin/prettier --write \
  source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectorySkeleton.scss \
  source/common/types/governance.types.ts \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.scss \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss \
  source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx \
  source/renderer/app/components/governance/_shared/DRepErrorBanner.scss \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx \
  tests/jest/governance/logDRepStateSnapshot.spec.ts
```

**Excluded from that list on purpose** — the five pre-drifted files, hand-matched
instead: `source/common/ipc/api.ts`,
`source/main/governance/GovernanceQueryService.ts`,
`.../drep-directory/DRepDirectory.tsx`,
`storybook/stories/governance/DRepDirectory.stories.tsx`,
`tests/jest/governance/GovernanceQueryService.spec.ts` — plus the locale catalogs
and `translations/messages.json`, which are tool-managed.

---

# task-124 — Selfnode CLI-unsupported empty-state copy and IPC payload

**Interaction mode:** `autonomous`. **Commit type:** `feat`.
**Precondition:** task-123 is complete and its verification matrix is green.

## 1. What this task is

Close the last unreachable UI state. Today a selfnode user sees the raw
main-process string *"DRep data is unavailable in selfnode mode. A synced node is
required."* rendered straight into the generic error arm. This task routes
`SelfnodeCliUnsupported` to a designed empty state with an unavailability badge,
before that generic arm can ever fire.

1. `DRepEmptyStateVariant` gains `'selfnode'`, with the designed copy and a
   plain-markup unavailability badge (icon **and** textual label) under a newly
   minted message id.
2. `DRepDirectory.renderContent()` gains a selfnode arm ahead of the `Failed`
   arm, and `showNoSyncFallback` is reconciled with it.
3. Two new i18n keys in both catalogs, one Storybook state + story, four
   component tests, one main-process no-spawn test, one container remount test.
4. Two one-line design-doc reconciliations.
5. The slice findings note is created.

## 2. What this task is NOT — read this before you start

- **`DRepStatus` gains no member and `DRepStatusBadge` is not touched, not
  imported and not extended.** The selfnode indicator is a *directory-level*
  state, not a per-DRep on-chain status. `DRepStatusBadge.tsx:20-29` is an
  exhaustive `Record<DRepStatus, string>` over a closed
  `'active' | 'inactive'` union that a locked invariant forbids widening. The
  badge you build is plain markup inside the empty state.
- **No IPC payload change, despite the task title.** The wire is already
  complete: `GovernanceQueryErrorType.SelfnodeCliUnsupported` exists
  (`source/common/types/governance.types.ts:163`), is produced in exactly one
  place (`GovernanceQueryService.ts:209-213`), crosses IPC intact as a plain
  object (`source/main/ipc/governanceChannel.ts:32-45`) and is normalised by the
  store (`GovernanceStore.ts:562+`). **Add no field, no enum member, no channel.**
  task-123 owned the only payload widening in this slice.
- **Do not change the `_assertQueryable()` throw site or its message.**
- **No mount guard in `DRepDirectoryPage.tsx`.** `componentDidMount` refreshes
  when `refreshState` is `Idle` **or** `Failed` (`:32-38`), so a selfnode user
  re-fires the query on every mount. That is *not* a retry loop:
  `_doFetchDRepRegistrations()` calls `_assertQueryable()` as its **first
  statement** (`:224-225`), which throws before any `Promise.all` and therefore
  before any `spawn` — the "retry" costs one synchronous throw and issues zero
  CLI invocations. Pin that property with a test instead of adding node-capability
  knowledge to a container that should not have it.
- **No partial directory, no cached-data fallback, no retained snapshot on the
  selfnode path.**
- **No suppression or duplication of the syncing banner** (`DRepDirectory.tsx:392-417`).
- **No stylelint sweep** of `DRepEmptyState.scss` (3 pre-existing errors) or any
  other file.
- **Do not edit any closed slice's tracker text**, and do not delete the
  shared-design-tokens §1 selfnode table row.

## 3. Locked invariants this change must not break (inlined — do not look them up)

1. **Local-first.** Discovery data comes only from the local node via the
   main-process `GovernanceQueryService`. No hosted explorers, indexers, GovTool,
   Koios, Blockfrost or public governance APIs.
2. **Sanitization floor.** No DRep id, no `abstain` / `no_confidence` literal, no
   CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store
   payload. The selfnode path logs nothing new; the existing failure log records
   `errorType` only (`GovernanceStore.ts:349-351`) and that shape is preserved.
   Re-assert `tests/jest/security/governance-sanitization.spec.ts` green.
3. **CLI discipline.** Bulk `--all-dreps` once per refresh; per-DRep invocations
   are forbidden. A selfnode refresh must issue **zero** CLI invocations.
4. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!`.
5. **No partial directory for selfnode.** When the service reports selfnode
   CLI-unsupported, the renderer shows the empty state — never a partially
   populated directory. The selfnode state replaces the **entire** list area.
6. **DRep status grounding.** Canonical on-chain status is `active | inactive`;
   `expiring` is renderer-derived display state; `retired` is deferred. **Do not
   widen the canonical union.** This invariant is the direct cause of the
   plain-markup badge design.
7. **`Abstain` / `No Confidence` are form-only sentinels**, never DRep directory
   entries. The empty-state copy must stay scoped to registered DReps and must
   never suggest the directory is where to find them. The copy below already
   complies — do not embellish it.
8. **Smallest truthful change.** `DRepEmptyState` has an explicitly-named
   extension point; extend the union and add one early-return body. No new
   component, no structural rewrite.

## 4. Verified line anchors (re-verified at `0cdcab581`)

| Anchor | What is there |
|---|---|
| `.../_shared/DRepEmptyState.tsx:12-52` | the `defineMessages` block |
| `.../_shared/DRepEmptyState.tsx:54-56` | the guard comment + `DRepEmptyStateVariant` |
| `.../_shared/DRepEmptyState.tsx:73-103` | the `noResults` early return |
| `.../_shared/DRepEmptyState.tsx:125-129` | the `noSync` fall-through default |
| `.../drep-directory/DRepDirectory.tsx:228-238` | `hasRetainedData`, `showErrorBanner`, `showNoSyncFallback` |
| `.../drep-directory/DRepDirectory.tsx:250-251` | the `showNoSyncFallback` arm |
| `.../drep-directory/DRepDirectory.tsx:253-269` | the `Failed` arm (untouched) |
| `source/main/governance/GovernanceQueryService.ts:208-222` | `_assertQueryable()` |
| `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx:39-103` | `buildGovernanceStore` + `renderPage` |
| en-US / ja-JP `:355` | `governance.drepDirectory.empty.noSync` |
| en-US / ja-JP `:388` | `governance.drepDirectory.status.inactive` |

## 5. Decisions already made — implement them, do not re-derive

- **Two new message ids, both minted here:**
  `governance.drepDirectory.empty.selfnode` (the empty-state body) and
  `governance.drepDirectory.status.selfnodeUnavailable` (the badge label). The
  second is new — it exists nowhere in the design's microcopy table, and this
  task adds it there.
- **The badge is plain markup inside the empty state**: an icon plus a textual
  label, because colour must never be the sole indicator. It reuses the same
  inline warning-triangle `<svg>` shape already used by `DRepErrorBanner.tsx:30-46`
  and the syncing banner.
- **The selfnode arm fires on the error type alone**, regardless of retained
  data, so a selfnode failure can never leave a partially populated directory on
  screen.
- **`showNoSyncFallback` excludes selfnode outright**, not just on its `Failed`
  leg — otherwise a selfnode error arriving while the node is out of sync would
  route to the wrong empty state.
- Stylelint contract: the new declarations are alphabetical inside their blocks,
  so this task adds **0** errors. The repo total stays at **111** (task-123's
  measured post-state). No `--fix`, no reordering.

---

## Step 1 — Add the `selfnode` variant to `DRepEmptyState`

**File:** `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx`

**1a.** Insert two entries into the `defineMessages` block immediately after the
`noSync` entry (which ends at `:18`), before `noResults`:

```tsx
  selfnode: {
    id: 'governance.drepDirectory.empty.selfnode',
    defaultMessage:
      '!!!DRep directory data is unavailable on the selfnode cluster.',
    description: 'Directory empty state on the selfnode cluster',
  },
  selfnodeUnavailable: {
    id: 'governance.drepDirectory.status.selfnodeUnavailable',
    defaultMessage: '!!!DRep data unavailable on selfnode',
    description:
      'Directory-level unavailability badge rendered inside the selfnode empty state',
  },
```

**1b.** Replace the guard comment and union at `:54-56`:

```tsx
// noSync, noResults and noFavorites ship for now; the designed selfnode
// variant joins this union when its owning slice lands.
export type DRepEmptyStateVariant = 'noSync' | 'noResults' | 'noFavorites';
```

with:

```tsx
export type DRepEmptyStateVariant =
  | 'noSync'
  | 'noResults'
  | 'noFavorites'
  | 'selfnode';
```

The two-line guard comment is **deleted**, not amended — leaving it would be
change-history, which the comment convention forbids.

**1c.** Add the variant body as the **first** early return in the component,
immediately after the opening `}: Props) {` line (`:72`) and **before**
`if (variant === 'noResults')`:

```tsx
  if (variant === 'selfnode') {
    return (
      <div className={styles.container} data-variant={variant}>
        <span className={styles.unavailableBadge}>
          <svg
            className={styles.unavailableIcon}
            aria-hidden="true"
            width="14"
            height="14"
            viewBox="0 0 16 16"
          >
            <path
              d="M8 1.5 15 14H1L8 1.5z"
              fill="none"
              stroke="currentColor"
              strokeWidth="1.5"
              strokeLinejoin="round"
            />
            <path d="M8 6v4" stroke="currentColor" strokeWidth="1.5" />
            <circle cx="8" cy="12" r="0.9" fill="currentColor" />
          </svg>
          {intl.formatMessage(messages.selfnodeUnavailable)}
        </span>
        <p className={styles.message}>
          {intl.formatMessage(messages.selfnode)}
        </p>
      </div>
    );
  }
```

The `noSync` fall-through default at `:125-129` stays the default — do not turn
it into an explicit branch.

## Step 2 — Add the badge styles

**File:** `source/renderer/app/components/governance/_shared/DRepEmptyState.scss`

**Append** these two blocks at the end of the file. Both are already in
alphabetical property order, so the file stays at its 3 pre-existing errors.
**Do not reorder anything already in the file.**

```scss
.unavailableBadge {
  align-items: center;
  background: var(--badge-disabled-bg, rgba(107, 115, 132, 0.12));
  border-radius: 4px;
  color: var(--theme-text-secondary, #6b7384);
  display: inline-flex;
  font-size: 13px;
  font-weight: 500;
  gap: 6px;
  line-height: 1;
  padding: 4px 8px;
}

.unavailableIcon {
  flex-shrink: 0;
}
```

Verify with
`node_modules/.bin/stylelint source/renderer/app/components/governance/_shared/DRepEmptyState.scss`
— expect **3**, unchanged.

## Step 3 — Route `SelfnodeCliUnsupported` in the directory

**File:** `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`
(**pre-drifted for prettier** — hand-match style, do not run `--write`.)

**3a.** Replace the predicate block at `:231-238`:

```tsx
  // While syncing, an empty or unavailable directory is expected — fall back
  // to the noSync empty state instead of a bare error or "No DReps found".
  const showNoSyncFallback =
    !isNodeInSync &&
    !hasRetainedData &&
    (refreshState === GovernanceRefreshState.Loaded ||
      (refreshState === GovernanceRefreshState.Failed &&
        error?.type !== GovernanceQueryErrorType.SelfnodeCliUnsupported));
```

with:

```tsx
  const isSelfnodeUnsupported =
    error?.type === GovernanceQueryErrorType.SelfnodeCliUnsupported;

  // While syncing, an empty or unavailable directory is expected — fall back
  // to the noSync empty state instead of a bare error or "No DReps found".
  // Selfnode is a node-capability failure rather than a sync gap, so it owns
  // its own arm and is excluded here on every refresh state.
  const showNoSyncFallback =
    !isNodeInSync &&
    !hasRetainedData &&
    !isSelfnodeUnsupported &&
    (refreshState === GovernanceRefreshState.Loaded ||
      refreshState === GovernanceRefreshState.Failed);
```

The `GovernanceQueryErrorType` import at `:19` is already present and stays.

**3b.** Insert the new arm into the `switch (true)` **between** the
`showNoSyncFallback` arm (`:250-251`) and the `Failed` arm (`:253`):

```tsx
      case showNoSyncFallback:
        return <DRepEmptyState variant="noSync" />;

      case isSelfnodeUnsupported:
        return <DRepEmptyState variant="selfnode" />;

      case refreshState === GovernanceRefreshState.Failed:
```

`DRepEmptyState` is already imported at `:9`. The `Failed` arm body and every
arm below it are **unchanged**.

## Step 4 — Add the two new keys to both locale catalogs

**Files:** `source/renderer/app/i18n/locales/en-US.json` and
`source/renderer/app/i18n/locales/ja-JP.json`

Both catalogs are strictly `Array.prototype.sort()` ordered. Two insert points
per file.

**Insert point A** — immediately after `governance.drepDirectory.empty.noSync`
(line `355`), before `governance.drepDirectory.error`.

en-US region after the edit:

```json
  "governance.drepDirectory.empty.noSync": "!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.",
  "governance.drepDirectory.empty.selfnode": "!!!DRep directory data is unavailable on the selfnode cluster.",
  "governance.drepDirectory.error": "!!!Could not load DRep data.",
```

ja-JP region after the edit:

```json
  "governance.drepDirectory.empty.noSync": "!!!ノードは同期中です。DRepデータはノードが最新ブロックに到達すると利用できるようになります。",
  "governance.drepDirectory.empty.selfnode": "!!!selfnodeクラスターではDRepディレクトリのデータを利用できません。",
  "governance.drepDirectory.error": "!!!DRepデータを読み込めませんでした。",
```

**Insert point B** — immediately after `governance.drepDirectory.status.inactive`
(line `388`), before `governance.drepDirectory.syncing`.

en-US region after the edit:

```json
  "governance.drepDirectory.status.inactive": "!!!Inactive",
  "governance.drepDirectory.status.selfnodeUnavailable": "!!!DRep data unavailable on selfnode",
  "governance.drepDirectory.syncing": "!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes.",
```

ja-JP region after the edit:

```json
  "governance.drepDirectory.status.inactive": "!!!非アクティブ",
  "governance.drepDirectory.status.selfnodeUnavailable": "!!!DRepデータ利用不可",
  "governance.drepDirectory.syncing": "!!!ノードは同期中です({progress}%)。同期が完了するまでDRepリストは不完全な場合があります。",
```

Rules for this step:

- The ja-JP badge label `!!!DRepデータ利用不可` is **locked copy** taken verbatim
  from the design's status-badge table — do not paraphrase it.
- The ja-JP empty-state string is a slice-level placeholder and keeps its `!!!`.
- Line numbers shift by one after insert point A; find insert point B by its
  neighbouring **keys**, not by line number.
- **Never** run prettier on either catalog.
- Then run `yarn i18n:manage`; its writes to `translations/messages.json` and
  `defaultMessages.json` are expected output. Check `git status` and
  `git restore` anything unintended (never `git stash`).

## Step 5 — Storybook

**File:** `storybook/stories/governance/DRepDirectory.stories.tsx`
(**pre-drifted for prettier** — hand-match style, do not run `--write`.)

**5a.** Add a fixture next to `SOCKET_ERROR` (`:110-113`):

```tsx
const SELFNODE_ERROR: DirectoryError = {
  message: 'DRep data is unavailable in selfnode mode. A synced node is required.',
  type: 'SELFNODE_CLI_UNSUPPORTED',
};
```

**5b.** Add one key to `DIRECTORY_STATE_OPTIONS`, after the `'Refresh failed'`
key task-123 added:

```tsx
  'Selfnode unavailable': 'selfnode',
```

**5c.** Add the matching case to `resolveDirectoryState`, immediately before the
`'error'` case:

```tsx
    case 'selfnode':
      return {
        refreshState: GovernanceRefreshState.Failed,
        entries: [],
        error: SELFNODE_ERROR,
      };
```

**5d.** Add one discrete story, immediately after the existing `'Error'` story
(`:420-422`):

```tsx
  .add('Selfnode unavailable', () =>
    renderCentered(GovernanceRefreshState.Failed, [], SELFNODE_ERROR)
  )
```

No local `IntlProvider`, no per-locale variant, no second decorator. The
`'Connected flow'` story must still run.

## Step 6 — Component tests

**File:** `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`

Add these four tests to the root `describe('DRepDirectory')` block, immediately
after `it('keeps the retained list without the fallback when syncing with data present', …)`.
Do not modify any existing test, and do not touch the snapshot.

```tsx
  it('renders the selfnode empty state instead of the raw query error', () => {
    renderComponent({
      drepList: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      refreshState: GovernanceRefreshState.Failed,
    });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText('!!!DRep data unavailable on selfnode')
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/unavailable in selfnode mode/)
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Could not load DRep data.')
    ).not.toBeInTheDocument();
  });

  it('prefers the selfnode empty state over the noSync fallback while the node is syncing', () => {
    renderComponent({
      drepList: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      isNodeInSync: false,
      refreshState: GovernanceRefreshState.Failed,
      syncProgress: 42,
    });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/DRep data becomes available once the node reaches/)
    ).not.toBeInTheDocument();
  });

  it('renders no directory row on the selfnode path even with a retained list', () => {
    renderComponent({
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('!!!Voting power:')).not.toBeInTheDocument();
  });

  it('renders the selfnode empty state in ja-JP', () => {
    renderComponent({
      drepList: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      locale: 'ja-JP',
      refreshState: GovernanceRefreshState.Failed,
    });

    expect(
      screen.getByText(
        '!!!selfnodeクラスターではDRepディレクトリのデータを利用できません。'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!DRepデータ利用不可')).toBeInTheDocument();
  });
```

The third test is the invariant-6 proof: a selfnode error replaces the whole list
area even when `showAllList` is non-empty.

**Expected:** 61 → **65** tests, 1 snapshot unchanged.

## Step 7 — Pin the no-spawn property in the main-process suite

**File:** `tests/jest/governance/GovernanceQueryService.spec.ts`
(**pre-drifted for prettier** — hand-match style, do not run `--write`.)

Add one test to `describe('selfnode guard')`, immediately after
`it('emits SocketUnavailable when nodeSocketPath is null', …)` (ends `:189`):

```ts
    it('issues no CLI invocation across repeated selfnode refreshes', async () => {
      service.setSelfnodeMode(true);

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SelfnodeCliUnsupported,
      });
      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SelfnodeCliUnsupported,
      });

      expect(mockSpawn).not.toHaveBeenCalled();
    });
```

**Expected:** 40 → **41** tests.

## Step 8 — Pin the remount behaviour in the container suite

**File:** `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx`

**8a.** Let the harness override store fields. In `buildGovernanceStore`
(`:39-62`), add a third parameter and spread it last. **Keep every existing
property of the returned object byte-identical** — the only edits are the new
parameter in the signature and the `...governanceOverrides` spread as the last
entry of the object literal, after `votingPowerState`:

```tsx
const buildGovernanceStore = (
  entry: typeof drepEntry = drepEntry,
  favoriteDRepIds: Set<string> = new Set<string>(),
  governanceOverrides: Record<string, unknown> = {}
) => ({
```

and the object's closing lines become:

```tsx
  votingPowerState: VotingPowerEnrichState.Loaded,
  ...governanceOverrides,
});
```

In `renderPage` (`:64-103`), add `governanceOverrides = {}` to the destructured
defaults, `governanceOverrides?: Record<string, unknown>;` to its inline type, and
pass it through:

```tsx
  const governance = buildGovernanceStore(
    entry,
    favoriteDRepIds,
    governanceOverrides
  );
```

The name matches the sibling precedent in
`source/renderer/app/containers/governance/DRepDetailPage.spec.tsx:96`, `:103`,
`:112`.

**8b.** Add one test to `describe('DRepDirectoryPage')`:

```tsx
  it('keeps the selfnode empty state across remounts', () => {
    const selfnodeOverrides = {
      displayedDRepList: [],
      drepIndex: new Map(),
      drepList: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      isCohortActive: false,
      refreshState: GovernanceRefreshState.Failed,
      showAllList: [],
    };

    const first = renderPage({ governanceOverrides: selfnodeOverrides });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('!!!Voting power:')).not.toBeInTheDocument();
    expect(first.governance.refresh).toHaveBeenCalledTimes(1);
    first.unmount();

    const second = renderPage({ governanceOverrides: selfnodeOverrides });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(second.governance.refresh).toHaveBeenCalledTimes(1);
  });
```

One `refresh()` per mount is the documented, correct behaviour — the no-CLI
guarantee is proved main-side in Step 7, not by suppressing the call here.

**Expected:** 9 → **10** tests.

## Step 9 — Design-doc reconciliations (exactly two edits)

**File:** `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md`

**9a.** In §1 (the status-badge section), **do not delete or edit the
"Selfnode / CLI unsupported" table row at `:16`** — its labels and tokens are the
contract for this badge's copy and styling. Append **one** clarifying sentence to
the prose block below the table (near the contrast rule at `:18` and the
status-grounding note at `:20`), stating that:

> the "Selfnode / CLI unsupported" row is a directory-level state indicator
> rendered as plain markup inside the `selfnode` empty state — it is not a
> `DRepStatus` member and not a `DRepStatusBadge` variant — and its label ships
> under `governance.drepDirectory.status.selfnodeUnavailable`.

One sentence. Do not restructure the section.

**9b.** In §9 (the microcopy table, `:161-222`), add **one** row for
`governance.drepDirectory.status.selfnodeUnavailable` with the en-US label
`DRep data unavailable on selfnode`, placed with the other
`governance.drepDirectory.status.*` rows so the table stays grouped. Match the
surrounding row formatting exactly.

**No other design-doc edit is in scope.** In particular: the §6 refresh table is
already the budget table task-123's criterion asks for — confirm it is there, add
nothing; and `drep-discovery-design.md` needs no edit at all (its component map
and state table already describe everything slice-8 builds).

## Step 10 — Create the slice findings note

**New file:** `.agent/plans/governance/drep-discovery/research/slice-8-findings.md`

This note is **mandatory** for slice-8. Create it with at least the following
entries, written as measured facts with no unrun command reported green:

1. **`yarn stylelint` is red at HEAD `0cdcab581` with 118 errors**, every one
   `order/properties-alphabetical-order`, every one in this feature's own
   governance SCSS. Failing files: `governance/drep-detail/DRepDetail.scss`;
   `governance/drep-directory/{DRepCard,DRepDirectoryBanner,DRepDirectoryFilters,DRepDirectoryList,DRepDirectory,DRepDirectorySearch}.scss`;
   `governance/_shared/{DRepCategoryBadge,DRepEmptyState,DRepErrorBanner,DRepIdDisplay,DRepStatusBadge}.scss`;
   `voting/voting-governance/CurrentVoteSummary.scss`. **Out of scope for
   slice-8; a user-owned pre-merge cleanup item.** Record that slice-8 moved the
   count to **111** solely by deleting two dead selector blocks in
   `DRepDirectory.scss` (`.errorBanner`, `.refreshingBadge`) along with the code
   that used them — no `--fix`, no reordering — and that every declaration
   slice-8 added is alphabetical.
2. **Corrected gate premises.** `yarn storybook:build` and `yarn compile` are
   both **green** at HEAD (exit 0, ~84 s and ~26 s), contradicting the text
   carried in a preceding slice's tracker entry. **Do not edit that closed
   entry** — a closed slice's tracker text is a record of what that slice
   measured, and correcting it retroactively destroys the audit trail. The
   correction lives here.
3. **Unclosed design/code divergence, recorded not fixed:** the design says the
   refresh button is disabled during first load, while `DRepDirectoryBanner.tsx:99`
   disables it only while `isRefreshing`; during `Loading` the button is live. No
   acceptance criterion asks for it and slice-8 does not change it.
4. **Locale-catalog sort correction:** `governance.drepDirectory.error.refresh`
   sorts **after** `governance.drepDirectory.error.rankingUnavailable`, not
   between `…error` and `…error.rankingUnavailable` as the slice PRD's i18n
   inventory stated. Verified against the catalogs' actual
   `Array.prototype.sort()` ordering (1652 keys, 0 mismatches).
5. Anything else slice-8 discovers.

---

## Verification for task-124

### Per-shard checks

| Steps | Command |
|---|---|
| 1–3 | `yarn compile` &nbsp;·&nbsp; `yarn lint` &nbsp;·&nbsp; `node_modules/.bin/stylelint source/renderer/app/components/governance/_shared/DRepEmptyState.scss` (expect **3**, unchanged) |
| 4–5 | `yarn i18n:manage` &nbsp;·&nbsp; `node_modules/.bin/jest --no-coverage --runInBand tests/jest/i18n/preliminaryCopyMarkers.spec.ts` &nbsp;·&nbsp; `yarn storybook:build` |
| 6–10 | the full matrix below |

### Full matrix (before the task is called done)

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-slice-8

# 1. Typecheck.
yarn compile
#    expect exit 0.

# 2. Directory component + its single snapshot.
node_modules/.bin/jest --no-coverage --runInBand source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx
#    baseline (post task-123) 61 tests / 1 snapshot -> expect 65 tests / 1 snapshot UNCHANGED.

# 3. Main-process no-spawn property.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/governance/GovernanceQueryService.spec.ts
#    baseline (post task-123) 40 -> expect 41.

# 4. Container remount behaviour.
node_modules/.bin/jest --no-coverage --runInBand source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx
#    baseline 9 -> expect 10.

# 5. Untouched neighbours that must stay green.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/governance/GovernanceStore.spec.ts tests/jest/governance/logDRepStateSnapshot.spec.ts source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx
#    expect 56, 6 and 9, unchanged.

# 6. Sanitization floor — mandatory in every slice-8 task.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/security/governance-sanitization.spec.ts
#    expect 39, unchanged, green.

# 7. i18n markers and both-catalog parity.
node_modules/.bin/jest --no-coverage --runInBand tests/jest/i18n/preliminaryCopyMarkers.spec.ts
#    expect 5, unchanged, green. governance.drepDirectory.* goes 67 -> 69 per catalog.

# 8. i18n pipeline. WRITES translations/messages.json — expected output.
yarn i18n:manage

# 9. Lint. Errors are the gate; warnings are not.
yarn lint
#    expect exit 0, 0 errors.

# 10. Stylelint. NOT a gate; measure and record only.
yarn stylelint
#    expect 111, UNCHANGED from task-123's post-state. This task adds 0 and fixes 0.

# 11. Storybook.
yarn storybook:build
#    expect exit 0.

# 12. Zero-renderer-timer property. Both commands must return NOTHING.
#     The recursive scan also covers files that are still untracked.
grep -rn 'setTimeout\|setInterval' source/renderer/app/components/governance source/renderer/app/containers/governance storybook/stories/governance
git diff HEAD -- source/renderer storybook | grep -nE '^\+.*(setTimeout|setInterval)'
```

Also confirm by inspection, and state it in the task report:

- `DRepStatus` and `DRepStatusBadge.tsx` are byte-identical to HEAD;
- `source/common/types/governance.types.ts` gained no enum member in this task;
- `GovernanceQueryService._assertQueryable()` is byte-identical to HEAD;
- the shared-design-tokens §1 selfnode table row still exists and was not
  deleted.

## Files this task edits

```
source/renderer/app/components/governance/_shared/DRepEmptyState.tsx
source/renderer/app/components/governance/_shared/DRepEmptyState.scss
source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx
source/renderer/app/i18n/locales/en-US.json
source/renderer/app/i18n/locales/ja-JP.json
storybook/stories/governance/DRepDirectory.stories.tsx
source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx
tests/jest/governance/GovernanceQueryService.spec.ts
source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx
.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md
.agent/plans/governance/drep-discovery/research/slice-8-findings.md   (new)
translations/messages.json                      (written by yarn i18n:manage)
source/renderer/app/i18n/locales/defaultMessages.json  (written by yarn i18n:manage, if it changes)
```

Prettier-format the edited files that are prettier-clean at HEAD:

```bash
/home/node/.claude/jobs/3bad97d1/wt-slice-8/node_modules/.bin/prettier --write \
  source/renderer/app/components/governance/_shared/DRepEmptyState.tsx \
  source/renderer/app/components/governance/_shared/DRepEmptyState.scss \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx
```

**Excluded on purpose:** the three pre-drifted files this task edits
(`.../drep-directory/DRepDirectory.tsx`,
`storybook/stories/governance/DRepDirectory.stories.tsx`,
`tests/jest/governance/GovernanceQueryService.spec.ts`) — hand-match style there;
the locale catalogs and `translations/messages.json` (tool-managed); and **every
markdown file**, including `slice-8-findings.md` and `shared-design-tokens.md` —
`.md` is outside prettier's configured scope, so running it on a doc does
nothing.

---

# task-125 — Release verification: browse → evaluate → select → delegate

**Interaction mode:** `manual_execution`. **No build steps. Do not implement
anything for this row from this guide.**

## Why it cannot be built autonomously

task-125's acceptance criterion is *"Release verification confirms users can
complete browse → evaluate → select → delegate without external portals on a
synced node."* Discharging it requires all of the following, none of which exists
in this container:

- a **synced** Cardano node (`isNodeInSync === true`) on mainnet or preprod;
- a **packaged** Daedalus build — the dev shell runs a different LedgerDB backend
  than packaged builds, so dev-shell behaviour is not evidence;
- both wallet types, including a **real** Ledger or Trezor device with on-device
  confirmation observed **on the device**, not asserted from the app;
- a human verifier. No agent may run, simulate or infer the result.

It is a locked, user-owned stop condition and a listed release gate. **No agent
may relabel it autonomous, and no agent may promote its tracker row past
`pending`.**

## What is delivered instead

The only autonomous deliverable is a **release-verification checklist document**,
authored at slice close at:

```
.agent/plans/governance/drep-discovery/release-verification-checklist.md
```

Its required contract — preconditions, the seven journey legs (browse, evaluate,
search/show-all, favorites, select, delegate, confirm), the standing
"no external portal" and sanitization assertions, the excluded riders, and the
outcome table the verifier fills in — is specified in the slice PRD's
**Release Verification Checklist Contract (task-125)** section. The checklist
lands under its own `docs(gov): task-125 …` commit; the row still stays
`pending`, because the commit delivers the instrument, not the verification.

---

# Appendix — Handled by the orchestrator pipeline — NOT implementation steps

The following are owned by **later pipeline stages** (a scribe, then a
committer). **Do not perform them, and do not treat their absence from your diff
as incomplete work.** They are listed only so you know why they are missing.

1. **Tracker JSON updates.** Editing
   `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
   — `status`, `statusReason`, `evidence`, `updatedAt` — is the scribe's job.
   Field order in a completed row is
   `id, title, description, status, statusReason, evidence, updatedAt, priority,
   estimatedHours, dependencies, targetPath, acceptanceCriteria`; `updatedAt` is
   `"YYYY-MM-DD"`; `evidence` is a flat array of repo-relative path strings; the
   JSON is **never** prettier-formatted. task-125's row stays `pending`, and no
   slice-8 row is promoted to `verified`.
2. **The formatter pass.** `nix fmt` cannot run here. The substitute — explicit
   paths only, files you created — is described under *Shared conventions*, and
   a final formatting sweep is the pipeline's, not a numbered step.
3. **The git commit.** Exactly one commit per task, subject line only, no body,
   no blank line, no trailers and specifically **no `Co-Authored-By`**, formatted
   `<type>(gov): task-NNN <short imperative summary>`. Created by the committer
   stage after review.
4. **The PRD's Final Outcome section** and the slice's `auditSummary` decision
   are closed out at slice close by the orchestrator, not by an implementer.
