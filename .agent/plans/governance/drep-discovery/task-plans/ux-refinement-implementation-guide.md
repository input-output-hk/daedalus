# UX-Refinement Implementation Guide: Sync Awareness + Two-Phase First Load

> **Companion PRD:** [ux-refinement-PRD.md](./ux-refinement-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/workspaces/daedalus/.agent/worktrees/ux-refinement` (branch `wt/ux-refinement`,
> base `a463c31d0`) on 2026-07-23. Anchors are **pre-edit** for the step that
> introduces them; where a later step touches a file an earlier step already edited,
> the step says so explicitly ("post-Step-N"). Re-verify an anchor only if an earlier
> step of this same guide already touched the file.

---

## Implementation Order

Eleven tasks, serialized to honor every tracker dependency edge
(159 → 160 needs 159; 162/163 need 161; 164 needs 160+162; 167 needs 160+162+163;
168 needs 162; 169 applies to both phase calls, so it runs after 161):

**task-159 → task-160 → task-161 → task-162 → task-163 → task-164 → task-165 →
task-166 → task-167 → task-168 → task-169**

Step mapping — this file (PART 1) carries Steps 1-6 in full; PART 2 carries Steps 7-13:

| Step | Task | Content |
|---|---|---|
| 1 | task-159 | Container passes node-sync state into the directory |
| 2 | task-160 | Syncing banner + `DRepEmptyState noSync` fallback + clear-on-sync refetch |
| 3 | task-161 | Split query service into registration + stake phases (+ new IPC channel) |
| 4 | task-162 | Store drives the two phases; `DRepErrorBanner rankingUnavailable`; `—` tooltips |
| 5 | task-163 | Per-phase CLI timeout budgets threaded per call |
| 6 | task-164 | i18n: 5 new keys in en-US + ja-JP; `yarn i18n:manage` |
| 7-13 | task-165 … task-169 + verification + close-out | PART 2 (below) |

Each step ends with its own verify-and-commit sub-step: exactly **one subject-only
Conventional Commits commit per task** (NFR-8), committed locally only — never push,
never `gh` (no credentials in this environment).

---

## Cross-Cutting Notes (apply to every step)

- **Locked invariants, inline:**
  - **Local-first** — no external network calls of any kind. Discovery data comes only
    from the local node via `GovernanceQueryService` (`cardano-cli` against the local
    socket). Nothing in this phase may add an HTTP/fetch/WebSocket call anywhere.
  - **#2 sanitization floor** — no DRep id, no `abstain`/`no_confidence` literal, no
    CIP-129/CIP-105 bech32 string, and no raw hex DRep credential in any `logger.*`,
    analytics, or electron-store payload. In this part that means: every new
    `GovernanceQueryError` message identifies entries by **index only** (never by key,
    hex hash, or bech32 id), and both `GovernanceStore` phase catches log
    `{ errorType }` only (the renderer logger applies no `filterLogData` — slice-3
    proven). `tests/jest/security/governance-sanitization.spec.ts` must stay at
    **20/20, never below**; re-run it after Step 4. (The task-168 snapshot file is the
    ONE documented exception and lives in PART 2 — nothing in Steps 1-6 touches it.)
  - **#5 lovelace losslessness** — json-bigint lossless parse (`storeAsString: true`)
    → decimal-string IPC → renderer `BigNumber`. Never route raw JSONbig objects
    across IPC or into observables. `votingPower` is `BigNumber | null` in the
    renderer, `Lovelace | null` (decimal string) on the wire — **never `Number`**, and
    never a silent fallback to 0.
  - **#6 CLI discipline** — bulk `--all-dreps` once per refresh **per phase**
    (per-DRep CLI calls are forbidden); network flag derived from node config only
    (`setNetwork`), never from renderer/IPC input, appended AFTER the subcommand args;
    socket via `CARDANO_NODE_SOCKET_PATH` in `spawn.env`, never argv; era token
    `latest` with `conway` fallback on **every** query, including the new
    `drep-stake-distribution` call.
  - **#11 preliminary copy (`!!!`)** — every new en-US **and** ja-JP string (locale
    JSONs and component `defaultMessage`s alike) keeps the leading `!!!` marker. Never
    remove an existing marker. New keys are placed alphabetically within the
    governance block. Run `yarn i18n:manage` after any copy change (Step 6).
  - **#13 form-only sentinels** — `drep-alwaysAbstain` / `drep-alwaysNoConfidence`
    stake keys are **skipped** in the Phase-2 parse; sentinels never become directory
    entries.
  - **#14 status grounding** — syncing/noSync are refresh/availability states only;
    stored DRep status stays `active | inactive`. No new status vocabulary.
- **Verification commands** (run from the worktree root after every step):
  1. `node_modules/.bin/tsc --noEmit` — must be zero errors. Use `tsc` directly:
     `yarn compile` is unreliable under Node v24 (slice-3 precedent).
  2. `yarn lint` — clean on every touched file.
  3. Focused Jest per step, e.g.
     `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
     or `yarn test:jest tests/jest/governance/` — the step lists its exact paths.
  4. After Step 4 additionally:
     `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` (20/20).
  5. Step 6 only: `yarn i18n:manage`.
  Report results honestly — never claim an unrun or failing check as passing.
- **Formatting** — `nix` is NOT available here. Instead of `nix fmt`, run
  `yarn prettier --write <file...>` scoped to the `.ts`/`.tsx` files changed for the
  **current task only**. Never reformat JSON (locale files, tracker, mocks), never run
  repo-wide prettier — ~240 files carry pre-existing drift that would flood the diff.
  (`yarn i18n:manage` may rewrite its own managed files; that is the one exception.)
- **Assertion style (Jest 27.5.1 + prettier 2.1.2)**: never
  `toHaveBeenCalledWith('str', { literal: 'object' })` — always
  `expect.objectContaining({ … })` for object arguments (prettier 2.1.2 oscillates
  forever otherwise). Array-literal arguments are fine (existing spec precedent).
  There is NO `jest.advanceTimersByTimeAsync` — use the exact fake-timer +
  `await Promise.resolve()` flush patterns given in Step 5. `clearMocks: true` is set;
  mock state auto-clears between tests.
- **Comments**: 1-3 plain why-lines only, and only where logic is not self-evident.
  No task IDs, no slice IDs, no review labels, no ALL-CAPS tags, no change history.
- **Imports**: never inline `import { type X }` — use a separate `import type`
  statement.
- **SCSS**: no `.scss.d.ts` files are needed — the repo types all SCSS modules via the
  global `declare module '*.scss'` in `source/renderer/declaration.d.ts:1` (verified);
  the existing drep-directory components ship no per-file `.d.ts`. Do not "fix" this.
- **Test scope (PD-11)**: Steps 1-5 only update the **existing** suites they touch to
  keep them green (call-site/prop churn, renamed methods, changed fixtures). The new
  behavior matrix (banner render/clear, two-phase store transitions,
  ranking-unavailable) lands consolidated in task-167 (PART 2, Step 9) by tracker
  design. Steps in this part must not front-run it.

**Files touched in PART 1 (complete list — nothing else):**

| # | File | Steps | Action |
|---|---|---|---|
| 1 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` | 1, 2, 4 | EDIT |
| 2 | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` | 1, 2, 4 | EDIT |
| 3 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | 1, 4 | EDIT |
| 4 | `storybook/stories/governance/DRepDirectory.stories.tsx` | 1, 2, 4 | EDIT |
| 5 | `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx` | 2 | CREATE |
| 6 | `source/renderer/app/components/governance/_shared/DRepEmptyState.scss` | 2 | CREATE |
| 7 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.scss` | 2 | EDIT |
| 8 | `source/common/types/governance.types.ts` | 3 | EDIT |
| 9 | `source/common/ipc/api.ts` | 3 | EDIT |
| 10 | `source/main/governance/GovernanceQueryService.ts` | 3, 5 | EDIT |
| 11 | `source/main/ipc/governanceChannel.ts` | 3 | EDIT (full replacement) |
| 12 | `source/renderer/app/ipc/governanceChannel.ts` | 3 | EDIT (full replacement) |
| 13 | `tests/jest/governance/GovernanceQueryService.spec.ts` | 3, 5 | EDIT |
| 14 | `tests/mocks/governance/drep-stake-distribution.json` | 3 | REWRITE |
| 15 | `source/renderer/app/stores/GovernanceStore.ts` | 4 | EDIT |
| 16 | `source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx` | 4 | CREATE |
| 17 | `source/renderer/app/components/governance/_shared/DRepErrorBanner.scss` | 4 | CREATE |
| 18 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx` | 4 | EDIT |
| 19 | `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` | 4 | EDIT |
| 20 | `tests/jest/governance/GovernanceStore.spec.ts` | 4 | EDIT |
| 21 | `source/renderer/app/i18n/locales/en-US.json` | 6 | EDIT |
| 22 | `source/renderer/app/i18n/locales/ja-JP.json` | 6 | EDIT |

Do NOT touch `NetworkStatusStore.ts`, `Routes.tsx`, `routes-config.ts`, any voting
component, the tracker JSON (until each task's close-out), any slice-1/2/3 planning
doc, or `shared-design-tokens.md` (Step 5 only *reads* §6; §12 is task-168, PART 2).

---

## Step 1: task-159 — Container reads node-sync state and passes it to the directory

`DRepDirectoryPage` reads `stores.networkStatus.isNodeInSync` / `syncProgress` and
passes them into `DRepDirectory` as new **required** props. `GovernanceStore` stays
decoupled from `NetworkStatusStore` — the container is the integration point
(research R1, do not re-litigate). Four files change; the two new props are added to
the `Props` interface but deliberately **not destructured** in the component body yet
(they are unused until Step 2, and an unused destructured binding would trip lint).

### 1a. Add the props to `DRepDirectory.tsx` (`:48-56` pre-edit)

The `Props` interface currently reads exactly:

```ts
interface Props {
  drepList: AppDRepDirectoryEntry[];
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  onRefresh: () => void;
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}
```

Replace it with:

```ts
interface Props {
  drepList: AppDRepDirectoryEntry[];
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  isNodeInSync: boolean;
  syncProgress: number | null;
  onRefresh: () => void;
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}
```

Do NOT change the `function DRepDirectory({ … }: Props)` destructuring in this step.
(`NetworkStatusStore.syncProgress` is initialized `= null` and holds a 0-100 number
once the node reports; `number | null` is the correct prop type.)

### 1b. Pass the props from `DRepDirectoryPage.tsx` (`:46-62` pre-edit)

The `render()` method currently reads exactly:

```tsx
  render() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) return null;

    return (
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        onRefresh={() => governanceStore.refresh()}
        onSelectForDelegation={this.handleSelectForDelegation}
      />
    );
  }
```

Replace it with:

```tsx
  render() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;
    const networkStatus = stores?.networkStatus;

    if (!governanceStore || !networkStatus) return null;

    return (
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        isNodeInSync={networkStatus.isNodeInSync}
        syncProgress={networkStatus.syncProgress}
        onRefresh={() => governanceStore.refresh()}
        onSelectForDelegation={this.handleSelectForDelegation}
      />
    );
  }
```

The container is already `@observer`; reading the two observables in `render()` makes
the directory re-render on every sync-state change with no further wiring.
(`isNodeInSync = false` is declared at `NetworkStatusStore.ts:96`, `syncProgress =
null` at `:119`, set at `:608/:611`; `networkStatus` is in `StoresMap` at
`stores/index.ts:69` — all verified live.)

### 1c. Update the spec helper `DRepDirectory.spec.tsx` (`:39-65` pre-edit)

The `renderComponent` helper currently reads exactly (first lines):

```tsx
const renderComponent = ({
  drepList = baseEntries,
  error = null,
  refreshState = GovernanceRefreshState.Loaded,
  locale = 'en-US',
  onSelectForDelegation = jest.fn(),
}: {
  drepList?: AppDRepDirectoryEntry[];
  error?: { message: string; type: string; details?: string } | null;
  refreshState?: GovernanceRefreshState;
  locale?: string;
  onSelectForDelegation?: jest.Mock;
} = {}) => {
```

Replace the whole helper (through the closing `};` at `:65`) with:

```tsx
const renderComponent = ({
  drepList = baseEntries,
  error = null,
  isNodeInSync = true,
  refreshState = GovernanceRefreshState.Loaded,
  locale = 'en-US',
  onSelectForDelegation = jest.fn(),
  syncProgress = 100,
}: {
  drepList?: AppDRepDirectoryEntry[];
  error?: { message: string; type: string; details?: string } | null;
  isNodeInSync?: boolean;
  refreshState?: GovernanceRefreshState;
  locale?: string;
  onSelectForDelegation?: jest.Mock;
  syncProgress?: number | null;
} = {}) => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepDirectory
        drepList={drepList}
        error={error}
        isNodeInSync={isNodeInSync}
        lastFetchedAt={Date.now() - 60_000}
        onRefresh={jest.fn()}
        onSelectForDelegation={onSelectForDelegation}
        refreshState={refreshState}
        syncProgress={syncProgress}
      />
    </IntlProvider>
  );
};
```

Defaults `isNodeInSync: true` / `syncProgress: 100` keep every existing test's
behavior unchanged. Do not add new tests in this step (PD-11 — task-167 owns them).

### 1d. Update the story helpers `DRepDirectory.stories.tsx` (`:123-146` pre-edit)

First, immediately after the line `type DirectoryError = { message: string; type:
string } | null;` (`:25` pre-edit), insert:

```ts
type DirectorySyncState = {
  isNodeInSync: boolean;
  syncProgress: number | null;
};

const DEFAULT_SYNC_STATE: DirectorySyncState = {
  isNodeInSync: true,
  syncProgress: 100,
};
```

Then replace the two render helpers, which currently read exactly:

```tsx
const renderDirectory = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null
) => (
  <DRepDirectory
    drepList={entries}
    error={error}
    lastFetchedAt={Date.now() - 3 * 60 * 1000}
    onRefresh={action('onRefresh')}
    onSelectForDelegation={action('onSelectForDelegation')}
    refreshState={refreshState}
  />
);

const renderCentered = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null
) => (
  <div style={CENTERED_STYLE}>
    {renderDirectory(refreshState, entries, error)}
  </div>
);
```

with:

```tsx
const renderDirectory = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE
) => (
  <DRepDirectory
    drepList={entries}
    error={error}
    isNodeInSync={syncState.isNodeInSync}
    lastFetchedAt={Date.now() - 3 * 60 * 1000}
    onRefresh={action('onRefresh')}
    onSelectForDelegation={action('onSelectForDelegation')}
    refreshState={refreshState}
    syncProgress={syncState.syncProgress}
  />
);

const renderCentered = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE
) => (
  <div style={CENTERED_STYLE}>
    {renderDirectory(refreshState, entries, error, syncState)}
  </div>
);
```

The locale comment above `renderDirectory` (`:120-122`) stays — the global
English/Japanese toggle keeps driving all labels; do not add a local `IntlProvider`.

### 1e. Store-boundary check, verify, commit

1. Confirm the boundary is intact (acceptance criterion 2):
   `grep -n "NetworkStatus" source/renderer/app/stores/GovernanceStore.ts` → must
   print nothing.
2. `node_modules/.bin/tsc --noEmit` → zero errors.
3. `yarn lint` → clean.
4. `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
   → all 12 existing tests pass.
5. `yarn prettier --write source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx source/renderer/app/containers/governance/DRepDirectoryPage.tsx source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx storybook/stories/governance/DRepDirectory.stories.tsx`
6. Commit (subject only):
   `feat(gov): task-159 pass node-sync state into the drep directory`

---

## Step 2: task-160 — Syncing banner + `DRepEmptyState noSync` fallback + clear-on-sync refetch

While `!isNodeInSync`, `DRepDirectory` renders a persistent, non-dismissible
inline-warning banner (icon + text + live sync %, `--badge-warning-*` token slots,
never color alone) above the list. If the syncing query yields zero DReps or an
era/availability error, the list area falls back to a NEW `DRepEmptyState` component
(`noSync` variant only — PD-1). A MobX `reaction` in the container refetches exactly
once when `isNodeInSync` flips false → true; the banner clears by plain re-render.

Until Step 6 lands the locale keys, the new strings render from their `!!!`-prefixed
`defaultMessage` fallbacks (react-intl logs a missing-key console warning in the new
story — expected and transitional; no spec renders these states by default).

### 2a. CREATE `source/renderer/app/components/governance/_shared/DRepEmptyState.scss`

Full file content:

```scss
.container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 16px;
  padding: 48px 0;
  text-align: center;
}

.message {
  font-size: 14px;
  line-height: 1.4;
  color: var(--theme-text-secondary, #6b7384);
  max-width: 480px;
}
```

(`line-height` + `max-width` let the JA/DE copy wrap to ≥2 lines without truncation —
NFR-7. No `.scss.d.ts` — global declaration covers it, PD-12.)

### 2b. CREATE `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx`

Full file content:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './DRepEmptyState.scss';

const messages = defineMessages({
  noSync: {
    id: 'governance.drepDirectory.empty.noSync',
    defaultMessage:
      '!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.',
    description: 'Directory fallback while the node has not reached the tip',
  },
});

// Only the noSync variant ships for now; the designed noResults and selfnode
// variants join this union when their owning slices land.
export type DRepEmptyStateVariant = 'noSync';

interface Props {
  variant: DRepEmptyStateVariant;
  intl: intlShape.isRequired;
}

function DRepEmptyState({ variant, intl }: Props) {
  const messageByVariant = {
    noSync: messages.noSync,
  };

  return (
    <div className={styles.container} data-variant={variant}>
      <p className={styles.message}>
        {intl.formatMessage(messageByVariant[variant])}
      </p>
    </div>
  );
}

export default injectIntl(DRepEmptyState);
```

The en source string is the canonical §9 row (`shared-design-tokens.md`,
`governance.drepDirectory.empty.noSync`), `!!!`-prefixed per invariant #11.

### 2c. Wire banner + fallback into `DRepDirectory.tsx`

This file was touched in Step 1 (Props only); the anchors below are otherwise
unchanged from the pre-Step-1 line numbers.

**Imports** — after the existing line (`:6` pre-edit)

```ts
import DRepDirectoryBanner from './DRepDirectoryBanner';
```

insert:

```ts
import DRepEmptyState from '../_shared/DRepEmptyState';
```

and after the existing stores import block (`:8-12` pre-edit) ending in

```ts
} from '../../../stores/GovernanceStore';
```

insert:

```ts
import { GovernanceQueryErrorType } from '../../../../../common/types/governance.types';
```

(Same relative path the `_shared/DRepStatusBadge.tsx` uses for this module.)

**Messages** — the last entry of the `messages` block currently reads exactly
(`:41-45` pre-edit):

```ts
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Refreshing state badge label',
  },
```

After it (inside `defineMessages({ … })`), add:

```ts
  syncing: {
    id: 'governance.drepDirectory.syncing',
    defaultMessage:
      '!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes.',
    description: 'Persistent soft-warning banner while the node is syncing',
  },
```

(Canonical §9 row uses the `{progress}` placeholder — §6's `{n}` is the informal
variant; PD-10.)

**Destructuring** — the component signature currently reads exactly (`:58-66`
pre-edit):

```tsx
function DRepDirectory({
  drepList,
  refreshState,
  error,
  lastFetchedAt,
  onRefresh,
  onSelectForDelegation,
  intl,
}: Props) {
```

Replace with:

```tsx
function DRepDirectory({
  drepList,
  refreshState,
  error,
  lastFetchedAt,
  isNodeInSync,
  syncProgress,
  onRefresh,
  onSelectForDelegation,
  intl,
}: Props) {
```

**Fallback predicate** — the two consts at the top of the body currently read exactly
(`:67-68` pre-edit):

```tsx
  const hasRetainedData = drepList.length > 0;
  const showErrorBanner = error && hasRetainedData;
```

After them, add:

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

(Covers both fallback triggers: Loaded-with-zero-DReps and Failed on an
era/availability error. Selfnode is excluded — its dedicated variant belongs to a
later slice, PD-1. A Loading state keeps its spinner; retained data keeps the list.)

**renderContent case** — the `switch (true)` currently transitions from the Loading
case directly to (`:80` pre-edit):

```tsx
      case refreshState === GovernanceRefreshState.Failed:
```

Immediately BEFORE that `case` line (i.e. after the closing `);` of the Loading
case), insert:

```tsx
      case showNoSyncFallback:
        return <DRepEmptyState variant="noSync" />;
```

**Banner mount** — the outer return currently reads exactly (`:147-156` pre-edit):

```tsx
  return (
    <div className={styles.container}>
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
      />
      {renderContent()}
    </div>
  );
```

Replace with:

```tsx
  return (
    <div className={styles.container}>
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
      />
      {!isNodeInSync && (
        <div className={styles.syncingBanner} role="status">
          <svg
            className={styles.syncingIcon}
            aria-hidden="true"
            width="16"
            height="16"
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
          <span>
            {intl.formatMessage(messages.syncing, {
              progress: Math.floor(syncProgress ?? 0),
            })}
          </span>
        </div>
      )}
      {renderContent()}
    </div>
  );
```

Notes: the banner is persistent (no dismiss control — by design), `role="status"`
announces it to screen readers, the inline warning-triangle SVG pairs an icon with
the text so color is never the sole cue (§1 rule), and `Math.floor(syncProgress ?? 0)`
pins the fractional/null-mid-boot cases (PRD risk row).

### 2d. Banner styles — append to `DRepDirectory.scss` (`:42-49` pre-edit is the last rule)

Append after the existing `.refreshingBadge` rule:

```scss
.syncingBanner {
  display: flex;
  align-items: flex-start;
  gap: 8px;
  padding: 12px 16px;
  border-radius: 8px;
  font-size: 14px;
  line-height: 1.4;
  color: var(--badge-warning-fg, #b76e00);
  background: var(--badge-warning-bg, rgba(230, 162, 60, 0.12));
}

.syncingIcon {
  flex-shrink: 0;
  margin-top: 2px;
}
```

(`--badge-warning-*` are the §1 semantic slots; the fallback literals follow the
`DRepStatusBadge.scss` fallback pattern. `align-items: flex-start` keeps the icon
pinned when the JA/DE copy wraps to 2+ lines — NFR-7.)

### 2e. Clear-on-sync refetch reaction in `DRepDirectoryPage.tsx`

This file was touched in Step 1 (render only). **Imports** — after the existing
line (`:2` pre-edit):

```ts
import { observer, inject } from 'mobx-react';
```

insert:

```ts
import { reaction } from 'mobx';
import type { IReactionDisposer } from 'mobx';
```

**Lifecycle** — `componentDidMount` currently reads exactly (`:20-34` pre-edit):

```tsx
  componentDidMount() {
    const governanceStore: GovernanceStore | undefined =
      this.props.stores?.governance;

    if (!governanceStore) {
      return;
    }

    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
    }
  }
```

Replace with:

```tsx
  syncReactionDisposer: IReactionDisposer | null = null;

  componentDidMount() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) {
      return;
    }

    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
    }

    // Replace the possibly-incomplete syncing snapshot exactly once when the
    // node reaches the tip; reaction fires only on the false -> true edge.
    this.syncReactionDisposer = reaction(
      () => stores?.networkStatus.isNodeInSync,
      (isNodeInSync) => {
        if (isNodeInSync) {
          governanceStore.refresh();
        }
      }
    );
  }

  componentWillUnmount() {
    if (this.syncReactionDisposer) {
      this.syncReactionDisposer();
      this.syncReactionDisposer = null;
    }
  }
```

(mobx is 5.15.7 — the effect signature is `(value, reaction)`; a reaction only fires
on change, so mounting while already in sync triggers nothing, and repeated `true`
values cannot re-fire. `GovernanceStore.refresh()` carries its own in-flight dedup.)

### 2f. Storybook: `Node syncing` story

In `DRepDirectory.stories.tsx` (post-Step-1), change the knobs import (`:5`
pre-edit):

```ts
import { withKnobs, select } from '@storybook/addon-knobs';
```

to:

```ts
import { withKnobs, select, number } from '@storybook/addon-knobs';
```

Then, after the existing `'Refreshing'` story block, which reads exactly (`:305-311`
pre-edit):

```ts
  .add('Refreshing', () =>
    renderCentered(
      GovernanceRefreshState.Refreshing,
      baseEntries,
      REFRESH_ERROR
    )
  )
```

insert:

```ts
  .add('Node syncing', () =>
    renderCentered(GovernanceRefreshState.Loaded, baseEntries, null, {
      isNodeInSync: false,
      syncProgress: number('Sync progress (%)', 87, {
        max: 100,
        min: 0,
        range: true,
        step: 1,
      }),
    })
  )
  .add('Node syncing — empty fallback', () =>
    renderCentered(GovernanceRefreshState.Loaded, [], null, {
      isNodeInSync: false,
      syncProgress: number('Sync progress (%)', 87, {
        max: 100,
        min: 0,
        range: true,
        step: 1,
      }),
    })
  )
```

(The second story exercises the `DRepEmptyState noSync` fallback. Locale stays on the
global toggle — no local `IntlProvider`.)

### 2g. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
   → all existing tests still pass (defaults render in-sync: banner absent, fallback
   never triggers). New banner/fallback/reaction tests land in task-167 (PD-11).
4. `yarn prettier --write source/renderer/app/components/governance/_shared/DRepEmptyState.tsx source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx source/renderer/app/containers/governance/DRepDirectoryPage.tsx storybook/stories/governance/DRepDirectory.stories.tsx`
   (prettier 2.1.2 does not format `.scss` in this repo's config scope — leave the
   two SCSS files as written).
5. Commit (subject only):
   `feat(gov): task-160 add syncing banner with nosync fallback and refetch on sync`

---

## Step 3: task-161 — Split the query service into registration + stake phases

`GovernanceQueryService.fetchDRepList()` becomes Phase 1 `fetchDRepRegistrations()`
(`drep-state --all-dreps --output-json` — **no** `--include-stake` — plus
`query tip`; every entry `votingPower: null`) and a new Phase 2 `fetchDRepStake()`
(`drep-stake-distribution --all-dreps --output-json` → CIP-129-keyed decimal-string
map). One new request/response IPC channel carries Phase 2 (PD-3); the renderer store
still calls only the list channel until Step 4, so after this step the app
transitionally shows `—` for every voting power — expected, and resolved by Step 4.

Both phases keep every slice-1 guarantee: era `latest`→`conway` fallback, network
flag from node config appended after the subcommand, socket via `spawn.env`,
json-bigint lossless parse. The single 10s `CLI_TIMEOUT_MS` still applies to both
phases in this step — Step 5 (task-163) threads the per-phase budgets.

### 3a. Shared types — `source/common/types/governance.types.ts` (`:87-94` pre-edit)

The query-payload block currently reads exactly:

```ts
export interface DRepListQueryPayload {
  /** All DRep entries from the ledger state. */
  dreps: DRepDirectoryEntry[];
  /** Unix timestamp (ms) when the data was fetched. */
  fetchedAt: number;
  /** Current epoch number returned by `query tip`; nullable for compatibility. */
  epoch: number | null;
}
```

Immediately after it, add:

```ts
export interface DRepStakeQueryPayload {
  /** Voting power in lovelace (decimal string) keyed by CIP-129 DRep id. */
  stakeByDRepId: Record<DRepId, Lovelace>;
  /** Unix timestamp (ms) when the stake distribution was fetched. */
  fetchedAt: number;
}
```

(`DRepId` and `Lovelace` are already exported by this file at `:14` and `:47`.)

### 3b. IPC contract — `source/common/ipc/api.ts`

The type import at `:85` currently reads exactly:

```ts
import type { DRepListQueryPayload } from '../types/governance.types';
```

Replace with:

```ts
import type {
  DRepListQueryPayload,
  DRepStakeQueryPayload,
} from '../types/governance.types';
```

The governance block currently ends at `:657-659` with exactly:

```ts
export const GOVERNANCE_DREP_LIST_CHANNEL = 'GOVERNANCE_DREP_LIST_CHANNEL';
export type GovernanceDRepListRendererRequest = void;
export type GovernanceDRepListMainResponse = DRepListQueryPayload;
```

Append after it:

```ts
export const GOVERNANCE_DREP_STAKE_CHANNEL = 'GOVERNANCE_DREP_STAKE_CHANNEL';
export type GovernanceDRepStakeRendererRequest = void;
export type GovernanceDRepStakeMainResponse = DRepStakeQueryPayload;
```

### 3c. Service split — `source/main/governance/GovernanceQueryService.ts`

**Imports** (`:6-12` pre-edit) — the common-types import currently reads exactly:

```ts
import {
  GovernanceQueryErrorType,
  DRepListQueryPayload,
  DRepDirectoryEntry,
  DRepStatus,
  DrepActivity,
} from '../../common/types/governance.types';
```

Add `DRepStakeQueryPayload` after `DRepListQueryPayload`:

```ts
import {
  GovernanceQueryErrorType,
  DRepListQueryPayload,
  DRepStakeQueryPayload,
  DRepDirectoryEntry,
  DRepStatus,
  DrepActivity,
} from '../../common/types/governance.types';
```

**In-flight fields** (`:63-64` pre-edit) — currently exactly:

```ts
  private lastSuccessfulData: DRepListQueryPayload | null = null;
  private inFlightRefresh: Promise<DRepListQueryPayload> | null = null;
```

Replace with (per-phase dedup):

```ts
  private lastSuccessfulData: DRepListQueryPayload | null = null;
  private inFlightRegistrations: Promise<DRepListQueryPayload> | null = null;
  private inFlightStake: Promise<DRepStakeQueryPayload> | null = null;
```

**reset()** (`:131-137` pre-edit) — currently exactly:

```ts
  reset(): void {
    this.lastSuccessfulData = null;
    this.inFlightRefresh = null;
    this.nodeSocketPath = null;
    this.isSelfnode = false;
    this.networkFlag = null;
  }
```

Replace with:

```ts
  reset(): void {
    this.lastSuccessfulData = null;
    this.inFlightRegistrations = null;
    this.inFlightStake = null;
    this.nodeSocketPath = null;
    this.isSelfnode = false;
    this.networkFlag = null;
  }
```

**Public phase methods** — replace the whole `fetchDRepList` method **including its
doc comment** (`:139-162` pre-edit, starting `/**\n   * Fetch the full DRep list…`
and ending with the `finally` block's closing braces) with:

```ts
  /**
   * Phase 1: fetch DRep registrations (no stake read) from the local node.
   * Voting power is always null here; fetchDRepStake() enriches it.
   * Deduplicates in-flight requests — if a refresh is already running,
   * the same promise is returned to all concurrent callers.
   *
   * @throws {GovernanceQueryError} on socket-unavailable, CLI-not-found,
   *         query-failed, parse-failed, or timeout.
   */
  async fetchDRepRegistrations(): Promise<DRepListQueryPayload> {
    if (this.inFlightRegistrations) {
      return this.inFlightRegistrations;
    }

    this.inFlightRegistrations = this._doFetchDRepRegistrations();

    try {
      const result = await this.inFlightRegistrations;
      this.lastSuccessfulData = result;
      return result;
    } finally {
      this.inFlightRegistrations = null;
    }
  }

  /**
   * Phase 2: fetch the DRep stake distribution keyed by the same CIP-129
   * DRep id the registration payload derives, so the renderer merges by
   * plain string equality.
   *
   * @throws {GovernanceQueryError} on the same failure classes as Phase 1.
   */
  async fetchDRepStake(): Promise<DRepStakeQueryPayload> {
    if (this.inFlightStake) {
      return this.inFlightStake;
    }

    this.inFlightStake = this._doFetchDRepStake();

    try {
      return await this.inFlightStake;
    } finally {
      this.inFlightStake = null;
    }
  }
```

**Private fetch implementations** — replace the whole `_doFetchDRepList` method
(`:171-217` pre-edit, beginning exactly with):

```ts
  private async _doFetchDRepList(): Promise<DRepListQueryPayload> {
    if (this.isSelfnode) {
```

with the following three methods (the guards move into a shared helper so both
phases enforce identical preconditions):

```ts
  private _assertQueryable(): void {
    if (this.isSelfnode) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.SelfnodeCliUnsupported,
        'DRep data is unavailable in selfnode mode. A synced node is required.'
      );
    }

    if (!this.nodeSocketPath) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.SocketUnavailable,
        'Cardano node socket path is not available. The node may not be fully started.'
      );
    }
  }

  private async _doFetchDRepRegistrations(): Promise<DRepListQueryPayload> {
    this._assertQueryable();

    try {
      const [drepStateStdout, tipStdout] = await Promise.all([
        this._runCliQueryWithEraFallback([
          'query',
          'drep-state',
          '--all-dreps',
          '--output-json',
        ]),
        this._runCliQueryWithEraFallback(['query', 'tip', '--output-json']),
      ]);

      const currentEpoch = this._parseTipEpoch(tipStdout);
      const dreps = this._parseDRepState(drepStateStdout, currentEpoch);

      return {
        dreps,
        fetchedAt: Date.now(),
        epoch: currentEpoch,
      };
    } catch (error) {
      if (error instanceof GovernanceQueryError) {
        throw error;
      }
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.QueryFailed,
        `DRep query failed: ${
          error instanceof Error ? error.message : String(error)
        }`
      );
    }
  }

  private async _doFetchDRepStake(): Promise<DRepStakeQueryPayload> {
    this._assertQueryable();

    try {
      const stakeStdout = await this._runCliQueryWithEraFallback([
        'query',
        'drep-stake-distribution',
        '--all-dreps',
        '--output-json',
      ]);

      return {
        stakeByDRepId: this._parseStakeDistribution(stakeStdout),
        fetchedAt: Date.now(),
      };
    } catch (error) {
      if (error instanceof GovernanceQueryError) {
        throw error;
      }
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.QueryFailed,
        `DRep stake query failed: ${
          error instanceof Error ? error.message : String(error)
        }`
      );
    }
  }
```

Note the Phase-1 argv no longer contains `--include-stake` (that was the split's
whole point) and the stake phase is one bulk `--all-dreps` call — never per-DRep
(invariant #6).

**Voting power in `_parseDRepState`** (`:450-454` pre-edit) — currently exactly:

```ts
        // Voting power from optional stake (only with --include-stake)
        const votingPower: string | null =
          state.stake !== undefined && state.stake !== null
            ? String(state.stake)
            : null;
```

Replace with (PD-5 — Phase 1 always null, even if a stray `stake` field appears):

```ts
        // Phase 1 never reads stake; fetchDRepStake() fills voting power.
        const votingPower: string | null = null;
```

Also update the `_parseDRepState` doc comment (`:380-394` pre-edit): change its first
line from

```ts
   * Parse the raw JSON stdout from `cardano-cli latest query drep-state --all-dreps --include-stake --output-json`.
```

to

```ts
   * Parse the raw JSON stdout from `cardano-cli latest query drep-state --all-dreps --output-json`.
```

and replace the two stake-mentioning bullet/sentence lines

```ts
   * - `state` has `expiry` (epoch number), `anchor` (object|null), `deposit` (lovelace),
   *   and optional `stake` (lovelace string) only when `--include-stake` is used.
```
```ts
   * Voting power is nullable when `stake` is absent.
```

with

```ts
   * - `state` has `expiry` (epoch number), `anchor` (object|null), `deposit` (lovelace).
```
```ts
   * Voting power is always null in this phase; the stake phase enriches it.
```

**Stake parser** — insert the following new method immediately AFTER the closing
brace of `_parseDRepState` (`:474` pre-edit) and BEFORE `_credentialToDRepId`
(`:476` pre-edit):

```ts
  /**
   * Parse `drep-stake-distribution --all-dreps --output-json` into a
   * CIP-129-keyed decimal-string lovelace map.
   *
   * cardano-cli serialized this query as an object map in some major versions
   * and as an array of [key, value] pairs in others; both container shapes are
   * accepted. Keys are `drep-keyHash-<hex>` / `drep-scriptHash-<hex>` plus the
   * two voting sentinels, which are skipped (sentinels are ballot forms, never
   * directory entries). Any other key or value shape throws ParseFailed.
   * Error messages identify entries by index only — never by key or id.
   */
  private _parseStakeDistribution(rawOutput: string): Record<string, string> {
    let parsed: unknown;
    try {
      parsed = JSONBig.parse(rawOutput);
    } catch (err) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        'Failed to parse drep-stake-distribution JSON output',
        err instanceof Error ? err.message : undefined
      );
    }

    let pairs: Array<[string, unknown]>;
    if (Array.isArray(parsed)) {
      pairs = (parsed as Array<unknown>).map((entry, index) => {
        if (
          !Array.isArray(entry) ||
          entry.length < 2 ||
          typeof entry[0] !== 'string'
        ) {
          throw new GovernanceQueryError(
            GovernanceQueryErrorType.ParseFailed,
            `Stake entry at index ${index} is not a [key, value] pair`
          );
        }
        return [entry[0], entry[1]] as [string, unknown];
      });
    } else if (parsed && typeof parsed === 'object') {
      pairs = Object.entries(parsed as Record<string, unknown>);
    } else {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        `Expected an object map or array of pairs from drep-stake-distribution, got ${typeof parsed}`
      );
    }

    const stakeByDRepId: Record<string, string> = {};
    pairs.forEach(([key, value], index) => {
      if (key === 'drep-alwaysAbstain' || key === 'drep-alwaysNoConfidence') {
        return;
      }

      const keyHashMatch = /^drep-keyHash-([0-9a-fA-F]+)$/.exec(key);
      const scriptHashMatch = /^drep-scriptHash-([0-9a-fA-F]+)$/.exec(key);
      if (!keyHashMatch && !scriptHashMatch) {
        throw new GovernanceQueryError(
          GovernanceQueryErrorType.ParseFailed,
          `Stake entry at index ${index} has an unknown key shape`
        );
      }

      const stakeString = String(value);
      if (
        (typeof value !== 'string' && typeof value !== 'number') ||
        !/^\d+$/.test(stakeString)
      ) {
        throw new GovernanceQueryError(
          GovernanceQueryErrorType.ParseFailed,
          `Stake entry at index ${index} has a non-numeric stake value`
        );
      }

      const drepId = keyHashMatch
        ? this._credentialToDRepId({ keyHash: keyHashMatch[1] }, index)
        : this._credentialToDRepId({ scriptHash: scriptHashMatch![1] }, index);

      stakeByDRepId[drepId] = stakeString;
    });

    return stakeByDRepId;
  }
```

(json-bigint with `storeAsString: true` turns oversized JSON integers into decimal
strings before they can lose precision — invariant #5; the CIP-129 id comes from the
same `_credentialToDRepId` → `Cardano.DRepID.cip129FromCredential` path
(`:481-511` pre-edit) the list payload uses, which is what makes the renderer's
merge-by-string-equality sound. A Phase-2 `ParseFailed` degrades to
`rankingUnavailable` in Step 4 — it never corrupts the list.)

### 3d. Main IPC handler — `source/main/ipc/governanceChannel.ts` (full replacement)

Replace the entire file (45 lines pre-edit) with:

```ts
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GOVERNANCE_DREP_LIST_CHANNEL,
  GOVERNANCE_DREP_STAKE_CHANNEL,
} from '../../common/ipc/api';
import type {
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse,
  GovernanceDRepStakeRendererRequest,
  GovernanceDRepStakeMainResponse,
} from '../../common/ipc/api';
import { GovernanceQueryService } from '../governance/GovernanceQueryService';
import { logger } from '../utils/logging';

const governanceDRepListChannel: MainIpcChannel<
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);

const governanceDRepStakeChannel: MainIpcChannel<
  GovernanceDRepStakeRendererRequest,
  GovernanceDRepStakeMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_STAKE_CHANNEL);

// Re-throw a marked PLAIN OBJECT (not an Error) so the structured error
// survives Electron structured clone intact. IpcChannel.onRequest forwards
// the raw thrown value via event.sender.send(responseChannel, false, error)
// with no re-wrap, and request() rejects the renderer promise with the
// structured-cloned value. Error instances flatten to { name, message } and
// would lose `details`; a plain object keeps every property.
const toGovernanceIpcError = (error: unknown) => {
  const queryErr = error as {
    queryErrorType?: string;
    message?: string;
    details?: string;
  };
  return {
    __governanceError: true,
    type: queryErr.queryErrorType ?? 'UNKNOWN',
    message:
      queryErr.message ?? 'An unknown error occurred while querying DRep data.',
    details: queryErr.details,
  };
};

export const handleGovernanceRequests = () => {
  governanceDRepListChannel.onRequest(async (_request) => {
    logger.info('Governance IPC: DRep list requested from renderer');
    try {
      return await GovernanceQueryService.getInstance().fetchDRepRegistrations();
    } catch (error) {
      logger.error('Governance IPC: DRep list query failed', { error });
      // eslint-disable-next-line
      throw toGovernanceIpcError(error);
    }
  });

  governanceDRepStakeChannel.onRequest(async (_request) => {
    logger.info(
      'Governance IPC: DRep stake distribution requested from renderer'
    );
    try {
      return await GovernanceQueryService.getInstance().fetchDRepStake();
    } catch (error) {
      logger.error('Governance IPC: DRep stake query failed', { error });
      // eslint-disable-next-line
      throw toGovernanceIpcError(error);
    }
  });
};
```

(The `__governanceError` plain-object contract is shared verbatim with the list
channel — FR-7; `source/main/ipc/index.ts:51` already calls
`handleGovernanceRequests()`, so no registration change is needed.)

### 3e. Renderer channel — `source/renderer/app/ipc/governanceChannel.ts` (full replacement)

Replace the entire file (12 lines pre-edit) with:

```ts
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GOVERNANCE_DREP_LIST_CHANNEL,
  GOVERNANCE_DREP_STAKE_CHANNEL,
} from '../../../common/ipc/api';
import type {
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest,
  GovernanceDRepStakeMainResponse,
  GovernanceDRepStakeRendererRequest,
} from '../../../common/ipc/api';

export const governanceDRepListChannel: RendererIpcChannel<
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);

export const governanceDRepStakeChannel: RendererIpcChannel<
  GovernanceDRepStakeMainResponse,
  GovernanceDRepStakeRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_STAKE_CHANNEL);
```

### 3f. REWRITE `tests/mocks/governance/drep-stake-distribution.json`

The committed mock uses a wrong shape (bech32-keyed `{ stake: … }` objects) and is
referenced by no test (PD-6). Replace its entire content with the canonical
object-map key shape (values are bare JSON numbers, as the CLI emits; the hex
credentials match the spec's existing drep-state fixtures so ids line up):

```json
{
  "drep-keyHash-a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4": 23137980123456,
  "drep-scriptHash-c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6": 9007199254740993,
  "drep-alwaysAbstain": 5000000000,
  "drep-alwaysNoConfidence": 1000000000
}
```

`9007199254740993` exceeds `Number.MAX_SAFE_INTEGER` — the spec reads this file as a
**raw string** (`fs.readFileSync`, never `require`) and pipes it in as CLI stdout, so
JS number precision never touches it before json-bigint. Real-shape confirmation
against a live CLI is explicitly part of the task-166 manual follow-up (PD-6). Do not
run prettier on this file.

### 3g. Update `tests/jest/governance/GovernanceQueryService.spec.ts`

Six mechanical changes, then one new describe block:

1. **Imports** — after the existing import block (`:12-20` pre-edit, ending with
   `import * as childProcess from 'child_process';`), add:

   ```ts
   import fs from 'fs';
   import path from 'path';
   import { Cardano } from '@cardano-sdk/core';
   ```

   Place these BEFORE the `jest.mock('child_process', …)` call at `:22` (jest.mock
   is hoisted anyway; keeping imports together satisfies lint's import/order).

2. **Rename every call** `service.fetchDRepList()` → `service.fetchDRepRegistrations()`
   (28 occurrences; plain find-and-replace across the file). Also update the one
   test title containing the old name (`:547` pre-edit):
   `'returns the same promise for concurrent fetchDRepList calls'` →
   `'returns the same promise for concurrent fetchDRepRegistrations calls'`.

3. **`VALID_DREP_STATE_JSON` fixture** (`:34-57` pre-edit): delete the single line
   `      stake: '23137980123456',` from the first tuple's state object, and change
   the fixture doc comment (`:34`) from
   `/** Realistic drep-state tuple output from cardano-cli --include-stake. */` to
   `/** Realistic drep-state tuple output from cardano-cli (registration phase). */`

4. **Assertions on the parsed fixture** — in
   `'parses valid drep-state tuple output with tip epoch'` (`:195-225` pre-edit):
   - change `expect(drep0.votingPower).toBe('23137980123456');` (`:209`) to
     `expect(drep0.votingPower).toBeNull();`
   - change the comment `// First DRep (keyHash, with stake and anchor)` (`:206`) to
     `// First DRep (keyHash, with anchor); voting power is a Phase-2 concern`

5. **Conway-retry argv arrays** (`:254-267` and `:274-287` pre-edit): delete the
   `'--include-stake',` line from BOTH expected argv arrays (calls 1 and 3).

6. **Delete** the constant `RAW_OVERSIZED_STAKE_DREP_STATE_JSON` (`:71-81` pre-edit)
   and the whole test
   `'preserves oversized unquoted lovelace values through json-bigint parsing'`
   (`:296-306` pre-edit) — the losslessness proof moves to the stake phase below.

7. **Fixture constant** — after `LATEST_ALIAS_MISSING_STDERR` (`:68-69` pre-edit),
   add:

   ```ts
   /** Canonical object-map drep-stake-distribution output (committed mock). */
   const STAKE_DISTRIBUTION_FIXTURE = fs.readFileSync(
     path.join(__dirname, '../../mocks/governance/drep-stake-distribution.json'),
     'utf-8'
   );
   ```

8. **New describe block** — insert immediately BEFORE the
   `// ---- network flag injection (FP-1) ----` comment (`:564` pre-edit):

   ```ts
     // ---- stake distribution phase ----

     describe('stake distribution phase', () => {
       const KEY_HASH =
         'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4';
       const SCRIPT_HASH =
         'c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6';
       // Ids derived through the same production path the parser uses, so the
       // merge-key alignment is proven rather than assumed.
       const keyHashDRepId = Cardano.DRepID.cip129FromCredential({
         type: Cardano.CredentialType.KeyHash,
         hash: KEY_HASH,
       } as any);
       const scriptHashDRepId = Cardano.DRepID.cip129FromCredential({
         type: Cardano.CredentialType.ScriptHash,
         hash: SCRIPT_HASH,
       } as any);

       it('parses the object-map container shape from the committed fixture', async () => {
         mockSpawn.mockReturnValueOnce(
           createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
         );

         const result = await service.fetchDRepStake();

         expect(result.fetchedAt).toBeGreaterThan(0);
         expect(result.stakeByDRepId[keyHashDRepId]).toBe('23137980123456');
         expect(result.stakeByDRepId[scriptHashDRepId]).toBe(
           '9007199254740993'
         );
       });

       it('skips the two voting sentinels without creating entries', async () => {
         mockSpawn.mockReturnValueOnce(
           createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
         );

         const result = await service.fetchDRepStake();

         expect(Object.keys(result.stakeByDRepId)).toHaveLength(2);
       });

       it('parses the array-of-pairs container shape', async () => {
         const arrayShape = `[
           ["drep-keyHash-${KEY_HASH}", 23137980123456],
           ["drep-alwaysAbstain", 5000000000]
         ]`;
         mockSpawn.mockReturnValueOnce(createMockChildProcess(arrayShape));

         const result = await service.fetchDRepStake();

         expect(result.stakeByDRepId).toEqual({
           [keyHashDRepId]: '23137980123456',
         });
       });

       it('preserves oversized unquoted lovelace values through json-bigint parsing', async () => {
         const oversized = `{ "drep-keyHash-${KEY_HASH}": 9007199254740993 }`;
         mockSpawn.mockReturnValueOnce(createMockChildProcess(oversized));

         const result = await service.fetchDRepStake();

         expect(result.stakeByDRepId[keyHashDRepId]).toBe('9007199254740993');
       });

       it('throws ParseFailed on an unknown stake key shape', async () => {
         mockSpawn.mockReturnValueOnce(
           createMockChildProcess('{ "pool-keyHash-abc123": 42 }')
         );

         await expect(service.fetchDRepStake()).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.ParseFailed,
         });
       });

       it('builds the exact bulk argv with era token leading and network flag trailing', async () => {
         mockSpawn.mockReturnValueOnce(
           createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
         );

         await service.fetchDRepStake();

         expect(mockSpawn).toHaveBeenCalledTimes(1);
         expect(mockSpawn).toHaveBeenCalledWith(
           'cardano-cli',
           [
             'latest',
             'query',
             'drep-stake-distribution',
             '--all-dreps',
             '--output-json',
             '--mainnet',
           ],
           expect.any(Object)
         );
       });

       it('retries the stake query with conway when the CLI rejects the latest alias', async () => {
         mockSpawn
           .mockReturnValueOnce(
             createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
           )
           .mockReturnValueOnce(
             createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
           );

         const result = await service.fetchDRepStake();

         expect(result.stakeByDRepId[keyHashDRepId]).toBe('23137980123456');
         const secondCallArgs = mockSpawn.mock.calls[1][1] as string[];
         expect(secondCallArgs[0]).toBe('conway');
       });

       it('deduplicates concurrent stake fetches per phase', async () => {
         mockSpawn.mockReturnValueOnce(
           createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
         );

         const [r1, r2] = await Promise.all([
           service.fetchDRepStake(),
           service.fetchDRepStake(),
         ]);

         expect(r1).toBe(r2);
         expect(mockSpawn).toHaveBeenCalledTimes(1);
       });

       it('guards selfnode and missing socket like the registration phase', async () => {
         service.setSelfnodeMode(true);
         await expect(service.fetchDRepStake()).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.SelfnodeCliUnsupported,
         });

         service.setSelfnodeMode(false);
         service.setNodeSocketPath(null);
         await expect(service.fetchDRepStake()).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
         });
         expect(mockSpawn).not.toHaveBeenCalled();
       });
     });
   ```

### 3h. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts` → all tests
   pass: 26 pre-existing − 1 deleted + 9 new = **34**.
4. `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → still 8/8 (the
   store still uses only the list channel until Step 4).
5. `yarn prettier --write source/common/types/governance.types.ts source/common/ipc/api.ts source/main/governance/GovernanceQueryService.ts source/main/ipc/governanceChannel.ts source/renderer/app/ipc/governanceChannel.ts tests/jest/governance/GovernanceQueryService.spec.ts`
   — NOT the mock JSON.
6. Commit (subject only):
   `feat(gov): task-161 split drep query into registration and stake phases`

---

## Step 4: task-162 — Store drives the two phases (Loaded, then enrich)

`GovernanceStore.fetchDRepList()` sequences Phase 1 (list paints at `Loaded`, every
`votingPower` null) then Phase 2 (merge `stakeByDRepId` into the entries by `drepId`
string equality). Phase-2 failure keeps the list and surfaces the NEW
`DRepErrorBanner rankingUnavailable` (PD-1) with `—` + "unavailable" tooltip; while
Phase 2 runs, `—` carries the "Loading voting power…" tooltip (PD-10). Manual Refresh
re-runs both phases via the same `fetchDRepList()`. **Reshuffle scope note (PD-4):**
Reshuffle/seed do not exist until slice-5 task-118; this task's "Reshuffle reseeds
without re-querying" AC half is forward-compatibility only — the two-phase path
re-queries exclusively inside `fetchDRepList()` and adds no seed coupling. Do not
build any Reshuffle control.

### 4a. `source/renderer/app/stores/GovernanceStore.ts`

**Imports** (`:4` pre-edit) — currently exactly:

```ts
import { governanceDRepListChannel } from '../ipc/governanceChannel';
```

Replace with:

```ts
import {
  governanceDRepListChannel,
  governanceDRepStakeChannel,
} from '../ipc/governanceChannel';
```

**Enum** — after the closing brace of `GovernanceRefreshState` (`:29-35` pre-edit),
add:

```ts
export enum VotingPowerEnrichState {
  Idle = 'idle',
  Loading = 'loading',
  Loaded = 'loaded',
  Failed = 'failed',
}
```

**Observable** — after the `lastFetchedAt` observable (`:59-60` pre-edit):

```ts
  /** Unix timestamp (ms) when data was last successfully fetched. */
  @observable lastFetchedAt: number | null = null;
```

add:

```ts
  /** Phase-2 voting-power enrichment lifecycle, independent of the list. */
  @observable votingPowerState: VotingPowerEnrichState =
    VotingPowerEnrichState.Idle;
```

**Computed** — after the `drepCount` computed (`:84-86` pre-edit), add:

```ts
  @computed get isRankingUnavailable(): boolean {
    return this.votingPowerState === VotingPowerEnrichState.Failed;
  }
```

**Two-phase action** — replace the whole `fetchDRepList` method **including its doc
comment** (`:90-133` pre-edit, starting `/**\n   * Fetch the DRep list from the main
process.` and ending at the closing brace after the `catch` block) with:

```ts
  /**
   * Fetch the DRep directory in two phases: registrations paint the list,
   * then the stake distribution enriches voting power. Deduplicates
   * in-flight requests locally, including the enrich window.
   */
  @action
  async fetchDRepList(): Promise<void> {
    // A re-entrant refresh during the enrich window would restart Phase 1
    // mid-merge, so the guard covers both phases.
    if (
      this.refreshState === GovernanceRefreshState.Loading ||
      this.refreshState === GovernanceRefreshState.Refreshing ||
      this.votingPowerState === VotingPowerEnrichState.Loading
    ) {
      return;
    }

    const hasExistingData = this.drepList.length > 0;

    runInAction(() => {
      this.refreshState = hasExistingData
        ? GovernanceRefreshState.Refreshing
        : GovernanceRefreshState.Loading;
      this.error = null;
    });

    try {
      const payload = await governanceDRepListChannel.request();

      runInAction(() => {
        const entries = this._rehydrateDReps(payload.dreps);
        this.drepList = entries;
        this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));
        this.refreshState = GovernanceRefreshState.Loaded;
        this.lastFetchedAt = payload.fetchedAt;
        this.error = null;
        this.votingPowerState = VotingPowerEnrichState.Loading;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      // CLI stderr can carry query context; log only the normalized type.
      logger.error('GovernanceStore: fetchDRepList failed', {
        errorType: normalized.type,
      });
      runInAction(() => {
        this.error = normalized;
        this.refreshState = hasExistingData
          ? GovernanceRefreshState.Loaded
          : GovernanceRefreshState.Failed;
      });
      return;
    }

    await this._enrichVotingPower();
  }

  /**
   * Phase 2: merge the stake distribution into the painted list by DRep id.
   * Failure keeps the list and flags ranking-unavailable — never an error
   * state for the directory itself.
   */
  @action
  private async _enrichVotingPower(): Promise<void> {
    try {
      const payload = await governanceDRepStakeChannel.request();

      runInAction(() => {
        const entries = this.drepList.map((entry) => {
          const stake = payload.stakeByDRepId[entry.drepId];
          return {
            ...entry,
            votingPower: stake ? new BigNumber(stake) : null,
          };
        });
        this.drepList = entries;
        this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));
        this.votingPowerState = VotingPowerEnrichState.Loaded;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      logger.error('GovernanceStore: voting power enrich failed', {
        errorType: normalized.type,
      });
      runInAction(() => {
        this.votingPowerState = VotingPowerEnrichState.Failed;
      });
    }
  }
```

Invariants held here: Phase-1 entries arrive with `votingPower: null` and
`_rehydrateDReps` (unchanged, `:153-161` pre-edit) keeps them null (PD-5); the merge
builds `BigNumber` from the decimal string — never `Number` (#5); a DRep absent from
the stake map stays `null` (renders `—` + unavailable tooltip — §3 `:70`, never a
silent 0); both catches log `{ errorType }` only (#2 — the renderer logger applies no
`filterLogData`); a Phase-1 failure with retained data keeps the OLD list including
its old voting power (stale-while-refresh applies to the list, not to fresh stake).

### 4b. CREATE `source/renderer/app/components/governance/_shared/DRepErrorBanner.scss`

Full file content:

```scss
.banner {
  display: flex;
  align-items: flex-start;
  gap: 8px;
  padding: 12px 16px;
  border-radius: 8px;
  font-size: 14px;
  line-height: 1.4;
  color: var(--badge-warning-fg, #b76e00);
  background: var(--badge-warning-bg, rgba(230, 162, 60, 0.12));
}

.icon {
  flex-shrink: 0;
  margin-top: 2px;
}

.message {
  max-width: 640px;
}
```

### 4c. CREATE `source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx`

Full file content:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './DRepErrorBanner.scss';

const messages = defineMessages({
  rankingUnavailable: {
    id: 'governance.drepDirectory.error.rankingUnavailable',
    defaultMessage:
      '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.',
    description: 'Non-blocking banner when the stake phase fails',
  },
});

// Only the rankingUnavailable variant ships for now; the designed
// refresh-failed variant joins this union when its owning slice lands.
export type DRepErrorBannerVariant = 'rankingUnavailable';

interface Props {
  variant: DRepErrorBannerVariant;
  intl: intlShape.isRequired;
}

function DRepErrorBanner({ variant, intl }: Props) {
  const messageByVariant = {
    rankingUnavailable: messages.rankingUnavailable,
  };

  return (
    <div className={styles.banner} role="status" data-variant={variant}>
      <svg
        className={styles.icon}
        aria-hidden="true"
        width="16"
        height="16"
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
      <span className={styles.message}>
        {intl.formatMessage(messageByVariant[variant])}
      </span>
    </div>
  );
}

export default injectIntl(DRepErrorBanner);
```

### 4d. Thread `votingPowerState` through `DRepDirectory.tsx` (post-Step-2)

**Imports**: extend the stores import (post-Step-1/2 it reads
`GovernanceRefreshState, AppDRepDirectoryEntry, GovernanceStoreError`) to also name
`VotingPowerEnrichState`:

```ts
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
  AppDRepDirectoryEntry,
  GovernanceStoreError,
} from '../../../stores/GovernanceStore';
```

and next to the Step-2 `DRepEmptyState` import add:

```ts
import DRepErrorBanner from '../_shared/DRepErrorBanner';
```

**Props**: add to the interface (after `syncProgress: number | null;`):

```ts
  votingPowerState: VotingPowerEnrichState;
```

**Destructuring**: add `votingPowerState,` after `syncProgress,` in the function
signature.

**Render**: in the `default:` branch of `renderContent()`, the refreshing badge block
reads exactly (`:132-137` pre-Step-2 numbering):

```tsx
            {refreshState === GovernanceRefreshState.Refreshing && (
              <div className={styles.refreshingBadge}>
                <LoadingSpinner />
                {intl.formatMessage(messages.refreshing)}
              </div>
            )}
```

Immediately AFTER that block (before `<DRepDirectoryList`), insert:

```tsx
            {votingPowerState === VotingPowerEnrichState.Failed && (
              <DRepErrorBanner variant="rankingUnavailable" />
            )}
```

and change the list mount from:

```tsx
            <DRepDirectoryList
              entries={drepList}
              onSelectForDelegation={onSelectForDelegation}
            />
```

to:

```tsx
            <DRepDirectoryList
              entries={drepList}
              onSelectForDelegation={onSelectForDelegation}
              votingPowerState={votingPowerState}
            />
```

### 4e. Thread through `DRepDirectoryList.tsx` (`:29-35` pre-edit)

The Props interface and signature currently read exactly:

```tsx
interface Props {
  entries: AppDRepDirectoryEntry[];
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectoryList({ entries, onSelectForDelegation, intl }: Props) {
```

Replace with:

```tsx
interface Props {
  entries: AppDRepDirectoryEntry[];
  onSelectForDelegation: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}

function DRepDirectoryList({
  entries,
  onSelectForDelegation,
  votingPowerState,
  intl,
}: Props) {
```

Add the import after the existing type import (`:6` pre-edit):

```ts
import { VotingPowerEnrichState } from '../../../stores/GovernanceStore';
```

and pass it to each card — the mount currently reads exactly (`:65-71` pre-edit):

```tsx
        {pageEntries.map((entry) => (
          <DRepCard
            key={entry.drepId}
            entry={entry}
            onSelectForDelegation={onSelectForDelegation}
          />
        ))}
```

becomes:

```tsx
        {pageEntries.map((entry) => (
          <DRepCard
            key={entry.drepId}
            entry={entry}
            onSelectForDelegation={onSelectForDelegation}
            votingPowerState={votingPowerState}
          />
        ))}
```

### 4f. Tooltips in `DRepCard.tsx`

**Import** — after the existing type import (`:9` pre-edit)

```ts
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
```

add:

```ts
import { VotingPowerEnrichState } from '../../../stores/GovernanceStore';
```

**Messages** — the `messages` block (`:12-23` pre-edit) gains two entries after
`select`:

```ts
  votingPowerLoadingTooltip: {
    id: 'governance.drepDirectory.votingPower.loadingTooltip',
    defaultMessage: '!!!Loading voting power…',
    description: 'Tooltip on the voting-power placeholder during enrichment',
  },
  votingPowerUnavailableTooltip: {
    id: 'governance.drepDirectory.votingPower.unavailableTooltip',
    defaultMessage: '!!!Stake distribution unavailable this refresh.',
    description: 'Tooltip on the voting-power placeholder when stake failed',
  },
```

**Props** (`:25-29` pre-edit) — currently exactly:

```tsx
interface Props {
  entry: AppDRepDirectoryEntry;
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}
```

becomes:

```tsx
interface Props {
  entry: AppDRepDirectoryEntry;
  onSelectForDelegation: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}
```

**Render** — the component currently begins exactly (`:44` pre-edit):

```tsx
function DRepCard({ entry, onSelectForDelegation, intl }: Props) {
  return (
```

Replace with:

```tsx
function DRepCard({
  entry,
  onSelectForDelegation,
  votingPowerState,
  intl,
}: Props) {
  // Native title/aria-label keep the placeholder accessible without a
  // PopOver dependency; loading vs unavailable follows the enrich state.
  const votingPowerTooltip =
    entry.votingPower === null
      ? intl.formatMessage(
          votingPowerState === VotingPowerEnrichState.Loading
            ? messages.votingPowerLoadingTooltip
            : messages.votingPowerUnavailableTooltip
        )
      : undefined;

  return (
```

and the voting-power span, currently exactly (`:55-57` pre-edit):

```tsx
        <span className={styles.votingPowerValue}>
          {formatVotingPower(entry.votingPower)}
        </span>
```

becomes:

```tsx
        <span
          className={styles.votingPowerValue}
          title={votingPowerTooltip}
          aria-label={votingPowerTooltip}
        >
          {formatVotingPower(entry.votingPower)}
        </span>
```

`formatVotingPower` (`:31-42` pre-edit) is NOT changed — its `—`-for-null seam
already exists.

### 4g. Container, spec, and story call sites

**`DRepDirectoryPage.tsx`** (post-Step-2): in `render()`, after
`syncProgress={networkStatus.syncProgress}` add:

```tsx
        votingPowerState={governanceStore.votingPowerState}
```

**`DRepDirectory.spec.tsx`** (post-Step-1): extend the stores import to name
`VotingPowerEnrichState`, then in `renderComponent` add a
`votingPowerState = VotingPowerEnrichState.Loaded,` default as the LAST entry of both
the destructuring and the type literal (`votingPowerState?: VotingPowerEnrichState;`),
and pass `votingPowerState={votingPowerState}` in the JSX after
`syncProgress={syncProgress}`.

**`DRepDirectory.stories.tsx`** (post-Step-2): extend the `GovernanceRefreshState`
import (`:22` pre-edit) to
`import { GovernanceRefreshState, VotingPowerEnrichState } from '…/stores/GovernanceStore';`
and add `votingPowerState={VotingPowerEnrichState.Loaded}` to the `renderDirectory`
JSX (after `syncProgress`). Optionally add one state story after
`'Node syncing — empty fallback'`:

```ts
  .add('Ranking unavailable', () => (
    <div style={CENTERED_STYLE}>
      <DRepDirectory
        drepList={baseEntries.map((entry) => ({ ...entry, votingPower: null }))}
        error={null}
        isNodeInSync
        lastFetchedAt={Date.now() - 3 * 60 * 1000}
        onRefresh={action('onRefresh')}
        onSelectForDelegation={action('onSelectForDelegation')}
        refreshState={GovernanceRefreshState.Loaded}
        syncProgress={100}
        votingPowerState={VotingPowerEnrichState.Failed}
      />
    </div>
  ))
```

**`tests/jest/governance/GovernanceStore.spec.ts`**: the module mock (`:10-12`
pre-edit) currently reads exactly:

```ts
jest.mock('../../../source/renderer/app/ipc/governanceChannel', () => ({
  governanceDRepListChannel: { request: jest.fn() },
}));
```

Replace with:

```ts
jest.mock('../../../source/renderer/app/ipc/governanceChannel', () => ({
  governanceDRepListChannel: { request: jest.fn() },
  governanceDRepStakeChannel: { request: jest.fn() },
}));
```

No other spec change is needed to stay green: the dedup test's never-resolving list
request keeps the store in `Loading`, so Phase 2 never fires; the new transition
matrix (Phase 1 → Phase 2, stake failure → `rankingUnavailable`, enrich-window dedup,
`{ errorType }`-only logging) lands in task-167 (PD-11).

### 4h. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → 8/8.
4. `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
   → all pass (default `votingPowerState: Loaded` leaves existing behavior intact).
5. `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → **20/20**
   (NFR-4 checkpoint).
6. `yarn prettier --write source/renderer/app/stores/GovernanceStore.ts source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx source/renderer/app/components/governance/drep-directory/DRepCard.tsx source/renderer/app/containers/governance/DRepDirectoryPage.tsx source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx tests/jest/governance/GovernanceStore.spec.ts storybook/stories/governance/DRepDirectory.stories.tsx`
7. Commit (subject only):
   `feat(gov): task-162 drive two-phase drep load from the store`

---

## Step 5: task-163 — Per-phase CLI timeouts, threaded per call

Replace the single static `CLI_TIMEOUT_MS` with two budgets threaded through
`_runCliQueryWithEraFallback` → `_runCliQuery` as a parameter (PD-7 — the timeout
lives in the shared `_runCliQuery`, so it MUST travel per call, not per instance):
registration phase (drep-state + tip) 10 s, stake phase 30 s. The 30 s value is
provisional pending the task-166 manual latency measurement — do not re-derive it.
`shared-design-tokens.md §6` (lines 89-104) is **already** two-phase — this step
confirms it, never rewrites it.

All anchors below are **post-Step-3** positions described by their content (Step 3
moved this file's line numbers; the quoted seams are unique in the file).

### 5a. Constants — `GovernanceQueryService.ts`

The static currently reads exactly (was `:51-52` pre-Step-3):

```ts
  /** CLI subprocess timeout budget (ms) per the shared-design-tokens refresh contract. */
  private static readonly CLI_TIMEOUT_MS = 10_000;
```

Replace with:

```ts
  /**
   * Per-phase CLI timeout budgets (ms) per the shared-design-tokens two-phase
   * refresh contract. The 30s stake budget is provisional until real
   * synced-node latency is measured.
   */
  private static readonly REGISTRATION_TIMEOUT_MS = 10_000;
  private static readonly STAKE_TIMEOUT_MS = 30_000;
```

### 5b. Thread the budget through the era-fallback wrapper

`_runCliQueryWithEraFallback` currently reads exactly (was `:223-236` pre-Step-3):

```ts
  private async _runCliQueryWithEraFallback(args: string[]): Promise<string> {
    try {
      return await this._runCliQuery(['latest', ...args]);
    } catch (error) {
      if (this._shouldRetryWithConway(error)) {
        logger.warn(
          'GovernanceQueryService: retrying governance query with conway era flag',
          { args }
        );
        return this._runCliQuery(['conway', ...args]);
      }
      throw error;
    }
  }
```

Replace with:

```ts
  private async _runCliQueryWithEraFallback(
    args: string[],
    timeoutMs: number
  ): Promise<string> {
    try {
      return await this._runCliQuery(['latest', ...args], timeoutMs);
    } catch (error) {
      if (this._shouldRetryWithConway(error)) {
        logger.warn(
          'GovernanceQueryService: retrying governance query with conway era flag',
          { args }
        );
        return this._runCliQuery(['conway', ...args], timeoutMs);
      }
      throw error;
    }
  }
```

### 5c. Thread the budget through `_runCliQuery`

Its signature currently reads exactly (was `:261` pre-Step-3):

```ts
  private _runCliQuery(args: string[]): Promise<string> {
```

Replace with:

```ts
  private _runCliQuery(args: string[], timeoutMs: number): Promise<string> {
```

and the timeout block, currently exactly (was `:323-331` pre-Step-3):

```ts
      timeout = setTimeout(() => {
        child.kill('SIGTERM');
        reject(
          new GovernanceQueryError(
            GovernanceQueryErrorType.Timeout,
            `cardano-cli DRep query timed out after ${GovernanceQueryService.CLI_TIMEOUT_MS}ms`
          )
        );
      }, GovernanceQueryService.CLI_TIMEOUT_MS);
```

becomes:

```ts
      timeout = setTimeout(() => {
        child.kill('SIGTERM');
        reject(
          new GovernanceQueryError(
            GovernanceQueryErrorType.Timeout,
            `cardano-cli DRep query timed out after ${timeoutMs}ms`
          )
        );
      }, timeoutMs);
```

### 5d. Pass the per-phase budgets at the call sites (both written in Step 3c)

In `_doFetchDRepRegistrations`, the `Promise.all` becomes:

```ts
      const [drepStateStdout, tipStdout] = await Promise.all([
        this._runCliQueryWithEraFallback(
          ['query', 'drep-state', '--all-dreps', '--output-json'],
          GovernanceQueryService.REGISTRATION_TIMEOUT_MS
        ),
        this._runCliQueryWithEraFallback(
          ['query', 'tip', '--output-json'],
          GovernanceQueryService.REGISTRATION_TIMEOUT_MS
        ),
      ]);
```

In `_doFetchDRepStake`, the stake call becomes:

```ts
      const stakeStdout = await this._runCliQueryWithEraFallback(
        ['query', 'drep-stake-distribution', '--all-dreps', '--output-json'],
        GovernanceQueryService.STAKE_TIMEOUT_MS
      );
```

After these edits, `CLI_TIMEOUT_MS` must have zero remaining references:
`grep -n "CLI_TIMEOUT_MS" source/main/governance/GovernanceQueryService.ts` → empty.

### 5e. Spec updates — `GovernanceQueryService.spec.ts` (post-Step-3)

Inside `describe('timeout behavior', …)`:

1. Rename the first test's title from
   `'emits Timeout after CLI_TIMEOUT_MS when CLI never responds'` to
   `'emits Timeout after the 10s registration budget when the CLI never responds'`
   (its body is already correct — it advances `10_001` against
   `fetchDRepRegistrations`).
2. Replace the static-pin test, currently exactly:

   ```ts
       it('has a static CLI_TIMEOUT_MS matching the design token budget', () => {
         expect((GovernanceQueryService as any).CLI_TIMEOUT_MS).toBe(10_000);
       });
   ```

   with:

   ```ts
       it('pins the per-phase budgets to the design-token contract', () => {
         expect((GovernanceQueryService as any).REGISTRATION_TIMEOUT_MS).toBe(
           10_000
         );
         expect((GovernanceQueryService as any).STAKE_TIMEOUT_MS).toBe(30_000);
       });

       it('gives the stake phase its full 30s budget before timing out', async () => {
         jest.useFakeTimers();

         mockSpawn.mockReturnValueOnce(createNeverClosingChildProcess());

         let settled = false;
         const fetchPromise = service.fetchDRepStake();
         fetchPromise.catch(() => {
           settled = true;
         });

         // Past the registration budget the stake query must still be running.
         jest.advanceTimersByTime(10_001);
         await Promise.resolve();
         await Promise.resolve();
         expect(settled).toBe(false);

         jest.advanceTimersByTime(20_000);
         await Promise.resolve();

         await expect(fetchPromise).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.Timeout,
         });

         jest.useRealTimers();
       });
   ```

   (Jest 27: no `advanceTimersByTimeAsync` — the double `await Promise.resolve()`
   flushes the microtask queue before asserting the not-yet-settled state.)

### 5f. Confirm the design tokens (read-only)

Open `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md` and
confirm §6 (lines 89-104) already states: phase-1 list ≤10 s, voting-power enrich
≤30 s with the "Loading voting power…" tooltip, rankingUnavailable banner at 30 s,
syncing soft-warning row. It does (verified 2026-07-23) — make **no edit**. If it
ever disagrees with the constants above, the constants are wrong, not the tokens.

### 5g. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts` → all pass
   (34 + 1 net new = **35**).
4. `yarn prettier --write source/main/governance/GovernanceQueryService.ts tests/jest/governance/GovernanceQueryService.spec.ts`
5. Commit (subject only):
   `feat(gov): task-163 thread per-phase cli timeout budgets`

---

## Step 6: task-164 — i18n: syncing key + voting-power tooltips (en-US + ja-JP)

Five new keys, alphabetical within the governance block, every string `!!!`-prefixed
(invariant #11). The component `defaultMessage`s were added in Steps 2 and 4; this
step lands the locale JSON entries and runs the manager. **Edit the JSON by hand —
never run prettier on locale files.**

### 6a. `source/renderer/app/i18n/locales/en-US.json` (`:284-302` pre-edit)

The governance block currently reads exactly (`:284-302`):

```json
  "governance.drepDirectory.card.select": "!!!Select for delegation",
  "governance.drepDirectory.copyButton": "!!!Copy",
  "governance.drepDirectory.copyId": "!!!Copy DRep ID",
  "governance.drepDirectory.empty": "!!!No DReps found on-chain.",
  "governance.drepDirectory.error": "!!!Could not load DRep data.",
  "governance.drepDirectory.lastUpdated": "!!!Last updated {time}",
  "governance.drepDirectory.loading": "!!!Loading DRep data…",
  "governance.drepDirectory.pagination.next": "!!!Next",
  "governance.drepDirectory.pagination.pageInfo": "!!!Page {current} of {total}",
  "governance.drepDirectory.pagination.previous": "!!!Previous",
  "governance.drepDirectory.refresh": "!!!Refresh",
  "governance.drepDirectory.refreshing": "!!!Refreshing…",
  "governance.drepDirectory.retry": "!!!Retry",
  "governance.drepDirectory.source.onChain": "!!!On-chain",
  "governance.drepDirectory.status.active": "!!!Active",
  "governance.drepDirectory.status.inactive": "!!!Inactive",
  "governance.drepDirectory.title": "!!!DRep Directory",
  "governance.drepDirectory.votingPower": "!!!Voting power",
  "governance.tabs.directory": "!!!Directory",
```

Insert the five new lines at their alphabetical positions so the block becomes:

```json
  "governance.drepDirectory.card.select": "!!!Select for delegation",
  "governance.drepDirectory.copyButton": "!!!Copy",
  "governance.drepDirectory.copyId": "!!!Copy DRep ID",
  "governance.drepDirectory.empty": "!!!No DReps found on-chain.",
  "governance.drepDirectory.empty.noSync": "!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.",
  "governance.drepDirectory.error": "!!!Could not load DRep data.",
  "governance.drepDirectory.error.rankingUnavailable": "!!!Voting power data unavailable this refresh. Ranking-based filters disabled.",
  "governance.drepDirectory.lastUpdated": "!!!Last updated {time}",
  "governance.drepDirectory.loading": "!!!Loading DRep data…",
  "governance.drepDirectory.pagination.next": "!!!Next",
  "governance.drepDirectory.pagination.pageInfo": "!!!Page {current} of {total}",
  "governance.drepDirectory.pagination.previous": "!!!Previous",
  "governance.drepDirectory.refresh": "!!!Refresh",
  "governance.drepDirectory.refreshing": "!!!Refreshing…",
  "governance.drepDirectory.retry": "!!!Retry",
  "governance.drepDirectory.source.onChain": "!!!On-chain",
  "governance.drepDirectory.status.active": "!!!Active",
  "governance.drepDirectory.status.inactive": "!!!Inactive",
  "governance.drepDirectory.syncing": "!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes.",
  "governance.drepDirectory.title": "!!!DRep Directory",
  "governance.drepDirectory.votingPower": "!!!Voting power",
  "governance.drepDirectory.votingPower.loadingTooltip": "!!!Loading voting power…",
  "governance.drepDirectory.votingPower.unavailableTooltip": "!!!Stake distribution unavailable this refresh.",
  "governance.tabs.directory": "!!!Directory",
```

The en strings are byte-identical to the component `defaultMessage`s from Steps 2/4
and to the §9 canonical rows — do not paraphrase.

### 6b. `source/renderer/app/i18n/locales/ja-JP.json` (`:284-301` pre-edit)

The ja-JP governance block occupies the same key order (`:284` is
`"governance.drepDirectory.card.select": "!!!委任先として選択",`). Insert the five
keys at the identical alphabetical positions:

- after `"governance.drepDirectory.empty": …` insert
  ```json
  "governance.drepDirectory.empty.noSync": "!!!ノードは同期中です。DRepデータはノードが最新ブロックに到達すると利用できるようになります。",
  ```
- after `"governance.drepDirectory.error": …` insert
  ```json
  "governance.drepDirectory.error.rankingUnavailable": "!!!今回の更新では投票権データを利用できません。ランキングに基づくフィルターは無効になります。",
  ```
- after `"governance.drepDirectory.status.inactive": …` insert
  ```json
  "governance.drepDirectory.syncing": "!!!ノードは同期中です({progress}%)。同期が完了するまでDRepリストは不完全な場合があります。",
  ```
- after `"governance.drepDirectory.votingPower": …` insert
  ```json
  "governance.drepDirectory.votingPower.loadingTooltip": "!!!投票権を読み込み中…",
  "governance.drepDirectory.votingPower.unavailableTooltip": "!!!今回の更新ではステーク分布を利用できません。",
  ```

Keep each line's two-space indentation identical to its neighbors. The `!!!` marker
stays on the ja-JP strings too (both locales keep markers until the final manual
review batch gate — tokens §9 `:222`). The `{progress}` placeholder name must match
the en key exactly.

### 6c. Run the manager, verify, commit

1. `yarn i18n:manage` (= `i18n:extract` + `i18n:check`). This command has never been
   exercised in this devcontainer (slices 1-3 added no copy) and may fail under Node
   v24. **Report the result honestly.** If it fails for environment reasons:
   - verify placement manually:
     `grep -n "drepDirectory.syncing\|empty.noSync\|rankingUnavailable\|loadingTooltip\|unavailableTooltip" source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json`
     → 5 hits per file at the positions above;
   - record the environment failure as verification debt in the task-164 tracker
     `statusReason`.
   If it succeeds and rewrites its own managed files (e.g.
   `translations/messages.json`), include those rewrites in the commit — they are the
   sanctioned exception to the no-JSON-reformat rule (NFR-5).
2. `node_modules/.bin/tsc --noEmit` → zero errors (nothing typed changed, sanity
   only).
3. `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
   → all pass (the ja-JP render test now resolves real keys; the missing-key console
   warnings from Steps 2/4 disappear).
4. Do NOT run prettier in this step (JSON only).
5. Commit (subject only):
   `feat(gov): task-164 add syncing and voting-power i18n copy`

---

# PART 2: Steps 7-13

> Anchors in this part were verified against the same worktree/base as PART 1 on
> 2026-07-23. Where a PART 1 step already edited a file, the anchor is described
> **post-Step-N** by its content (the quoted seam is unique in the file); all other
> anchors are live pre-edit line numbers.

**Files touched in PART 2 (complete list — nothing else):**

| # | File | Steps | Action |
|---|---|---|---|
| 1 | `.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md` | 7 | EDIT (insert one section) |
| 2 | `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md` | 8 | EDIT (one Risks cell) |
| 3 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | 9 | EDIT (post-Steps-1/4) |
| 4 | `tests/jest/governance/GovernanceStore.spec.ts` | 9 | EDIT (post-Step-4g) |
| 5 | `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` | 9 | CREATE |
| 6 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | 9 | EDIT (harness stubs only) |
| 7 | `source/main/utils/setupLogging.ts` | 10 | EDIT |
| 8 | `source/main/config.ts` | 10 | EDIT (one array entry) |
| 9 | `source/main/ipc/governanceChannel.ts` | 10 | EDIT (post-Step-3d) |
| 10 | `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md` | 10 | EDIT (append §12) |
| 11 | `tests/jest/governance/logDRepStateSnapshot.spec.ts` | 10 | CREATE |
| 12 | `source/common/types/governance.types.ts` | 11 | EDIT (post-Step-3a) |
| 13 | `source/main/governance/GovernanceQueryService.ts` | 11 | EDIT (post-Steps-3/5) |
| 14 | `tests/jest/governance/GovernanceQueryService.spec.ts` | 11 | EDIT (post-Steps-3/5) |
| 15 | `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` | 11 | CREATE |
| 16 | `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` | 13 | EDIT (hand-edit, never prettier) |
| 17 | `.agent/plans/governance/drep-discovery/task-plans/ux-refinement-PRD.md` | 13 | EDIT (Final Outcome) |
| 18 | `.agent/plans/governance/drep-discovery/task-plans/ux-refinement-code-review.md` | 13 | APPEND |
| 19 | `.agent/plans/governance/drep-discovery/research/ux-refinement-findings.md` | 13 | CREATE (only if durable findings) |

Do NOT touch any slice-1/2/3 planning doc under `task-plans/` (closed precedent), and
never run prettier on JSON or Markdown files in any step below.

---

## Step 7: task-165 — Document the ID-only v1 directory limitation

Docs-only task. The plan already carries the ratified Key Decisions row
(`governance-drep-discovery-plan.md:165`, verified live 2026-07-23), which reads
exactly:

```md
| Directory names (v1) | Directory cards and search are **DRep-ID-only** in v1. Verified `givenName` (CIP-119) appears only in the detail view (anchor-1) and confirmation; directory-wide name + name-search await a future bulk anchor-prefetch phase. (UX refinement, 2026-06-15.) |
```

That row is verified, **not edited**. What is missing is an explicit section in the
directory design doc (FR-12). No code changes anywhere in this step. Locked
invariants in play: none directly — but the section must not contradict #10
(byte-equality identity display) or the §11 search contract.

### 7a. Insert the section into `designs/drep-discovery-design.md`

The seam (`:236-238` pre-edit): the Filter/Search section's last paragraph ends
exactly with

```md
… Search results are sorted by relevance only.
```

and the next heading (`:238`) reads exactly:

```md
## Hardware Wallet Confirmation
```

Between them (blank line above and below), insert:

```md
## Directory Identity: ID-Only in v1

v1 directory cards and search are **DRep-ID-only**. Card identity is the dual-ID
display (`DRepIdDisplay`: CIP-129 + CIP-105 + copy) — no name field exists on the
card, and no card may grow one in v1. The card remains fully usable on ID alone:
status badge (tokens §1), voting power (enriched by load Phase 2; `—` with a
loading/unavailable tooltip until stake lands), on-chain source label, and the
View details / Select for delegation CTAs carry the complete v1 interaction with
no name dependency.

Verified `givenName` (CIP-119) appears **only** in the detail view (anchor-1,
after fetch + hash verification) and in the delegation confirmation.
Directory-wide names and name search await a future bulk anchor-prefetch phase —
see [shared-design-tokens.md §11](shared-design-tokens.md) for the complete v1
ID-search contract and the plan Key Decisions row "Directory names (v1)".
```

### 7b. Verify the live code matches the documented limitation (read-only)

1. `grep -rn "givenName" source/renderer/app/components/governance/` → must print
   nothing (no card renders a name — AC-2). Verified pre-write: the live
   `DRepCard.tsx` body (`:44-69`) renders only `DRepStatusBadge`, `DRepIdDisplay`,
   the voting-power span, `DRepSourceLabel`, and the Select `Button`.
2. `grep -n "Directory names (v1)" .agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`
   → prints line 165 (plan half of AC-1 already satisfied; design half added in 7a).
3. `grep -n "ID-Only in v1" .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md`
   → prints the new heading.

### 7c. Commit

No tsc/lint/jest/prettier (Markdown-only change). Commit (subject only):
`docs(gov): task-165 document the id-only v1 directory limitation`

---

## Step 8: task-166 — Record the deferred fixture + latency follow-up (autonomous portion ONLY)

**MANUAL-EXECUTION GUARD (PD in prompt + PRD Per-Task Contract): the task-166
remainder — mainnet fixture capture and p50/p95 latency measurement — is locked
`manual_execution`. Do NOT attempt it, do not touch any node socket, do not promote
any fixture. The task's `status` stays `partial` at close-out (Step 13).** The
autonomous portion is exactly one doc edit: make the plan Risks mitigation name all
three remaining manual items explicitly (FR-13).

### 8a. Extend the plan Risks row — `governance-drep-discovery-plan.md:345`

The row currently reads exactly (one table line):

```md
| 10s CLI timeout is an unvalidated guess; `drep-state --all-dreps --include-stake` is officially "potentially expensive" and has never run against mainnet-scale data | Two-phase load moves stake off the first-paint path; stake-phase timeout raised to 30s; deferred follow-up to capture real synced-node fixtures + p50/p95 latency and re-derive the budget. |
```

Replace ONLY the mitigation cell (right column; the risk cell stays byte-identical),
so the row becomes:

```md
| 10s CLI timeout is an unvalidated guess; `drep-state --all-dreps --include-stake` is officially "potentially expensive" and has never run against mainnet-scale data | Two-phase load moves stake off the first-paint path; stake-phase timeout raised to 30s (provisional). Preprod fixture captured 2026-06-16 (cardano-cli 11.0.0.0 / cardano-node 11.0.1, epoch 295, 258 DReps) at `research/drep-state-preprod-epoch295-sample.json`. Deferred manual follow-up (task-166 remainder, `manual_execution`): (1) capture the mainnet fixture; (2) measure p50/p95 latency for both phases on a synced node and re-derive the 30s stake budget; (3) promote the real captures into a committed `tests/jest/governance/` fixture replacing the synthetic mocks — including confirming the real `drep-stake-distribution` container/key shape assumed by the dual-shape parser. |
```

### 8b. Verify and commit

1. `grep -c "drep-stake-distribution" .agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`
   → at least 2 (the pre-existing First-load row plus this new mitigation text).
2. Confirm `research/drep-state-preprod-epoch295-sample.json` still exists (it does,
   verified live) — the mitigation references it.
3. Commit (subject only):
   `docs(gov): task-166 record the deferred fixture and latency follow-up`

The tracker `statusReason`/`evidence` update for this task lands in Step 13 with all
the others; the status value it must keep is `partial`.

---

## Step 9: task-167 — Jest: sync banner, two-phase transitions, ranking-unavailable

The phase's consolidated behavior matrix (PD-11): +7 component tests, +5 store tests,
a NEW 3-test container spec, plus a small harness-stub update to the existing
integrated flow spec. Locked invariants inline: **#2** — the new store tests assert
`{ errorType }`-only logging adversarially (the renderer logger applies no
`filterLogData`); the floor suite must stay **20/20, never below** (NFR-4). **#5** —
the merge test pins BigNumber-from-decimal-string and the never-silent-0 rule (a
DRep absent from the stake map stays `null`). No production file changes in this
step — test files only.

### 9a. Component tests — `DRepDirectory.spec.tsx` (post-Steps-1/4)

After Steps 1 and 4 the `renderComponent` helper accepts `isNodeInSync`,
`syncProgress`, and `votingPowerState` (defaults `true` / `100` /
`VotingPowerEnrichState.Loaded`) and the stores import already names
`VotingPowerEnrichState` — no import or helper change is needed. The last test in
the file ends exactly with:

```tsx
    expect(onSelectForDelegation).toHaveBeenCalledTimes(1);
    expect(onSelectForDelegation).toHaveBeenCalledWith(baseEntries[0].drepId);
  });
});
```

Insert the following seven tests immediately BEFORE the final `});` (i.e. still
inside `describe('DRepDirectory', …)`, after the `onSelectForDelegation` test's
closing `});`):

```tsx
  it('renders the persistent syncing banner with the floored live sync %', () => {
    renderComponent({ isNodeInSync: false, syncProgress: 87.6 });

    expect(
      screen.getByText(
        '!!!Your node is still syncing (87%). The DRep list may be incomplete until sync completes.'
      )
    ).toBeInTheDocument();
    // The soft warning never hides the data underneath it.
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });

  it('renders 0% in the syncing banner when syncProgress is null mid-boot', () => {
    renderComponent({ isNodeInSync: false, syncProgress: null });

    expect(screen.getByText(/still syncing \(0%\)/)).toBeInTheDocument();
  });

  it('does not render the syncing banner when the node is in sync', () => {
    renderComponent();

    expect(screen.queryByText(/still syncing/)).not.toBeInTheDocument();
  });

  it('falls back to the noSync empty state when syncing yields zero DReps', () => {
    renderComponent({ drepList: [], isNodeInSync: false, syncProgress: 42 });

    expect(
      screen.getByText(
        '!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!No DReps found on-chain.')
    ).not.toBeInTheDocument();
  });

  it('falls back to the noSync empty state on an availability failure while syncing', () => {
    renderComponent({
      drepList: [],
      error: {
        message: 'Cardano node socket path is not available.',
        type: 'SOCKET_UNAVAILABLE',
      },
      isNodeInSync: false,
      refreshState: GovernanceRefreshState.Failed,
      syncProgress: 42,
    });

    expect(
      screen.getByText(
        /DRep data becomes available once the node reaches the tip/
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!Could not load DRep data.')
    ).not.toBeInTheDocument();
  });

  it('keeps the retained list without the fallback when syncing with data present', () => {
    renderComponent({ isNodeInSync: false, syncProgress: 42 });

    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
    expect(
      screen.queryByText(/DRep data becomes available/)
    ).not.toBeInTheDocument();
  });

  it('drives the — tooltip by enrich state and shows the rankingUnavailable banner on stake failure', () => {
    const { unmount } = renderComponent({
      drepList: [{ ...baseEntries[0], votingPower: null }],
      votingPowerState: VotingPowerEnrichState.Loading,
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Loading voting power…'
    );
    expect(
      screen.queryByText(/Voting power data unavailable/)
    ).not.toBeInTheDocument();
    unmount();

    renderComponent({
      drepList: [{ ...baseEntries[0], votingPower: null }],
      votingPowerState: VotingPowerEnrichState.Failed,
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Stake distribution unavailable this refresh.'
    );
    expect(
      screen.getByText(
        '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.'
      )
    ).toBeInTheDocument();
  });
```

(The exact `!!!`-prefixed strings resolve from the real en-US.json entries Step 6
landed — the specs render through `IntlProvider` with the real locale file. The
banner/fallback tests double as the "clears at `isNodeInSync`" proof: the same
render tree with `isNodeInSync: true` provably omits both surfaces.)

### 9b. Store tests — `tests/jest/governance/GovernanceStore.spec.ts` (post-Step-4g)

Step 4g replaced only the `jest.mock` factory. Four mechanical edits, then five new
tests.

1. **Imports** — the channel import (`:3` pre-Step-4, unchanged by it) reads exactly:

   ```ts
   import { governanceDRepListChannel } from '../../../source/renderer/app/ipc/governanceChannel';
   ```

   Replace with:

   ```ts
   import {
     governanceDRepListChannel,
     governanceDRepStakeChannel,
   } from '../../../source/renderer/app/ipc/governanceChannel';
   ```

   The store import (`:2`) reads exactly:

   ```ts
   import GovernanceStore from '../../../source/renderer/app/stores/GovernanceStore';
   ```

   Replace with:

   ```ts
   import GovernanceStore, {
     GovernanceRefreshState,
     VotingPowerEnrichState,
   } from '../../../source/renderer/app/stores/GovernanceStore';
   import { logger } from '../../../source/renderer/app/utils/logging';
   ```

2. **Mock handles** — after the line (`:14` pre-edit):

   ```ts
   const mockRequest = governanceDRepListChannel.request as jest.Mock;
   ```

   add:

   ```ts
   const mockStakeRequest = governanceDRepStakeChannel.request as jest.Mock;

   /** Drain pending async continuations behind a macrotask boundary. */
   const flushAsync = () => new Promise((resolve) => setTimeout(resolve, 0));

   const DREP_ID = 'drep1xj23tk3yqyv7cqv7jn9mkz6xq8c7e5m3s2w1v0p9n8m7l6k5j';

   const phase1Payload = () => ({
     dreps: [
       {
         anchor: null,
         drepActivity: 8,
         drepId: DREP_ID,
         status: 'active' as const,
         votingPower: null,
       },
     ],
     epoch: 512,
     fetchedAt: 1_750_000_000_000,
   });
   ```

3. **beforeEach** — inside the existing `beforeEach`, after
   `mockRequest.mockReset();` add:

   ```ts
       mockStakeRequest.mockReset();
   ```

4. **New tests** — insert immediately BEFORE the file's final `});` (after the
   dedup test's closing `});`):

```ts
  it('paints the list from Phase 1 with null voting power, then merges stake by DRep id', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    let resolveStake: (value: unknown) => void = () => {};
    mockStakeRequest.mockImplementation(
      () =>
        new Promise((resolve) => {
          resolveStake = resolve;
        })
    );

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    void store.fetchDRepList();
    await flushAsync();

    // Phase 1 painted: list visible, voting power still null, enrich running.
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.drepList).toHaveLength(1);
    expect(store.drepList[0].votingPower).toBeNull();
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loading);

    resolveStake({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId: { [DREP_ID]: '9007199254740993' },
    });
    await flushAsync();

    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loaded);
    expect(store.drepList[0].votingPower?.toFixed()).toBe('9007199254740993');
  });

  it('keeps voting power null for DReps absent from the stake map', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId: {},
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    // Never a silent fallback to 0 — absence renders as unavailable.
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loaded);
    expect(store.drepList[0].votingPower).toBeNull();
  });

  it('keeps the painted list and flags ranking unavailable when the stake phase fails', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.drepList).toHaveLength(1);
    expect(store.drepList[0].votingPower).toBeNull();
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Failed);
    expect(store.isRankingUnavailable).toBe(true);
    // A stake failure never becomes a directory error.
    expect(store.error).toBeNull();
  });

  it('deduplicates a refresh fired during the voting-power enrich window', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockImplementation(() => new Promise(() => {}));

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    void store.refresh();
    await flushAsync();
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loading);

    void store.refresh();
    await flushAsync();

    expect(mockRequest).toHaveBeenCalledTimes(1);
  });

  it('logs only the normalized errorType from both phase failures', async () => {
    const errorSpy = jest.spyOn(logger, 'error').mockImplementation(() => {});
    const sensitive =
      'query failed for drep1qqsensitive000000000000000000000000000000000 drep-alwaysAbstain';

    mockRequest.mockRejectedValueOnce({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: sensitive,
      details: sensitive,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(errorSpy).toHaveBeenCalledWith(
      'GovernanceStore: fetchDRepList failed',
      expect.objectContaining({ errorType: 'QUERY_FAILED' })
    );
    // Phase 2 never fires after a Phase-1 failure.
    expect(mockStakeRequest).not.toHaveBeenCalled();

    errorSpy.mockClear();
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'PARSE_FAILED',
      message: sensitive,
    });
    await store.fetchDRepList();

    expect(errorSpy).toHaveBeenCalledWith(
      'GovernanceStore: voting power enrich failed',
      expect.objectContaining({ errorType: 'PARSE_FAILED' })
    );

    const serializedCalls = JSON.stringify(errorSpy.mock.calls);
    expect(serializedCalls).not.toContain('drep1qq');
    expect(serializedCalls).not.toContain('drep-alwaysAbstain');
    errorSpy.mockRestore();
  });
```

(Assertion style honors the prettier-2.1.2 rule: every object argument next to a
string uses `expect.objectContaining`. `flushAsync` uses a real macrotask so the
intermediate Phase-1-painted state is observable regardless of how babel chains
the async continuations — no fake timers in this suite.)

### 9c. CREATE `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx`

Full file content (the `Provider`/`Router` harness follows the
`VotingGovernancePage.spec.tsx` precedent; `refreshState: Loaded` on mount keeps
`componentDidMount` from firing its own refresh, so the reaction's calls are
countable in isolation):

```tsx
import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import { act, cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { ROUTES } from '../../routes-config';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../stores/GovernanceStore';
import DRepDirectoryPage from './DRepDirectoryPage';

const drepEntry = {
  anchor: null,
  drepActivity: 12,
  drepId: 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b',
  status: 'active' as const,
  votingPower: new BigNumber('23137980123456'),
};

const buildGovernanceStore = () => ({
  drepList: [drepEntry],
  error: null,
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  votingPowerState: VotingPowerEnrichState.Loaded,
});

const renderPage = ({
  isNodeInSync = true,
  syncProgress = 100,
}: { isNodeInSync?: boolean; syncProgress?: number | null } = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore();
  const history = createMemoryHistory({
    initialEntries: [ROUTES.GOVERNANCE.DREPS],
  });
  const view = render(
    <Provider stores={{ governance, networkStatus } as any}>
      <IntlProvider locale="en-US" messages={translations}>
        <Router history={history}>
          <Route
            path={ROUTES.GOVERNANCE.DREPS}
            component={DRepDirectoryPage}
          />
        </Router>
      </IntlProvider>
    </Provider>
  );
  return { governance, networkStatus, ...view };
};

describe('DRepDirectoryPage', () => {
  afterEach(cleanup);

  it('passes node-sync state into the directory (banner shows the live %)', () => {
    renderPage({ isNodeInSync: false, syncProgress: 87 });

    expect(screen.getByText(/still syncing \(87%\)/)).toBeInTheDocument();
  });

  it('refetches exactly once when the node reaches the tip', () => {
    const { governance, networkStatus } = renderPage({
      isNodeInSync: false,
      syncProgress: 99,
    });
    expect(governance.refresh).not.toHaveBeenCalled();

    act(() => {
      runInAction(() => {
        networkStatus.isNodeInSync = true;
        networkStatus.syncProgress = 100;
      });
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
  });

  it('disposes the sync reaction on unmount', () => {
    const { governance, networkStatus, unmount } = renderPage({
      isNodeInSync: false,
      syncProgress: 99,
    });
    unmount();

    runInAction(() => {
      networkStatus.isNodeInSync = true;
    });

    expect(governance.refresh).not.toHaveBeenCalled();
  });
});
```

### 9d. Harness stubs — `VotingGovernancePage.spec.tsx` (live anchors; PART 1 never touched this file)

After Steps 1/4 the container reads `networkStatus.isNodeInSync` / `syncProgress`
and `governance.votingPowerState`; the integrated flow harness must stub them or
its DReps-route renders would show a spurious "0%" syncing banner. Three edits:

1. The stores import (`:26` pre-edit) reads exactly:

   ```ts
   import { GovernanceRefreshState } from '../../stores/GovernanceStore';
   ```

   Replace with:

   ```ts
   import {
     GovernanceRefreshState,
     VotingPowerEnrichState,
   } from '../../stores/GovernanceStore';
   ```

2. The governance stub (`:113-119` pre-edit) reads exactly:

   ```ts
     governance: {
       drepList: [drepEntry],
       error: null,
       lastFetchedAt: Date.now() - 60_000,
       refresh: jest.fn(),
       refreshState: GovernanceRefreshState.Loaded,
     },
   ```

   Replace with:

   ```ts
     governance: {
       drepList: [drepEntry],
       error: null,
       lastFetchedAt: Date.now() - 60_000,
       refresh: jest.fn(),
       refreshState: GovernanceRefreshState.Loaded,
       votingPowerState: VotingPowerEnrichState.Loaded,
     },
   ```

3. The networkStatus stub (`:124` pre-edit) reads exactly:

   ```ts
     networkStatus: { isSynced: true, syncPercentage: 100 },
   ```

   Replace with:

   ```ts
     networkStatus: {
       isNodeInSync: true,
       isSynced: true,
       syncPercentage: 100,
       syncProgress: 100,
     },
   ```

### 9e. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
   → **19** passed (12 + 7).
4. `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → **13** passed (8 + 5).
5. `yarn test:jest source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx`
   → **3** passed.
6. `yarn test:jest source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
   → **7** passed (unchanged count — harness stubs only).
7. `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → **20/20**
   (NFR-4 checkpoint).
8. `yarn prettier --write source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx tests/jest/governance/GovernanceStore.spec.ts source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
9. Commit (subject only):
   `test(gov): task-167 cover sync banner and two-phase load behavior`

---

## Step 10: task-168 — DRep-state snapshot log file + support bundling

**Invariant #2's ONE documented exception (inline, binding):** the snapshot file is
public on-chain directory data that deliberately **bypasses** `filterLogData`
(which would redact every `drepId` and make the file useless for support), and it
must **NEVER** contain the user's own vote/delegation. The writer accepts only
`DRepListQueryPayload`, which structurally carries no wallet/vote data (PD-9);
no other call site may ever feed it anything else. Everything else in this step —
including the write-failure catch — stays under the floor. Invariant #1: the file
is written locally from the same local-node payload; no network involvement.

### 10a. Writer — `source/main/utils/setupLogging.ts`

**Imports.** After the existing line (`:6` pre-edit):

```ts
import { pubLogsFolderPath, appLogsFolderPath } from '../config';
```

insert:

```ts
import { environment } from '../environment';
```

and after the logging-types import block (`:13-19` pre-edit, ending
`} from '../../common/types/logging.types';`), insert:

```ts
import type { DRepListQueryPayload } from '../../common/types/governance.types';
```

**Writer.** `logStateSnapshot` ends (`:169-175` pre-edit) exactly with:

```ts
  const stateSnapshotFilePath = path.join(
    pubLogsFolderPath,
    'State-snapshot.json'
  );
  fs.writeFileSync(stateSnapshotFilePath, JSON.stringify(messageBody));
  return messageBody;
};
```

Immediately AFTER that closing `};` (and BEFORE
`export const generateWalletMigrationReport` at `:176` pre-edit), insert:

```ts
/**
 * Public on-chain DRep directory snapshot for support bundles. This payload
 * deliberately bypasses filterLogData: every value is public ledger data,
 * and the payload type cannot carry the user's delegation or vote.
 */
export const logDRepStateSnapshot = (
  payload: DRepListQueryPayload
): MessageBody => {
  const { network, os, platformVersion, version } = environment;
  const messageBodyParams: ConstructMessageBodyParams = {
    at: new Date().toISOString(),
    env: `${network}:${os}:${platformVersion}`,
    ns: ['daedalus', `v${version}`, `*${network}*`],
    data: (payload as unknown) as ConstructMessageBodyParams['data'],
    msg: 'Updating DRep-state-snapshot.json file',
    pid: '',
    sev: 'info',
    thread: '',
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  const drepStateSnapshotFilePath = path.join(
    pubLogsFolderPath,
    'DRep-state-snapshot.json'
  );
  fs.writeFileSync(drepStateSnapshotFilePath, JSON.stringify(messageBody));
  return messageBody;
};
```

(`environment` exposes `network` / `os` / `platformVersion` / `version` — verified
live at `source/main/environment.ts:93-130`; `fs.writeFileSync` overwrites by
default, satisfying AC-1's overwrite requirement. `constructMessageBody` applies no
filtering — verified at `source/common/utils/logging.ts:86-112` — which is what
makes the bypass structural rather than accidental.)

### 10b. Register the filename — `source/main/config.ts` (`:137-145` pre-edit)

The array currently reads exactly:

```ts
export const ALLOWED_LOGS = [
  'Daedalus.json',
  'System-info.json',
  'Daedalus-versions.json',
  'State-snapshot.json',
  'Wallet-migration-report.json',
  'cardano-wallet.log',
  'node.log',
];
```

Insert `'DRep-state-snapshot.json',` after `'State-snapshot.json',`:

```ts
export const ALLOWED_LOGS = [
  'Daedalus.json',
  'System-info.json',
  'Daedalus-versions.json',
  'State-snapshot.json',
  'DRep-state-snapshot.json',
  'Wallet-migration-report.json',
  'cardano-wallet.log',
  'node.log',
];
```

Never insert at index 0 — `source/main/ipc/get-logs.ts:28/:36` uses
`ALLOWED_LOGS[0]` (`Daedalus.json`) for name parsing, and `isFileAllowed`
(`get-logs.ts:46-47`) is a plain name-membership check, so mid-array insertion is
safe and sufficient for bundling.

### 10c. Hook the writer on Phase-1 success — `source/main/ipc/governanceChannel.ts` (post-Step-3d)

Step 3d rewrote this file; anchors below quote its post-Step-3d content.

**Import.** After:

```ts
import { logger } from '../utils/logging';
```

insert:

```ts
import { logDRepStateSnapshot } from '../utils/setupLogging';
```

**Handler.** The Phase-1 handler body reads exactly:

```ts
  governanceDRepListChannel.onRequest(async (_request) => {
    logger.info('Governance IPC: DRep list requested from renderer');
    try {
      return await GovernanceQueryService.getInstance().fetchDRepRegistrations();
    } catch (error) {
```

Replace that fragment with:

```ts
  governanceDRepListChannel.onRequest(async (_request) => {
    logger.info('Governance IPC: DRep list requested from renderer');
    try {
      const payload = await GovernanceQueryService.getInstance().fetchDRepRegistrations();
      // Support-bundle snapshot only; a write failure must never fail the
      // directory response.
      try {
        logDRepStateSnapshot(payload);
      } catch (snapshotError) {
        logger.error('Governance IPC: DRep-state snapshot write failed', {
          error: snapshotError,
        });
      }
      return payload;
    } catch (error) {
```

(The rest of the handler — the `logger.error` + `toGovernanceIpcError` re-throw —
and the stake handler stay byte-identical. A snapshot-write failure is an fs error
carrying only a file path — no DRep data — so logging it whole keeps the floor.)

### 10d. Document the boundary — `shared-design-tokens.md` (append §12)

The file currently ends (`:246`, last line) exactly with:

```md
… ID search semantics above are the complete v1 contract.
```

Append after it (blank line, then):

```md
## 12. DRep-State Snapshot Log Boundary (the one sanitization-floor exception)

`Logs/pub/DRep-state-snapshot.json` is written by `logDRepStateSnapshot`
(`source/main/utils/setupLogging.ts`) on every successful Phase-1 registration
response in the main governance IPC handler, overwriting the previous file, and is
bundled into support archives via `ALLOWED_LOGS`.

- **It deliberately BYPASSES `filterLogData`.** The payload is the public on-chain
  DRep directory (`DRepListQueryPayload`: DRep ids, status, activity, anchor
  url+hash pointers, epoch, fetch timestamp). `filterLogData` would redact every
  `drepId` and make the file useless for support diagnosis.
- **It must NEVER contain the user's own vote or delegation.** The writer accepts
  only the directory payload type, which structurally carries no wallet or vote
  data. No other call site may feed it anything else; wallet/vote state everywhere
  else remains under the slice-1 sanitization floor.
- The snapshot doubles as an on-chain anchor-POINTER cache (url + dataHash per
  DRep) to seed/cross-check the metadata-fetch slice. It is NOT a substitute for
  CIP-100/119 off-chain fetch + hash verification, which stays a slice-4 concern.
```

### 10e. CREATE `tests/jest/governance/logDRepStateSnapshot.spec.ts`

Full file content:

```ts
import fs from 'fs';
import os from 'os';
import path from 'path';
import { logDRepStateSnapshot } from '../../../source/main/utils/setupLogging';
import type { DRepListQueryPayload } from '../../../source/common/types/governance.types';

// main/config boots launcher configuration and throws outside an Electron
// launcher, so the log folder is redirected to a temp dir instead. jest.mock
// calls hoist above the imports, so the factories apply before setupLogging
// resolves its dependencies (same pattern as GovernanceStore.spec.ts).
jest.mock('../../../source/main/config', () => {
  const nodeOs = require('os');
  const nodePath = require('path');
  const base = nodePath.join(nodeOs.tmpdir(), 'drep-snapshot-spec');
  return {
    appLogsFolderPath: base,
    pubLogsFolderPath: nodePath.join(base, 'pub'),
  };
});

jest.mock('../../../source/main/environment', () => ({
  environment: {
    network: 'mainnet',
    os: 'linux',
    platformVersion: '0',
    version: '0.0.0',
  },
}));

jest.mock('electron-log-daedalus', () => ({
  transports: { console: {}, file: {}, rendererConsole: {} },
}));

// Mirrors the mocked config factory above.
const pubLogsFolderPath = path.join(os.tmpdir(), 'drep-snapshot-spec', 'pub');
const SNAPSHOT_PATH = path.join(pubLogsFolderPath, 'DRep-state-snapshot.json');

const publicPayload: DRepListQueryPayload = {
  dreps: [
    {
      anchor: {
        hash: 'a'.repeat(64),
        url: 'https://example.org/drep.jsonld',
      },
      drepActivity: 12,
      drepId: 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b',
      status: 'active',
      votingPower: null,
    },
  ],
  epoch: 512,
  fetchedAt: 1_750_000_000_000,
};

describe('logDRepStateSnapshot', () => {
  beforeEach(() => {
    fs.mkdirSync(pubLogsFolderPath, { recursive: true });
    if (fs.existsSync(SNAPSHOT_PATH)) {
      fs.unlinkSync(SNAPSHOT_PATH);
    }
  });

  it('writes the public directory payload with drepIds retained', () => {
    logDRepStateSnapshot(publicPayload);

    const written = fs.readFileSync(SNAPSHOT_PATH, 'utf-8');
    const parsed = JSON.parse(written);
    // The filterLogData bypass is the point: public drepIds must survive.
    expect(written).toContain(publicPayload.dreps[0].drepId);
    expect(parsed.data.dreps).toHaveLength(1);
    expect(parsed.data.epoch).toBe(512);
    expect(parsed.msg).toBe('Updating DRep-state-snapshot.json file');
  });

  it('overwrites the previous snapshot on each successful fetch', () => {
    logDRepStateSnapshot(publicPayload);
    logDRepStateSnapshot({
      ...publicPayload,
      epoch: 513,
      fetchedAt: 1_750_000_100_000,
    });

    const parsed = JSON.parse(fs.readFileSync(SNAPSHOT_PATH, 'utf-8'));
    expect(parsed.data.epoch).toBe(513);
  });

  it('never contains user vote or delegation fields', () => {
    logDRepStateSnapshot(publicPayload);

    const written = fs.readFileSync(SNAPSHOT_PATH, 'utf-8');
    // The payload type carries no wallet/vote state; pin the wire keys that
    // would betray a leak if the writer were ever fed the wrong payload.
    expect(written).not.toContain('voteKind');
    expect(written).not.toContain('chosenOption');
    expect(written).not.toContain('delegation');
  });

  it('registers the snapshot filename in ALLOWED_LOGS (source-text check)', () => {
    // Importing the real main/config throws outside an Electron launcher, so
    // membership is asserted at source level; the end-to-end bundle proof
    // stays with the release verification pass.
    const configSource = fs.readFileSync(
      path.resolve(__dirname, '../../../source/main/config.ts'),
      'utf-8'
    );
    expect(configSource).toContain("'DRep-state-snapshot.json'");
  });
});
```

### 10f. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest tests/jest/governance/logDRepStateSnapshot.spec.ts` → **4/4**.
4. `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → **20/20**
   (the exception must not erode the floor — NFR-4 checkpoint).
5. `yarn prettier --write source/main/utils/setupLogging.ts source/main/config.ts source/main/ipc/governanceChannel.ts tests/jest/governance/logDRepStateSnapshot.spec.ts`
   — never the `.md` file.
6. Commit (subject only):
   `feat(gov): task-168 write drep-state snapshot log for support bundles`

**Verification debt (record in Step 13 statusReason):** the end-to-end support-bundle
generation proof (AC-2's "appears in a generated support log bundle") cannot run in
Jest — `get-logs` requires a live Electron main process. The name-membership logic
is asserted at source level; full proof belongs to the task-125 release
verification.

---

## Step 11: task-169 — CLI robustness: structured era-retry signal + argv smoke test

PART A replaces the substring conway gate with spawn-boundary error classification
(PD-8 — no supported-era probe). PART B adds the parse-only real-binary smoke test,
**self-skipping** where `cardano-cli` is absent (it IS absent in this devcontainer —
the suite must report `skipped`, never `failed`). Locked invariants inline: **#6** —
era `latest`→`conway` fallback stays on every query, both phases; no argv change on
the happy path; network flag stays appended after the subcommand; socket stays in
`spawn.env`. **#2** — the new stderr classification changes no logged payloads;
GovernanceQueryError messages keep identifying nothing but exit codes.

### 11a. New error type — `source/common/types/governance.types.ts` (post-Step-3a; the enum block itself is untouched by Step 3)

The enum currently reads exactly:

```ts
export enum GovernanceQueryErrorType {
  SocketUnavailable = 'SOCKET_UNAVAILABLE',
  CliNotFound = 'CLI_NOT_FOUND',
  QueryFailed = 'QUERY_FAILED',
  ParseFailed = 'PARSE_FAILED',
  SelfnodeCliUnsupported = 'SELFNODE_CLI_UNSUPPORTED',
  Timeout = 'TIMEOUT',
  Unknown = 'UNKNOWN',
}
```

Replace with (`UsageError` inserted after `QueryFailed`):

```ts
export enum GovernanceQueryErrorType {
  SocketUnavailable = 'SOCKET_UNAVAILABLE',
  CliNotFound = 'CLI_NOT_FOUND',
  QueryFailed = 'QUERY_FAILED',
  UsageError = 'USAGE_ERROR',
  ParseFailed = 'PARSE_FAILED',
  SelfnodeCliUnsupported = 'SELFNODE_CLI_UNSUPPORTED',
  Timeout = 'TIMEOUT',
  Unknown = 'UNKNOWN',
}
```

(The renderer's `_normalizeError` passes unknown type strings through unchanged and
the directory renders them via the generic failure branch — no renderer change is
needed.)

### 11b. Classify at the spawn boundary — `GovernanceQueryService.ts` (post-Steps-3/5)

**Signature constant.** The Step-5 timeout constants read exactly:

```ts
  private static readonly REGISTRATION_TIMEOUT_MS = 10_000;
  private static readonly STAKE_TIMEOUT_MS = 30_000;
```

Immediately after them, add:

```ts
  /**
   * Structural signature of an optparse-applicative argv rejection (bad era
   * token, invalid flag, missing required argument). Node-side query failures
   * never print it, so it gates the conway era fallback safely.
   */
  private static readonly CLI_USAGE_SIGNATURE = /(invalid (option|argument)|missing:|usage:)/i;
```

**Close handler.** In `_runCliQuery`, the exit handler currently reads exactly
(content unchanged by Steps 3/5; live `:333-346` pre-Part-2):

```ts
      child.on('close', (code) => {
        if (timeout) clearTimeout(timeout);
        if (code !== 0) {
          reject(
            new GovernanceQueryError(
              GovernanceQueryErrorType.QueryFailed,
              `cardano-cli exited with code ${code}`,
              stderr.trim() || undefined
            )
          );
          return;
        }
        resolve(stdout);
      });
```

Replace with:

```ts
      child.on('close', (code) => {
        if (timeout) clearTimeout(timeout);
        if (code !== 0) {
          const trimmedStderr = stderr.trim();
          const isUsageRejection = GovernanceQueryService.CLI_USAGE_SIGNATURE.test(
            trimmedStderr
          );
          reject(
            new GovernanceQueryError(
              isUsageRejection
                ? GovernanceQueryErrorType.UsageError
                : GovernanceQueryErrorType.QueryFailed,
              `cardano-cli exited with code ${code}`,
              trimmedStderr || undefined
            )
          );
          return;
        }
        resolve(stdout);
      });
```

### 11c. Pure error-class retry gate

`_shouldRetryWithConway` currently reads exactly (content unchanged by Steps 3/5;
live `:238-254` pre-Part-2):

```ts
  private _shouldRetryWithConway(error: unknown): boolean {
    if (!(error instanceof GovernanceQueryError)) {
      return false;
    }

    if (error.queryErrorType !== GovernanceQueryErrorType.QueryFailed) {
      return false;
    }

    const failureText = `${error.message}\n${
      error.details ?? ''
    }`.toLowerCase();
    return (
      failureText.includes('latest') &&
      /(invalid|unknown|expected|expecting|conway|era)/.test(failureText)
    );
  }
```

Replace with:

```ts
  private _shouldRetryWithConway(error: unknown): boolean {
    return (
      error instanceof GovernanceQueryError &&
      error.queryErrorType === GovernanceQueryErrorType.UsageError
    );
  }
```

This closes AC-3's implicit coupling: no production message needs to avoid the word
"latest" anymore. The "network not set" message (live `:263-271`, created directly
— not via the close handler — so it stays `QueryFailed`) keeps its current wording;
the constraint on it is simply gone. The era fallback now applies to **both** phase
calls automatically because both route through `_runCliQueryWithEraFallback`.

### 11d. PART A spec updates — `GovernanceQueryService.spec.ts` (post-Steps-3/5)

1. **New stderr constant** — after `LATEST_ALIAS_MISSING_STDERR` (`:68-69`
   pre-Step-3, which still reads exactly
   ``'Invalid argument `latest`\nExpected one of: conway'`` — note it matches the
   usage signature via "Invalid argument", which is why the existing positive
   era-fallback tests keep passing untouched), add:

   ```ts
   /** Node-side failure that mentions era words but is not an argv rejection. */
   const NODE_QUERY_FAILURE_STDERR =
     'MuxError MuxBearerClosed: the latest era ledger query failed unexpectedly';
   ```

   This string deliberately contains both `latest` and `era` — under the old
   substring gate it triggered a spurious conway retry; under the structured
   signal it must not.

2. **New describe block** — insert immediately BEFORE the
   `// ---- network flag injection (FP-1) ----` comment (which Step 3g's
   stake-distribution describe now directly precedes):

   ```ts
     // ---- era-retry signal ----

     describe('era-retry signal', () => {
       it('classifies an argv usage rejection as UsageError and still retries with conway', async () => {
         mockSpawn
           .mockReturnValueOnce(
             createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
           )
           .mockReturnValueOnce(
             createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
           );

         // The conway retry also rejects, so the classified error surfaces.
         await expect(service.fetchDRepStake()).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.UsageError,
         });
         expect(mockSpawn).toHaveBeenCalledTimes(2);
         const retryArgs = mockSpawn.mock.calls[1][1] as string[];
         expect(retryArgs[0]).toBe('conway');
       });

       it('does not retry with conway when both queries fail with a non-era QueryFailed', async () => {
         mockSpawn
           .mockReturnValueOnce(
             createMockChildProcess('', 1, NODE_QUERY_FAILURE_STDERR)
           )
           .mockReturnValueOnce(
             createMockChildProcess('', 1, NODE_QUERY_FAILURE_STDERR)
           );

         await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.QueryFailed,
         });
         // Both parallel phase-1 queries spawned once each — no conway retry,
         // even though the stderr contains the words "latest" and "era".
         expect(mockSpawn).toHaveBeenCalledTimes(2);
       });

       it('does not retry the stake query on a non-era failure', async () => {
         mockSpawn.mockReturnValueOnce(
           createMockChildProcess('', 1, NODE_QUERY_FAILURE_STDERR)
         );

         await expect(service.fetchDRepStake()).rejects.toMatchObject({
           queryErrorType: GovernanceQueryErrorType.QueryFailed,
         });
         expect(mockSpawn).toHaveBeenCalledTimes(1);
       });
     });
   ```

3. **No existing test changes.** The registration positive era-fallback test
   (`'retries with conway when the installed CLI rejects the latest era alias'`,
   4 spawns) and the Step-3g stake retry test both use
   `LATEST_ALIAS_MISSING_STDERR`, which now classifies as `UsageError` → retry
   still fires. The `'rejects when the network flag was never set'` test stays
   `QueryFailed` (that rejection never passes through the close handler).

### 11e. PART B — CREATE `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`

Full file content:

```ts
import { spawnSync } from 'child_process';

/**
 * Parse-only smoke test against the real cardano-cli binary: proves the exact
 * argv the service builds clears the CLI argument parser. The mocked unit
 * suite can only confirm the argv matches the developer's belief about the
 * grammar; the prepend-vs-append network-flag regression class is only
 * closable against the real parser. No socket is provided, so a passing
 * invocation dies at the env-var/connection stage — never in the parser.
 */

const CLI_BIN = 'cardano-cli';

/** Mirrors GovernanceQueryService.CLI_USAGE_SIGNATURE. */
const USAGE_SIGNATURE = /(invalid (option|argument)|missing:|usage:)/i;

const isCliOnPath = (() => {
  try {
    return spawnSync(CLI_BIN, ['--version'], { timeout: 10_000 }).status === 0;
  } catch {
    return false;
  }
})();

// Self-skip (never fail) where the real binary is absent, so the mocked CI
// unit lane and plain devcontainers stay green; the positive run needs the
// Nix shell with the bundled cardano-cli on PATH.
const describeWithCli = isCliOnPath ? describe : describe.skip;

/**
 * The exact per-phase argv forms GovernanceQueryService builds: era token
 * first, subcommand args, network flag appended last.
 */
const SERVICE_ARGV_FORMS: string[][] = [];
(['latest', 'conway'] as const).forEach((era) => {
  [
    ['query', 'drep-state', '--all-dreps', '--output-json'],
    ['query', 'tip', '--output-json'],
    ['query', 'drep-stake-distribution', '--all-dreps', '--output-json'],
  ].forEach((args) => {
    SERVICE_ARGV_FORMS.push([era, ...args, '--mainnet']);
    SERVICE_ARGV_FORMS.push([era, ...args, '--testnet-magic', '1']);
  });
});

describeWithCli('cardano-cli argv smoke (parse-only, no socket)', () => {
  it.each(
    SERVICE_ARGV_FORMS.map((argv): [string, string[]] => [
      argv.join(' '),
      argv,
    ])
  )('clears the argument parser: %s', (_label, argv) => {
    const env: Record<string, string> = {};
    Object.keys(process.env).forEach((key) => {
      const value = process.env[key];
      if (typeof value === 'string' && key !== 'CARDANO_NODE_SOCKET_PATH') {
        env[key] = value;
      }
    });

    const result = spawnSync(CLI_BIN, argv, { env, timeout: 15_000 });
    const stderr = String(result.stderr ?? '');

    // Without a socket the query cannot succeed; it must fail at the
    // socket/connection stage. An optparse rejection (Invalid option /
    // "Missing: (--mainnet | --testnet-magic NATURAL)" / unknown era) is
    // the grammar regression this test exists to catch.
    expect(result.status).not.toBe(0);
    expect(stderr).not.toMatch(USAGE_SIGNATURE);
  });
});
```

Notes: the missing-socket failure prints an env-var/connection error ("Error while
looking up environment variable: CARDANO_NODE_SOCKET_PATH …" or a connect error) —
neither matches `USAGE_SIGNATURE`, while a real grammar break prints the optparse
`Missing:`/`Invalid option` signature and fails the assertion. 12 cases = 2 eras ×
3 queries × 2 network-flag forms. On a CLI old enough to reject the `latest` alias
the `latest` rows would fail — that CLI is below the bundled version and out of
scope (the service's conway fallback covers it at runtime).

### 11f. Verify and commit

1. `node_modules/.bin/tsc --noEmit` → zero errors.
2. `yarn lint` → clean.
3. `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts` → **38**
   passed (35 + 3).
4. `yarn test:jest tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` → in THIS
   environment: **12 skipped, 0 failed** (cardano-cli is not on PATH — the AC-6
   self-skip is itself what this run proves). Report it as skipped, never as
   passed.
5. `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts` → still **13**
   (renderer untouched).
6. `yarn prettier --write source/common/types/governance.types.ts source/main/governance/GovernanceQueryService.ts tests/jest/governance/GovernanceQueryService.spec.ts tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`
7. Commit (subject only):
   `feat(gov): task-169 harden cli era retry and add argv smoke test`

**Verification debt (record in Step 13 statusReason):** PART B's positive run
(cases actually executing against the real parser) requires the Nix shell; here the
suite proves only the skip mechanism.

---

## Step 12: Verification (run all, report honestly)

From the worktree root, after Step 11:

```bash
# 1. Typecheck — MUST exit 0 with zero errors (never `yarn compile` under Node v24)
node_modules/.bin/tsc --noEmit

# 2. Lint — MUST be clean on every file this phase touched
yarn lint

# 3. Governance Jest folder — expected:
#      GovernanceQueryService.spec.ts   38 passed
#      GovernanceStore.spec.ts          13 passed
#      logDRepStateSnapshot.spec.ts      4 passed
#      GovernanceCliArgvSmoke.spec.ts   12 SKIPPED, 0 failed (no cardano-cli here)
yarn test:jest tests/jest/governance/

# 4. Component + container suites — expected:
#      DRepDirectory.spec.tsx           19 passed
#      DRepDirectoryPage.spec.tsx        3 passed
#      VotingGovernancePage.spec.tsx     7 passed
yarn test:jest \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx

# 5. Sanitization floor — MUST report exactly 20 passed, 0 failed, never below 20
yarn test:jest tests/jest/security/governance-sanitization.spec.ts

# 6. i18n idempotence re-check (first run was Step 6c; expected: no diff, exit 0;
#    if it fails for environment reasons, record the debt — see Step 6c fallback)
yarn i18n:manage

# 7. Invariant greps — each MUST print nothing
grep -n "NetworkStatus" source/renderer/app/stores/GovernanceStore.ts
grep -n "CLI_TIMEOUT_MS" source/main/governance/GovernanceQueryService.ts
grep -rn "include-stake" source/main/governance/
grep -n "filterLogData" source/main/utils/setupLogging.ts

# 8. Snapshot filename registered exactly once — MUST print exactly one line
grep -n "DRep-state-snapshot.json" source/main/config.ts
```

**Per-task AC ↔ proof mapping (all must hold):**

| Task | Proof |
|---|---|
| task-159 | Step-1e grep empty (store boundary) + component/container suites green with the required sync props |
| task-160 | Component tests: banner render + floored `%` + null-progress + noSync fallback (empty and failure) + retained-list guard; container test: refetch-once reaction + disposal |
| task-161 | Service suite: stake describe (fixture parse, sentinel skip, dual shape, oversized lossless, ParseFailed, bulk argv, conway retry, per-phase dedup, guards) + Phase-1 `votingPower` null assertions |
| task-162 | Store tests: Phase-1 paint → Phase-2 merge, absent-id stays null, stake-failure → `isRankingUnavailable` with list retained, enrich-window dedup, `{ errorType }`-only logging; component test: `—` tooltips + banner |
| task-163 | Service suite: per-phase budget pins (10 s/30 s) + 30 s stake-timeout test; `CLI_TIMEOUT_MS` grep empty |
| task-164 | 5 keys in both locales (Step 6 greps) + ja-JP render test green + `yarn i18n:manage` result |
| task-165 | Design section present; `givenName` grep empty under governance components |
| task-166 | Plan mitigation names all three remaining manual items; nothing manual attempted |
| task-167 | Steps 9e counts: 19 / 13 / 3 / 7 + floor 20/20 |
| task-168 | Snapshot spec 4/4 (public data written, overwrite, no vote keys, `ALLOWED_LOGS` source check) + floor 20/20 |
| task-169 | Era-signal tests (spawn exactly twice negative, UsageError positive, stake non-retry) + smoke suite skipped-not-failed here |

**Honest-reporting rules (binding):** report the real outcome of every command —
never claim an unrun or failing check as passing. Two outcomes are *expected* to be
non-green-in-the-usual-sense in this environment and must be reported as exactly
what they are: the smoke suite reports **skipped** (verification debt: Nix-shell
positive run), and `yarn i18n:manage` may fail environmentally (debt recorded per
Step 6c). If anything else fails and cannot be fixed, stop and say so explicitly in
the return value — do not proceed to Step 13 commits for the affected task.

---

## Step 13: Tracker + docs + commit

The eleven per-task commits already exist (Steps 1-11, one subject-only commit per
task — NFR-8). This step is the phase close-out: tracker sync, PRD Final Outcome,
optional findings note, review-log entry, ONE final docs commit. All edits below
are hand-edits — never run prettier on JSON or Markdown.

### 13a. Tracker — `governance-drep-discovery-plan-tasks.json`

For each of the eleven ux-refinement task blocks set exactly four fields (field
names and shapes follow the task-114/115 precedent):

- **`status`** — `"complete"` for task-159 … task-165, task-167, task-168,
  task-169. **NEVER `"verified"`** (that requires dedicated proof beyond in-task
  tests, realistically task-125). **task-166 stays `"partial"`.**
- **`statusReason`** — truthful prose naming the gates run (tsc, eslint, exact
  Jest counts) and any debt. Required elements per task:
  - task-159: store boundary preserved (grep empty), props threaded through
    container/spec/story call sites.
  - task-160: banner + noSync fallback + refetch reaction landed; behavior tests
    landed in task-167 by design (PD-11).
  - task-161: phase split + new `GOVERNANCE_DREP_STAKE_CHANNEL`; synthetic stake
    mock rewritten to the canonical key shape; real-shape confirmation deferred to
    the task-166 manual follow-up (PD-6).
  - task-162: MUST contain this sentence verbatim: *"Reshuffle half of AC-3 is
    forward-compat only — Reshuffle/seed do not exist until slice-5 task-118; the
    two-phase path re-queries only via fetchDRepList and adds no seed coupling."*
  - task-163: per-call budgets threaded; 30 s provisional pending the task-166
    latency measurement; tokens §6 confirmed unchanged.
  - task-164: the honest `yarn i18n:manage` outcome (pass, or the environment
    failure + manual-grep fallback per Step 6c).
  - task-165: doc-only; design section added; plan row verified, not edited.
  - task-166: keep the existing PREPROD FIXTURE CAPTURED note, state that the plan
    mitigation now names all three remaining items, and that the remainder
    (mainnet fixture + p50/p95 latency + fixture promotion) is locked
    `manual_execution` and was NOT attempted — status stays partial.
  - task-167: the suite counts (19/13/3/7) + floor 20/20.
  - task-168: snapshot writer + `ALLOWED_LOGS` + tokens §12 + 4/4 spec; verification
    debt: end-to-end bundle proof deferred to task-125.
  - task-169: PART A structured signal + negative/positive tests; PART B suite
    self-skipped here (no cardano-cli) — positive run is Nix-shell verification
    debt.
- **`evidence`** — array of repo-relative touched-file paths per task:
  - task-159: the four Step-1 files.
  - task-160: the seven Step-2 files (incl. the two new `DRepEmptyState.*`).
  - task-161: the seven Step-3 files (incl. the rewritten stake mock).
  - task-162: the ten Step-4 files (incl. the two new `DRepErrorBanner.*`).
  - task-163: `source/main/governance/GovernanceQueryService.ts`,
    `tests/jest/governance/GovernanceQueryService.spec.ts`.
  - task-164: the two locale JSONs (+ any files `yarn i18n:manage` rewrote).
  - task-165: `.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md`.
  - task-166: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`.
  - task-167: the four Step-9 files.
  - task-168: the five Step-10 files.
  - task-169: the four Step-11 files.
- **`updatedAt`** — the actual execution date, `"YYYY-MM-DD"`.

Also bump the top-level `metadata.updated` to the same date. Change nothing else in
the JSON (no reordering, no reformatting, two-space indentation preserved).

### 13b. PRD Final Outcome

Fill the `## Final Outcome` section of `ux-refinement-PRD.md` (currently the
placeholder *"(to be filled at phase close)"*) with: what shipped per task (one
line each), the Step-12 verification results verbatim-honest (including the two
expected debts: smoke-suite skipped, i18n:manage outcome), the floor count
(20/20), and the statement that task-166 remains `partial` with its
`manual_execution` remainder untouched. If no durable research findings emerged,
add the line `No new research findings — research/ux-refinement-findings.md not
created.` Do not change the `Planning Status` header in this step — that field
belongs to the review-log flow.

### 13c. Findings note (conditional)

Create `.agent/plans/governance/drep-discovery/research/ux-refinement-findings.md`
ONLY if implementation produced durable, reusable findings (e.g. the real
`yarn i18n:manage` behavior under Node v24, a jsdom/mobx testing gotcha not already
in the slice-3 findings, the real optparse stderr shapes). Otherwise skip the file
and record "no new research" in the PRD Final Outcome (13b).

### 13d. Review log entry

Append to `task-plans/ux-refinement-code-review.md` (append-only, house style):

```md
## Implementer: <date> — ux-refinement close-out
```

summarizing: 11 tasks executed in guide order, per-task commit subjects, Step-12
results, the two recorded verification debts, and task-166 left `partial`.

### 13e. Final commit + end-state checks

1. Stage exactly: the tracker JSON, the PRD, the review log, and (if created) the
   findings note. Commit (subject only): `docs(gov): close out ux-refinement`
2. `git status` → clean.
3. `git log --oneline -13` → the eleven task commits + this close-out commit on
   `wt/ux-refinement`, in guide order (plus the base commit below them).
4. **Never push, never `gh`** — commits stay local to the worktree; branch stays
   `wt/ux-refinement` (the orchestrator owns moving the work back).
