# Slice-5 Implementation Guide: Default Cohort + Category Badges + BMVG Banner

> **Companion PRD:** [slice-5-PRD.md](./slice-5-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/workspaces/daedalus/.agent/worktrees/slice-5` (branch `feat/drep-discovery-slice-5`,
> base `b6b94268e`) on 2026-07-24. Re-verify an anchor only if the file was touched by
> an earlier step of this same guide.

---

## Implementation Order

```
task-118 (cohort + seed in GovernanceStore + banner cohort line + Reshuffle + Jest + i18n)
→ task-119 (DRepCategoryBadge + both call sites + snapshots + story + i18n)
→ task-120 (BMVG citation secondary line + banner spec + banner stories + i18n)
```

Dependencies force 118 before 120 (the JSON's `dependencies`); 119 sits between them by
JSON listing order (its own deps, task-107 and task-116, are already `complete`).

## Cross-Cutting Renderer Note (applies to every task)

- **react-intl is 2.9.0**: use `injectIntl` / `intlShape` / `defineMessages`. Never
  `useIntl()` or any react-intl hook.
- **Locked invariants stated inline per step.** The global ones for this slice:
  - **#2 sanitization floor** — never pass a DRep ID, `abstain`/`no_confidence`
    literal, or any CIP-129/CIP-105 bech32 string to `logger.*`, analytics, or an
    electron-store write. **Every new code path in this slice makes ZERO
    logger/analytics/storage calls** — including the seed value. The 23-test spy suite
    must stay green after every task.
  - **#5 lovelace losslessness** — cohort ranking compares `BigNumber` voting powers
    via `comparedTo`. Never call `.toNumber()`, never subtract BigNumbers into a
    `Number`, never use `Number(votingPower)` anywhere.
  - **#6 CLI discipline** — this slice contains **no** main-process, IPC, or CLI
    change. Do NOT touch `source/main/governance/GovernanceQueryService.ts` or
    `source/main/ipc/governanceChannel.ts`. `reshuffleCohort()` must not call any
    channel `request()`; a Jest case pins this.
  - **#7 default cohort binding** — exclude top 35 by voting power; up to the next 200
    eligible (`status === 'active'` AND `drepActivity > 6` — strictly greater, 6 is
    OUT); randomized. No fixture may place a sub-6-epoch DRep inside a cohort
    (sub-floor values appear in test fixtures only to assert exclusion). The cohort IS
    "Recommended": add no Recommended tab, badge, or copy.
  - **#8 badges informational** — `DRepCategoryBadge` (and `getDRepCategory`) must
    never be imported by `GovernanceStore` or any code that orders/filters entries.
  - **#11 preliminary copy** — every NEW en-US and ja-JP string starts with `!!!`.
    Never strip an existing `!!!`.
  - **#14 status grounding** — do not touch `DRepStatus`
    (`source/common/types/governance.types.ts:35`) or `DRepStatusBadge.tsx`. No
    `expiring` status value anywhere.
- **Code comments**: only where logic is not self-evident; 1–3 plain lines stating the
  why/invariant. No task IDs, no review labels, no ALL-CAPS tags, no change history.
- **Jest assertion style**: never `toHaveBeenCalledWith('str', { literal: 'object' })`
  (prettier 2.1.2 oscillates on it) — always `expect.objectContaining({ … })` for
  object arguments.
- **Verification commands** (run from the worktree root
  `/workspaces/daedalus/.agent/worktrees/slice-5`):
  - **`npx` DOES NOT WORK in this devcontainer** (slice-4 finding F-6): npm rejects the
    repo's string-form `devEngines` before any tool runs. Invoke every tool as
    `node_modules/.bin/<tool>` or `yarn <tool>`.
  - Typecheck: `node_modules/.bin/tsc --noEmit` — must exit 0 with ZERO errors
    (`yarn compile` is unreliable under Node 24 — do not use it).
  - Lint: `node_modules/.bin/eslint <touched paths> --ext .ts,.tsx`.
  - Focused Jest: `node_modules/.bin/jest <spec paths> --no-coverage --runInBand`.
  - Sanitization floor: `node_modules/.bin/jest
    tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand` →
    **23/23 at baseline and after every task; the suite file is never edited**.
  - Copy changes: `yarn i18n:manage` (works under Node 24 — ux-refinement F-7; it
    rewrites `source/renderer/app/i18n/defaultMessages.json` and
    `translations/messages.json` — those diffs ride with the task commit; never
    hand-edit or prettier those files or the locale JSONs).
  - Format: `node_modules/.bin/prettier --write` on the changed `.ts/.tsx/.scss` files
    ONLY (nix is unavailable — prettier substitutes `nix fmt`; never run it on JSON or
    `.snap` files). Collateral rewraps of pre-existing drift inside touched files are
    formatting-only — keep them (ux-refinement F-9).
- **Never commit `.scss.d.ts` files.** The global `declare module '*.scss'` in
  `source/renderer/declaration.d.ts` types the new `DRepCategoryBadge.scss`
  (ux-refinement F-2). Committing the `__snapshots__/*.snap` files task-119 generates
  is REQUIRED (they are the snapshot baselines).
- **Commits**: exactly one per task, subject-only Conventional Commits, no body, no
  trailers. Subjects are given at the end of each task. Never push, never open a PR.

---

## task-118: Default-cohort rule + randomization seed in GovernanceStore

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/utils/seededShuffle.ts` | CREATE |
| 2 | `source/renderer/app/utils/seededShuffle.spec.ts` | CREATE |
| 3 | `source/renderer/app/stores/GovernanceStore.ts` | EDIT |
| 4 | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` | EDIT |
| 5 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` | EDIT |
| 6 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx` | EDIT (replace file) |
| 7 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss` | EDIT (append) |
| 8 | `source/renderer/app/i18n/locales/en-US.json` | EDIT |
| 9 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT |
| 10 | `tests/jest/governance/GovernanceStore.spec.ts` | EDIT (append + 2 imports) |
| 11 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | EDIT |
| 12 | `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` | EDIT |
| 13 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | EDIT (3 mock fields only) |
| 14 | `storybook/stories/governance/DRepDirectory.stories.tsx` | EDIT |

Do **NOT** touch `DRepCard.tsx`, `DRepDetail.tsx` (task-119), the sanitization suite,
`DRepStatusBadge.tsx`, or anything under `source/main/` in this task.

### Step-by-Step

#### Step 1: Create `source/renderer/app/utils/seededShuffle.ts`

Full file contents:

```ts
/**
 * Deterministic seeded shuffle for the default DRep cohort. mulberry32 is a
 * tiny 32-bit PRNG: the same (items, seed) pair always yields the same
 * permutation, so cohort order is reproducible for the whole app session.
 */
function mulberry32(seed: number): () => number {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

export function generateCohortSeed(): number {
  return Math.floor(Math.random() * 4294967296) >>> 0;
}

export function seededShuffle<T>(items: T[], seed: number): T[] {
  const result = [...items];
  const random = mulberry32(seed);
  for (let i = result.length - 1; i > 0; i--) {
    const j = Math.floor(random() * (i + 1));
    [result[i], result[j]] = [result[j], result[i]];
  }
  return result;
}
```

Invariant #2: nothing here logs; the seed never reaches any sink.

#### Step 2: Create `source/renderer/app/utils/seededShuffle.spec.ts`

Co-located spec (repo precedent: `mithrilBehindness.spec.ts` in the same directory).
Full file contents:

```ts
import { generateCohortSeed, seededShuffle } from './seededShuffle';

describe('seededShuffle', () => {
  const items = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'];

  it('returns the same permutation for the same seed and input', () => {
    expect(seededShuffle(items, 42)).toEqual(seededShuffle(items, 42));
  });

  it('returns a different permutation for a different seed', () => {
    // Deterministic PRNG: if these two seeds ever collide on this input,
    // change the second seed rather than weakening the assertion.
    expect(seededShuffle(items, 1)).not.toEqual(seededShuffle(items, 2));
  });

  it('preserves membership and does not mutate its input', () => {
    const input = [...items];
    const shuffled = seededShuffle(input, 7);

    expect(input).toEqual(items);
    expect([...shuffled].sort()).toEqual([...items].sort());
  });

  it('handles empty and single-item arrays', () => {
    expect(seededShuffle([], 7)).toEqual([]);
    expect(seededShuffle(['only'], 7)).toEqual(['only']);
  });

  it('generates seeds inside the unsigned 32-bit range', () => {
    for (let i = 0; i < 100; i++) {
      const seed = generateCohortSeed();
      expect(Number.isInteger(seed)).toBe(true);
      expect(seed).toBeGreaterThanOrEqual(0);
      expect(seed).toBeLessThan(4294967296);
    }
  });
});
```

#### Step 3: Edit `source/renderer/app/stores/GovernanceStore.ts`

Current seams: imports :1-13, `AppDRepDirectoryEntry` :19-30, enums :32-45,
`GovernanceStoreError` :47-51, class opens :53, observables :57-74, computeds :78-104
(`isRankingUnavailable` :102-104), `fetchDRepList` :113-162, `_enrichVotingPower`
:169-195, `refresh()` :197-201.

**3a.** After the existing import block (line 13 ends the
`'../../../common/types/governance.types'` import), add:

```ts
import {
  generateCohortSeed,
  seededShuffle,
} from '../utils/seededShuffle';
```

**3b.** Insert between the `GovernanceStoreError` interface (ends line 51) and
`export default class GovernanceStore extends Store {` (line 53):

```ts
/**
 * Default-cohort rule (BMVG Simplified Phase-1 sizing): exclude the 35
 * largest DReps by voting power, then show up to the next 200 eligible
 * DReps - active with more than 6 remaining drepActivity epochs - in
 * seeded-random order.
 */
const COHORT_TOP_EXCLUSION = 35;
const COHORT_MAX_SIZE = 200;
const COHORT_MIN_REMAINING_EPOCHS = 6;

/**
 * Total, deterministic ranking: BigNumber voting power descending, null
 * powers last, drepId ascending as the tie-break. Never coerces lovelace
 * to Number.
 */
function compareByVotingPowerDesc(
  a: AppDRepDirectoryEntry,
  b: AppDRepDirectoryEntry
): number {
  if (a.votingPower && b.votingPower) {
    const cmp = b.votingPower.comparedTo(a.votingPower);
    if (cmp !== 0) return cmp;
  } else if (a.votingPower) {
    return -1;
  } else if (b.votingPower) {
    return 1;
  }
  if (a.drepId < b.drepId) return -1;
  if (a.drepId > b.drepId) return 1;
  return 0;
}
```

**3c.** After the `votingPowerState` observable (:72-74), add:

```ts
  /** Session randomization seed; replaced only by reshuffleCohort(). */
  @observable cohortSeed: number = generateCohortSeed();
```

**3d.** After the `isRankingUnavailable` computed (:102-104), add:

```ts
  /** The default cohort only exists once Phase-2 voting power has loaded. */
  @computed get isCohortActive(): boolean {
    return (
      this.votingPowerState === VotingPowerEnrichState.Loaded &&
      this.drepList.length > 0
    );
  }

  /**
   * Default cohort: rank by voting power, drop the top 35, keep up to the
   * next 200 eligible entries, then shuffle from the session seed. The
   * shuffle input is drepId-canonicalized so display order is a pure
   * function of (membership, seed) - stable across refreshes that change
   * voting powers without changing membership.
   */
  @computed get defaultCohort(): AppDRepDirectoryEntry[] | null {
    if (!this.isCohortActive) return null;
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    const eligible = ranked
      .slice(COHORT_TOP_EXCLUSION)
      .filter(
        (entry) =>
          entry.status === 'active' &&
          entry.drepActivity != null &&
          entry.drepActivity > COHORT_MIN_REMAINING_EPOCHS
      );
    const selected = eligible.slice(0, COHORT_MAX_SIZE);
    const canonical = [...selected].sort((a, b) => {
      if (a.drepId < b.drepId) return -1;
      if (a.drepId > b.drepId) return 1;
      return 0;
    });
    return seededShuffle(canonical, this.cohortSeed);
  }

  /** What the directory renders: the cohort when active, else the full list. */
  @computed get displayedDRepList(): AppDRepDirectoryEntry[] {
    return this.defaultCohort ?? this.drepList;
  }
```

**3e.** After `refresh()` (:197-201, `return this.fetchDRepList();` + closing brace),
add:

```ts
  /**
   * Replace the session seed to reorder the default cohort. Never triggers
   * a CLI query or IPC re-fetch - membership is recomputed from the
   * already-loaded list.
   */
  @action
  reshuffleCohort(): void {
    this.cohortSeed = generateCohortSeed();
  }
```

Nothing else in the store changes. `fetchDRepList` / `_enrichVotingPower` /
`_rehydrateDReps` / `_normalizeError` stay byte-identical — refresh preserves
`cohortSeed` simply because nothing writes it.

#### Step 4: Edit `source/renderer/app/containers/governance/DRepDirectoryPage.tsx`

Current seam — the `render()` return (:82-95):

```tsx
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        isNodeInSync={networkStatus.isNodeInSync}
        syncProgress={networkStatus.syncProgress}
        votingPowerState={governanceStore.votingPowerState}
        onRefresh={() => governanceStore.refresh()}
        onSelectForDelegation={this.handleSelectForDelegation}
        onViewDetails={this.handleViewDetails}
      />
```

Replace with:

```tsx
      <DRepDirectory
        drepList={governanceStore.displayedDRepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        isNodeInSync={networkStatus.isNodeInSync}
        syncProgress={networkStatus.syncProgress}
        votingPowerState={governanceStore.votingPowerState}
        isCohortActive={governanceStore.isCohortActive}
        onRefresh={() => governanceStore.refresh()}
        onReshuffle={() => governanceStore.reshuffleCohort()}
        onSelectForDelegation={this.handleSelectForDelegation}
        onViewDetails={this.handleViewDetails}
      />
```

No other change in this file (the mount/refresh/sync-reaction logic :24-56 and the two
handlers :58-73 stay byte-identical).

#### Step 5: Edit `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`

**5a.** In `interface Props` (:58-70), after `votingPowerState: VotingPowerEnrichState;`
(:65), add:

```ts
  isCohortActive: boolean;
```

and after `onRefresh: () => void;` (:66), add:

```ts
  onReshuffle: () => void;
```

**5b.** In the destructuring (:72-84), add `isCohortActive,` after
`votingPowerState,` and `onReshuffle,` after `onRefresh,`.

**5c.** The banner render (:184-188) currently reads:

```tsx
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
      />
```

Replace with:

```tsx
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
        isCohortActive={isCohortActive}
        onReshuffle={onReshuffle}
      />
```

Invariant #8 note: `DRepDirectory` renders whatever `drepList` it receives — it must
NOT sort or filter by category anywhere (no such code exists today; add none).

#### Step 6: Replace `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx`

Replace the entire 63-line file with (the existing `title`/`refresh`/`lastUpdated`
behavior is preserved exactly; new: two messages, two props, the cohort block):

```tsx
import React from 'react';
import moment from 'moment';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './DRepDirectoryBanner.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDirectory.title',
    defaultMessage: '!!!DRep Directory',
    description: 'Title banner for DRep directory',
  },
  refresh: {
    id: 'governance.drepDirectory.refresh',
    defaultMessage: '!!!Refresh',
    description: 'Refresh button label',
  },
  lastUpdated: {
    id: 'governance.drepDirectory.lastUpdated',
    defaultMessage: '!!!Last updated {time}',
    description: 'Last updated timestamp label',
  },
  cohortBanner: {
    id: 'governance.drepDirectory.cohortBanner',
    defaultMessage:
      '!!!Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power.',
    description: 'Primary line explaining the randomized default cohort',
  },
  reshuffle: {
    id: 'governance.drepDirectory.cohortBanner.reshuffle',
    defaultMessage: '!!!Reshuffle order',
    description: 'Control that reseeds the randomized cohort order',
  },
});

interface Props {
  lastFetchedAt: number | null;
  onRefresh: () => void;
  isRefreshing: boolean;
  isCohortActive: boolean;
  onReshuffle: () => void;
  intl: intlShape.isRequired;
}

function DRepDirectoryBanner({
  lastFetchedAt,
  onRefresh,
  isRefreshing,
  isCohortActive,
  onReshuffle,
  intl,
}: Props) {
  const timeAgo = lastFetchedAt ? moment(lastFetchedAt).fromNow() : null;

  return (
    <div className={styles.banner}>
      <div className={styles.headerRow}>
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <Button
          label={intl.formatMessage(messages.refresh)}
          onClick={onRefresh}
          disabled={isRefreshing}
          skin={ButtonSkin}
        />
      </div>
      {lastFetchedAt && timeAgo !== null && (
        <p className={styles.lastUpdated}>
          {intl.formatMessage(messages.lastUpdated, {
            time: timeAgo,
          })}
        </p>
      )}
      {isCohortActive && (
        <div className={styles.cohortLine}>
          <span>{intl.formatMessage(messages.cohortBanner)}</span>
          <Link
            className={styles.reshuffleLink}
            label={intl.formatMessage(messages.reshuffle)}
            hasIconAfter={false}
            onClick={onReshuffle}
            skin={LinkSkin}
          />
        </div>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryBanner);
```

Notes: the cohort block renders **only** when `isCohortActive` (R2 — no cohort claims
while Phase 2 is loading or failed). The banner has no dismiss control (tokens :85).
Explicit `LinkSkin` keeps specs ThemeProvider-free (slice-4 P-6 precedent).

#### Step 7: Append to `DRepDirectoryBanner.scss`

Current file is 27 lines (`.banner` / `.headerRow` / `.title` / `.lastUpdated`).
Append:

```scss
.cohortLine {
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 8px;
  font-size: 13px;
  color: var(--theme-text-secondary, #6b7384);
}

.reshuffleLink {
  font-size: 13px;
  white-space: nowrap;
}
```

`flex-wrap: wrap` lets the JA string reflow onto ≥2 lines instead of ellipsizing
(tokens :220). Do not commit any generated `.scss.d.ts`.

#### Step 8: Locale JSONs (task-118 keys; keep alphabetical key order)

`source/renderer/app/i18n/locales/en-US.json` — insert between
`"governance.drepDirectory.card.viewDetails"` (:306) and
`"governance.drepDirectory.copyButton"` (:307):

```json
  "governance.drepDirectory.cohortBanner": "!!!Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power.",
  "governance.drepDirectory.cohortBanner.reshuffle": "!!!Reshuffle order",
```

`source/renderer/app/i18n/locales/ja-JP.json` — same position (between :306 and :307):

```json
  "governance.drepDirectory.cohortBanner": "!!!デフォルト表示では、投票権が最大の35のDRepを除き、最大200の適格なDRepをランダムな順序で表示します。",
  "governance.drepDirectory.cohortBanner.reshuffle": "!!!順序をシャッフル",
```

Then run `yarn i18n:manage` and commit its `defaultMessages.json` /
`translations/messages.json` rewrites with this task. Invariant #11: both strings keep
`!!!`.

#### Step 9: Extend `tests/jest/governance/GovernanceStore.spec.ts`

**9a.** Add one import after the `bignumber.js` import (line 1):

```ts
import { runInAction } from 'mobx';
```

(Existing imports :2-14 stay unchanged; the file already mocks both channels :17-20
and the logger :24-31, and defines `flushAsync` :37.)

**9b.** Append a **sibling top-level describe** after the existing
`describe('GovernanceStore', …)` block closes (line 312):

```ts
describe('GovernanceStore default cohort', () => {
  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
  });

  const drepIdAt = (i: number) =>
    `drep1cohort${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  const buildDrep = (
    i: number,
    overrides: Partial<DRepDirectoryEntry> = {}
  ): DRepDirectoryEntry => ({
    anchor: null,
    drepActivity: 10,
    drepId: drepIdAt(i),
    status: 'active',
    votingPower: null,
    ...overrides,
  });

  // Stake descending with index: entry 0 is the largest, so ranks equal ids.
  const stakeFor = (count: number): Record<string, string> => {
    const map: Record<string, string> = {};
    for (let i = 0; i < count; i++) {
      map[drepIdAt(i)] = String(1_000_000_000_000 - i * 1_000_000);
    }
    return map;
  };

  const loadStore = async (
    dreps: DRepDirectoryEntry[],
    stakeByDRepId: Record<string, string>
  ): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps,
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  it('exposes no cohort until voting-power enrichment has loaded', async () => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockImplementation(() => new Promise(() => {}));

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    void store.fetchDRepList();
    await flushAsync();

    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loading);
    expect(store.isCohortActive).toBe(false);
    expect(store.defaultCohort).toBeNull();
    // Phase-1 full-list behavior is preserved while the enrich runs.
    expect(store.displayedDRepList).toBe(store.drepList);
  });

  it('keeps the full list displayed when the stake phase fails', async () => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(store.isRankingUnavailable).toBe(true);
    expect(store.isCohortActive).toBe(false);
    expect(store.defaultCohort).toBeNull();
    expect(store.displayedDRepList).toHaveLength(1);
  });

  it('excludes the 35 largest by voting power and keeps the rest', async () => {
    const dreps = Array.from({ length: 40 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(40));

    const cohort = store.defaultCohort!;
    expect(cohort).toHaveLength(5);
    const cohortIds = new Set(cohort.map((e) => e.drepId));
    for (let i = 0; i < 35; i++) {
      expect(cohortIds.has(drepIdAt(i))).toBe(false);
    }
    for (let i = 35; i < 40; i++) {
      expect(cohortIds.has(drepIdAt(i))).toBe(true);
    }
  });

  it('ranks the top-35 boundary with lossless BigNumber comparison', async () => {
    // The two boundary stakes differ by one lovelace beyond Number precision,
    // and the LARGER stake sits on the LARGER drepId: a float-coerced compare
    // would tie them, fall to the drepId tie-break, and invert which entry
    // lands in the top 35.
    const dreps = Array.from({ length: 37 }, (_, i) => buildDrep(i));
    const stake: Record<string, string> = {};
    for (let i = 0; i < 34; i++) {
      stake[drepIdAt(i)] = `90071992547410${String(10 + i)}`;
    }
    stake[drepIdAt(34)] = '9007199254740992';
    stake[drepIdAt(35)] = '9007199254740993';
    stake[drepIdAt(36)] = '1000000';
    const store = await loadStore(dreps, stake);

    const cohortIds = new Set(store.defaultCohort!.map((e) => e.drepId));
    expect(cohortIds.has(drepIdAt(35))).toBe(false);
    expect(cohortIds.has(drepIdAt(34))).toBe(true);
    expect(cohortIds.has(drepIdAt(36))).toBe(true);
  });

  it('applies the eligibility floor after the exclusion: active and more than 6 epochs', async () => {
    // Sub-floor and inactive entries appear here ONLY to prove exclusion;
    // no fixture may place them inside a cohort.
    const dreps = [
      ...Array.from({ length: 35 }, (_, i) => buildDrep(i)),
      buildDrep(35, { drepActivity: 7 }),
      buildDrep(36, { drepActivity: 6 }),
      buildDrep(37, { drepActivity: 0, status: 'inactive' }),
      buildDrep(38, { drepActivity: null }),
    ];
    const store = await loadStore(dreps, stakeFor(39));

    expect(store.defaultCohort!.map((e) => e.drepId)).toEqual([drepIdAt(35)]);
  });

  it('caps the cohort at the 200 highest-ranked eligible entries', async () => {
    const dreps = Array.from({ length: 245 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(245));

    const cohort = store.defaultCohort!;
    expect(cohort).toHaveLength(200);
    const cohortIds = new Set(cohort.map((e) => e.drepId));
    expect(cohortIds.has(drepIdAt(35))).toBe(true);
    expect(cohortIds.has(drepIdAt(234))).toBe(true);
    expect(cohortIds.has(drepIdAt(235))).toBe(false);
    expect(cohortIds.has(drepIdAt(244))).toBe(false);
  });

  it('derives a stable order from the session seed', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const storeA = await loadStore(dreps, stakeFor(45));
    runInAction(() => {
      storeA.cohortSeed = 7;
    });
    const first = storeA.defaultCohort!.map((e) => e.drepId);

    expect(storeA.defaultCohort!.map((e) => e.drepId)).toEqual(first);

    const storeB = await loadStore(dreps, stakeFor(45));
    runInAction(() => {
      storeB.cohortSeed = 7;
    });
    expect(storeB.defaultCohort!.map((e) => e.drepId)).toEqual(first);

    runInAction(() => {
      storeB.cohortSeed = 8;
    });
    // Deterministic PRNG: if seeds 7 and 8 ever collide on this membership,
    // pick a different second seed rather than weakening the assertion.
    expect(storeB.defaultCohort!.map((e) => e.drepId)).not.toEqual(first);
  });

  it('keeps the display order stable when voting powers change but membership does not', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const storeA = await loadStore(dreps, stakeFor(45));
    runInAction(() => {
      storeA.cohortSeed = 7;
    });
    const before = storeA.defaultCohort!.map((e) => e.drepId);

    // Same membership, different in-cohort ranking: swap two stakes below
    // the top-35 boundary.
    const jiggled = stakeFor(45);
    const tmp = jiggled[drepIdAt(40)];
    jiggled[drepIdAt(40)] = jiggled[drepIdAt(44)];
    jiggled[drepIdAt(44)] = tmp;
    const storeB = await loadStore(dreps, jiggled);
    runInAction(() => {
      storeB.cohortSeed = 7;
    });

    expect(storeB.defaultCohort!.map((e) => e.drepId)).toEqual(before);
  });

  it('reshuffles without any IPC query and preserves membership', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(45));
    const before = store.defaultCohort!.map((e) => e.drepId);
    const seedBefore = store.cohortSeed;

    store.reshuffleCohort();

    // Reshuffle must never re-query: both channel call counts are unchanged.
    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(mockStakeRequest).toHaveBeenCalledTimes(1);
    expect(store.cohortSeed).not.toBe(seedBefore);
    const after = store.defaultCohort!.map((e) => e.drepId);
    expect([...after].sort()).toEqual([...before].sort());
  });

  it('preserves the session seed across an explicit refresh', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(45));
    const seedBefore = store.cohortSeed;
    const before = store.defaultCohort!.map((e) => e.drepId);

    await store.refresh();

    expect(store.cohortSeed).toBe(seedBefore);
    expect(store.defaultCohort!.map((e) => e.drepId)).toEqual(before);
    expect(mockRequest).toHaveBeenCalledTimes(2);
  });

  it('keeps excluded DReps in drepList and drepIndex', async () => {
    const dreps = Array.from({ length: 40 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(40));

    expect(store.drepList).toHaveLength(40);
    expect(store.drepIndex.get(drepIdAt(0))).toBeDefined();
    expect(
      store.defaultCohort!.map((e) => e.drepId)
    ).not.toContain(drepIdAt(0));
  });
});
```

(Seed values `7`/`8`/`42` are arbitrary fixture constants; `cohortSeed` writes go
through `runInAction` because the store is decorated for the app's
`enforceActions: 'observed'` mode, `index.tsx:29-30`.)

#### Step 10: Extend `DRepDirectory.spec.tsx`

**10a.** In the `renderComponent` helper (:40-78): add to the destructured defaults
(after `isNodeInSync = true,` :43):

```ts
  isCohortActive = false,
  onReshuffle = jest.fn(),
```

add to the type block (:50-60):

```ts
  isCohortActive?: boolean;
  onReshuffle?: jest.Mock;
```

and pass both to the component (after `isNodeInSync={isNodeInSync}` :67):

```tsx
        isCohortActive={isCohortActive}
        onReshuffle={onReshuffle}
```

**10b.** Append these tests inside `describe('DRepDirectory', …)`:

```tsx
  it('renders the cohort banner line and Reshuffle control when the cohort is active', () => {
    renderComponent({ isCohortActive: true });

    expect(
      screen.getByText(
        '!!!Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power.'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Reshuffle order')).toBeInTheDocument();
  });

  it('invokes onReshuffle when the Reshuffle control is clicked', () => {
    const onReshuffle = jest.fn();
    renderComponent({ isCohortActive: true, onReshuffle });

    fireEvent.click(screen.getByText('!!!Reshuffle order'));

    expect(onReshuffle).toHaveBeenCalledTimes(1);
  });

  it('makes no cohort claim while ranking is unavailable', () => {
    renderComponent({
      drepList: [{ ...baseEntries[0], votingPower: null }],
      isCohortActive: false,
      votingPowerState: VotingPowerEnrichState.Failed,
    });

    expect(screen.queryByText(/Default view shows/)).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Reshuffle order')).not.toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.'
      )
    ).toBeInTheDocument();
  });

  it('renders the cohort banner in ja-JP', () => {
    renderComponent({ isCohortActive: true, locale: 'ja-JP' });

    expect(screen.getByText(/最大200の適格なDRep/)).toBeInTheDocument();
    expect(screen.getByText('!!!順序をシャッフル')).toBeInTheDocument();
  });
```

#### Step 11: Extend `DRepDirectoryPage.spec.tsx`

**11a.** `buildGovernanceStore` (:26-33) gains three fields (the container now reads
them). Replace the function with:

```ts
const buildGovernanceStore = () => ({
  displayedDRepList: [drepEntry],
  drepList: [drepEntry],
  error: null,
  isCohortActive: true,
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  reshuffleCohort: jest.fn(),
  votingPowerState: VotingPowerEnrichState.Loaded,
});
```

**11b.** Add `fireEvent` to the `@testing-library/react` import (:8), then append
inside the describe:

```tsx
  it('renders the displayed list and forwards Reshuffle to the store', () => {
    const { governance } = renderPage();

    fireEvent.click(screen.getByText('!!!Reshuffle order'));

    expect(governance.reshuffleCohort).toHaveBeenCalledTimes(1);
    expect(governance.refresh).not.toHaveBeenCalled();
  });
```

(`refresh` is not called on mount here because the mocked `refreshState` is `Loaded` —
seam `DRepDirectoryPage.tsx:32-37` — so the assertion pins "reshuffle triggers no
fetch" at the container boundary too.)

#### Step 12: Extend `VotingGovernancePage.spec.tsx` (mock fields only)

In `buildStores().governance` (:87-95), add three fields alphabetically:

```ts
    displayedDRepList: [drepEntry],
    isCohortActive: false,
    reshuffleCohort: jest.fn(),
```

(`isCohortActive: false` keeps the two-hop harness free of new banner text.) **No
assertion in this file changes** — the slice-2/3/4 pins stay byte-identical.

#### Step 13: Update `storybook/stories/governance/DRepDirectory.stories.tsx`

**13a.** `renderDirectory` (:136-154) gains a 5th parameter and two props. Replace the
function signature and component with:

```tsx
const renderDirectory = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE,
  isCohortActive = false
) => (
  <DRepDirectory
    drepList={entries}
    error={error}
    isCohortActive={isCohortActive}
    isNodeInSync={syncState.isNodeInSync}
    lastFetchedAt={Date.now() - 3 * 60 * 1000}
    onRefresh={action('onRefresh')}
    onReshuffle={action('onReshuffle')}
    onSelectForDelegation={action('onSelectForDelegation')}
    onViewDetails={action('onViewDetails')}
    refreshState={refreshState}
    syncProgress={syncState.syncProgress}
    votingPowerState={VotingPowerEnrichState.Loaded}
  />
);
```

**13b.** `renderCentered` (:156-165) forwards the new parameter:

```tsx
const renderCentered = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE,
  isCohortActive = false
) => (
  <div style={CENTERED_STYLE}>
    {renderDirectory(refreshState, entries, error, syncState, isCohortActive)}
  </div>
);
```

**13c.** Story call updates (cohort active only where a loaded list shows):
- `'Loaded'` (:316-318) → `renderCentered(GovernanceRefreshState.Loaded, baseEntries, null, DEFAULT_SYNC_STATE, true)`
- `'Refreshing'` (:324-330) → add `, true` after the `REFRESH_ERROR` argument (pass
  `DEFAULT_SYNC_STATE` explicitly as the 4th argument)
- `'Node syncing'` (:331-341) → add `true` as the 5th argument (keep the existing
  sync-state object as the 4th)
- `'Pagination — 30 entries'` (:369-371) → `renderCentered(GovernanceRefreshState.Loaded, paginatedEntries, null, DEFAULT_SYNC_STATE, true)`
- `'Empty'`, `'Error'`, `'Loading'`, `'Node syncing — empty fallback'` → unchanged
  (default `false`; no cohort claim without a cohort)
- `'Ranking unavailable'` inline JSX (:353-368) → add
  `isCohortActive={false}` and `onReshuffle={action('onReshuffle')}` to the
  `<DRepDirectory>` props (R2: full list + rankingUnavailable banner, no cohort claim)
- Connected flow (:241-315): the `renderDirectory(refreshState, entries, error)` call
  at :302 becomes
  `renderDirectory(refreshState, entries, error, DEFAULT_SYNC_STATE, refreshState === GovernanceRefreshState.Loaded || refreshState === GovernanceRefreshState.Refreshing)`
  — cohort-active for the loaded/refreshing knob states, inactive for
  empty/loading/error.

No local `IntlProvider` anywhere — the global toggle drives locale (comment at
:133-135 stays).

#### Step 14: Verify, format, commit

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/eslint source/renderer/app/utils/seededShuffle.ts \
  source/renderer/app/utils/seededShuffle.spec.ts \
  source/renderer/app/stores/GovernanceStore.ts \
  source/renderer/app/containers/governance/DRepDirectoryPage.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  storybook/stories/governance/DRepDirectory.stories.tsx --ext .ts,.tsx
node_modules/.bin/jest source/renderer/app/utils/seededShuffle.spec.ts \
  tests/jest/governance/GovernanceStore.spec.ts \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  --no-coverage --runInBand
node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts \
  --no-coverage --runInBand   # 23/23, file untouched
yarn i18n:manage              # after the locale edits; commit its rewrites
node_modules/.bin/prettier --write <the changed .ts/.tsx/.scss files above>
```

Commit (subject only):

```
feat(gov): task-118 implement default cohort rule and session reshuffle seed in GovernanceStore
```

### Acceptance (task-118)

- [ ] Cohort = rank by BigNumber desc → drop top 35 → filter `active && drepActivity > 6` → take ≤200 → drepId-canonicalize → seeded shuffle; Jest pins each stage
- [ ] Seed created once per session; `refresh()` preserves it; `reshuffleCohort()` replaces it with zero channel calls (Jest-pinned at store AND container level)
- [ ] Directory shows the full list while `votingPowerState` is Loading/Failed; cohort claims render only when active
- [ ] No fixture ships a sub-6-epoch DRep inside a cohort; boundary case 6 is excluded, 7 included
- [ ] `cohortBanner` + `cohortBanner.reshuffle` in both locales, `!!!`-prefixed; `yarn i18n:manage` run
- [ ] Floor suite 23/23; tsc zero errors; scoped eslint clean; one commit

---

## task-119: DRepCategoryBadge (Primary / Threshold / Non-metadata)

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx` | CREATE |
| 2 | `source/renderer/app/components/governance/_shared/DRepCategoryBadge.scss` | CREATE |
| 3 | `source/renderer/app/components/governance/_shared/DRepCategoryBadge.spec.tsx` | CREATE |
| 4 | `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` | EDIT |
| 5 | `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx` | EDIT |
| 6 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | EDIT (append 1 test) |
| 7 | `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` | EDIT (append 1 test) |
| 8 | `storybook/stories/governance/DRepCategoryBadge.stories.tsx` | CREATE |
| 9 | `source/renderer/app/i18n/locales/en-US.json` | EDIT |
| 10 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT |
| 11 | `__snapshots__/…` for files 6 and 7 | GENERATED — commit them |

Do **NOT** touch `GovernanceStore.ts`, `DRepStatusBadge.tsx`, `DRepDirectoryList.tsx`,
or `DRepDetailOnchainSection.tsx` (the status badge stays in the on-chain Status row,
`DRepDetailOnchainSection.tsx:98-100`; the category badge does NOT go there — PRD P-8).

### Step-by-Step

#### Step 1: Create `DRepCategoryBadge.tsx`

Pattern to copy: `_shared/DRepStatusBadge.tsx:25-42` (injectIntl fn component,
`classNames(styles.badge, styles[…])`, aria-label, dot + label). Full file contents:

```tsx
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepCategoryBadge.scss';

const messages = defineMessages({
  primary: {
    id: 'governance.drepDirectory.category.primary',
    defaultMessage: '!!!Primary',
    description: 'Category badge for DReps with anchor metadata',
  },
  primaryTooltip: {
    id: 'governance.drepDirectory.category.primary.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view with verified metadata.',
    description: 'Tooltip explaining the Primary category',
  },
  threshold: {
    id: 'governance.drepDirectory.category.threshold',
    defaultMessage: '!!!Threshold',
    description: 'Category badge for DReps in the 7-12 epoch expiry window',
  },
  thresholdTooltip: {
    id: 'governance.drepDirectory.category.threshold.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view but approaching expiry — review before delegating.',
    description: 'Tooltip explaining the Threshold category',
  },
  nonMetadata: {
    id: 'governance.drepDirectory.category.nonMetadata',
    defaultMessage: '!!!Non-metadata',
    description: 'Category badge for DReps without anchor metadata',
  },
  nonMetadataTooltip: {
    id: 'governance.drepDirectory.category.nonMetadata.tooltip',
    defaultMessage:
      '!!!Eligible for delegation but has no verified off-chain metadata yet.',
    description: 'Tooltip explaining the Non-metadata category',
  },
});

export type DRepCategory = 'primary' | 'threshold' | 'nonMetadata';

export type DRepCategorySource = Pick<
  AppDRepDirectoryEntry,
  'status' | 'drepActivity' | 'anchor'
>;

const THRESHOLD_WINDOW_MIN = 7;
const THRESHOLD_WINDOW_MAX = 12;

/**
 * Category rules with binding priority Threshold > Primary > Non-metadata:
 * the 7-12 remaining-epoch window wins outright; otherwise on-chain anchor
 * presence is the interim metadata-completeness proxy until the verified
 * anchor pipeline exists. Informational only - never used to order or
 * filter the cohort.
 */
export function getDRepCategory(entry: DRepCategorySource): DRepCategory {
  if (
    entry.drepActivity != null &&
    entry.drepActivity >= THRESHOLD_WINDOW_MIN &&
    entry.drepActivity <= THRESHOLD_WINDOW_MAX
  ) {
    return 'threshold';
  }
  return entry.anchor != null ? 'primary' : 'nonMetadata';
}

interface Props {
  entry: DRepCategorySource;
  intl: intlShape.isRequired;
}

function DRepCategoryBadge({ entry, intl }: Props) {
  const category = getDRepCategory(entry);
  const labelByCategory: Record<DRepCategory, string> = {
    primary: intl.formatMessage(messages.primary),
    threshold: intl.formatMessage(messages.threshold),
    nonMetadata: intl.formatMessage(messages.nonMetadata),
  };
  const tooltipByCategory: Record<DRepCategory, string> = {
    primary: intl.formatMessage(messages.primaryTooltip),
    threshold: intl.formatMessage(messages.thresholdTooltip),
    nonMetadata: intl.formatMessage(messages.nonMetadataTooltip),
  };

  return (
    <span
      className={classNames(styles.badge, styles[category])}
      title={tooltipByCategory[category]}
      aria-label={`${labelByCategory[category]}. ${tooltipByCategory[category]}`}
    >
      <span className={styles.dot} aria-hidden="true" />
      <span className={styles.label}>{labelByCategory[category]}</span>
    </span>
  );
}

export default injectIntl(DRepCategoryBadge);
```

Notes: the native `title` tooltip follows the `DRepCard.tsx:69-78` precedent (no
PopOver). `status` is part of `DRepCategorySource` for the anchor-1 extension but the
slice-5 rules do not read it (PRD P-6). Invariant #8: this module exports a pure
classifier + a presentational component only — no callbacks, no store imports beyond
the type.

#### Step 2: Create `DRepCategoryBadge.scss`

Mirror `DRepStatusBadge.scss` (same `.badge`/`.dot`/`.label` base; category color
variants on the existing badge token families). Full file contents:

```scss
.badge {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  font-size: 14px;
  line-height: 1;
  padding: 4px 8px;
  border-radius: 4px;
  font-weight: 500;
}

.dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  display: inline-block;
  flex-shrink: 0;
}

.label {
  white-space: nowrap;
}

/* Category colors — reuses existing Daedalus theme variables via CSS custom properties */
.primary {
  color: var(--badge-info-fg, #4a7bd4);
  background: var(--badge-info-bg, rgba(74, 123, 212, 0.1));

  .dot {
    background: var(--badge-info-fg, #4a7bd4);
  }
}

.threshold {
  color: var(--badge-warning-fg, #b26a00);
  background: var(--badge-warning-bg, rgba(230, 162, 60, 0.12));

  .dot {
    background: var(--badge-warning-fg, #b26a00);
  }
}

.nonMetadata {
  color: var(--badge-neutral-fg, #8e939e);
  background: var(--badge-neutral-bg, rgba(142, 147, 158, 0.1));

  .dot {
    background: var(--badge-neutral-fg, #8e939e);
  }
}
```

Do not commit a `.scss.d.ts` (the global module declaration types it). If the build
requires regenerated SCSS typedefs, run `yarn typedef:sass` and discard any generated
`.d.ts` under `_shared/`.

#### Step 3: Create `DRepCategoryBadge.spec.tsx`

Co-located (precedent: `drep-directory/DRepDirectory.spec.tsx`). Full file contents:

```tsx
import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import DRepCategoryBadge, { getDRepCategory } from './DRepCategoryBadge';
import type { DRepCategorySource } from './DRepCategoryBadge';

const baseEntry: DRepCategorySource = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  drepActivity: 20,
  status: 'active',
};

const renderBadge = (entry: DRepCategorySource, locale = 'en-US') => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepCategoryBadge entry={entry} />
    </IntlProvider>
  );
};

describe('getDRepCategory', () => {
  it('categorizes an entry with an anchor outside the threshold window as primary', () => {
    expect(getDRepCategory(baseEntry)).toBe('primary');
  });

  it('categorizes an anchor-less entry outside the threshold window as nonMetadata', () => {
    expect(getDRepCategory({ ...baseEntry, anchor: null })).toBe('nonMetadata');
  });

  it('applies threshold across the whole 7-12 epoch window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: 7 })).toBe(
      'threshold'
    );
    expect(getDRepCategory({ ...baseEntry, drepActivity: 12 })).toBe(
      'threshold'
    );
  });

  it('gives threshold priority over primary for a 7-12 epoch entry with metadata', () => {
    // The binding tie-break: metadata never demotes the expiry warning.
    expect(getDRepCategory({ ...baseEntry, drepActivity: 10 })).toBe(
      'threshold'
    );
  });

  it('gives threshold priority over nonMetadata inside the window', () => {
    expect(
      getDRepCategory({ ...baseEntry, anchor: null, drepActivity: 8 })
    ).toBe('threshold');
  });

  it('leaves 6 and 13 remaining epochs outside the threshold window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: 13 })).toBe('primary');
    expect(
      getDRepCategory({ ...baseEntry, anchor: null, drepActivity: 6 })
    ).toBe('nonMetadata');
  });

  it('treats null drepActivity as outside the threshold window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: null })).toBe(
      'primary'
    );
  });
});

describe('DRepCategoryBadge', () => {
  afterEach(cleanup);

  it('renders the primary label with its explanatory tooltip', () => {
    renderBadge(baseEntry);

    expect(screen.getByText('!!!Primary').closest('span[title]')).toHaveAttribute(
      'title',
      '!!!Inside the default Recommended view with verified metadata.'
    );
  });

  it('renders the threshold label with its tooltip', () => {
    renderBadge({ ...baseEntry, drepActivity: 9 });

    expect(
      screen.getByText('!!!Threshold').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Inside the default Recommended view but approaching expiry — review before delegating.'
    );
  });

  it('renders the nonMetadata label with its tooltip', () => {
    renderBadge({ ...baseEntry, anchor: null });

    expect(
      screen.getByText('!!!Non-metadata').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Eligible for delegation but has no verified off-chain metadata yet.'
    );
  });

  it('renders category labels in ja-JP', () => {
    renderBadge({ ...baseEntry, drepActivity: 9 }, 'ja-JP');

    expect(screen.getByText('!!!しきい値')).toBeInTheDocument();
  });
});
```

#### Step 4: Render on the card — `DRepCard.tsx`

**4a.** Add the import after the `DRepStatusBadge` import (line 6):

```tsx
import DRepCategoryBadge from '../_shared/DRepCategoryBadge';
```

**4b.** The topRow (:81-85) currently reads:

```tsx
      <div className={styles.topRow}>
        <DRepStatusBadge status={entry.status} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
```

Replace with:

```tsx
      <div className={styles.topRow}>
        <DRepStatusBadge status={entry.status} />
        <DRepCategoryBadge entry={entry} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
```

(`.topRow` is already `display: flex; gap: 12px` — `DRepCard.scss:17-21` — no SCSS
change needed. Exactly one category badge per card — tokens :26.)

#### Step 5: Render on the detail — `DRepDetail.tsx`

**5a.** Add the import after the `DRepIdDisplay` import (line 5):

```tsx
import DRepCategoryBadge from '../_shared/DRepCategoryBadge';
```

**5b.** The header (:101-103) currently reads:

```tsx
      <div className={styles.header}>
        <DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />
      </div>
```

Replace with:

```tsx
      <div className={styles.header}>
        <DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />
        <DRepCategoryBadge entry={entry} />
      </div>
```

(`.header` is `display: flex; align-items: center; gap: 12px` —
`DRepDetail.scss:20-24`. The badge goes in the header, NOT in
`DRepDetailOnchainSection` — the category is renderer-derived, and the On-chain
section's provenance must stay truthful. The status badge stays where slice-4 put it:
the on-chain Status row, `DRepDetailOnchainSection.tsx:98-100`.)

#### Step 6: Card call-site snapshot — `DRepDirectory.spec.tsx`

Append inside `describe('DRepDirectory', …)`:

```tsx
  it('renders exactly one category badge per card (snapshot)', () => {
    renderComponent();

    // baseEntries[0]: drepActivity 12, anchor null -> Threshold window edge.
    expect(
      screen.getAllByText(/^!!!(Primary|Threshold|Non-metadata)$/)
    ).toHaveLength(1);
    expect(
      screen.getByText('!!!Threshold').closest('span[title]')
    ).toMatchSnapshot();
  });
```

#### Step 7: Detail call-site snapshot — `DRepDetailPage.spec.tsx`

Append inside `describe('DRepDetailPage', …)` (:91):

```tsx
  it('renders the category badge in the detail header (snapshot)', () => {
    renderPage();

    // baseEntry: anchor present, drepActivity 34 -> Primary.
    expect(
      screen.getByText('!!!Primary').closest('span[title]')
    ).toMatchSnapshot();
  });
```

Run the two suites once to generate
`source/renderer/app/components/governance/drep-directory/__snapshots__/DRepDirectory.spec.tsx.snap`
and
`source/renderer/app/containers/governance/__snapshots__/DRepDetailPage.spec.tsx.snap`;
inspect each `.snap` (it must contain the badge span with its class, title, and label),
then **commit both files**. Never run prettier on `.snap` files.

#### Step 8: Create `DRepCategoryBadge.stories.tsx`

Full file contents (`storybook/stories/governance/DRepCategoryBadge.stories.tsx`):

```tsx
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepCategoryBadge from '../../../source/renderer/app/components/governance/_shared/DRepCategoryBadge';
import type { DRepCategorySource } from '../../../source/renderer/app/components/governance/_shared/DRepCategoryBadge';

const ROW_STYLE = {
  display: 'flex',
  flexWrap: 'wrap' as const,
  gap: 16,
  padding: 24,
};

const anchor = {
  hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
  url: 'https://governance-preview.example.org/dreps/1.json',
};

const primaryEntry: DRepCategorySource = {
  anchor,
  drepActivity: 20,
  status: 'active',
};

const thresholdEntry: DRepCategorySource = {
  anchor,
  drepActivity: 9,
  status: 'active',
};

const nonMetadataEntry: DRepCategorySource = {
  anchor: null,
  drepActivity: 20,
  status: 'active',
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
storiesOf('Governance / DRep Category Badge', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('All categories', () => (
    <div style={ROW_STYLE}>
      <DRepCategoryBadge entry={primaryEntry} />
      <DRepCategoryBadge entry={thresholdEntry} />
      <DRepCategoryBadge entry={nonMetadataEntry} />
    </div>
  ));
```

`flexWrap` satisfies the no-overflow acceptance in ja-JP (labels reflow, never clip).
No per-locale story variants.

#### Step 9: Locale JSONs (task-119 keys; keep alphabetical key order)

`en-US.json` — insert between `"governance.drepDirectory.card.viewDetails"` and the
task-118 `"governance.drepDirectory.cohortBanner"` line (category sorts before
cohortBanner):

```json
  "governance.drepDirectory.category.nonMetadata": "!!!Non-metadata",
  "governance.drepDirectory.category.nonMetadata.tooltip": "!!!Eligible for delegation but has no verified off-chain metadata yet.",
  "governance.drepDirectory.category.primary": "!!!Primary",
  "governance.drepDirectory.category.primary.tooltip": "!!!Inside the default Recommended view with verified metadata.",
  "governance.drepDirectory.category.threshold": "!!!Threshold",
  "governance.drepDirectory.category.threshold.tooltip": "!!!Inside the default Recommended view but approaching expiry — review before delegating.",
```

`ja-JP.json` — same position:

```json
  "governance.drepDirectory.category.nonMetadata": "!!!メタデータなし",
  "governance.drepDirectory.category.nonMetadata.tooltip": "!!!委任は可能ですが、検証済みのオフチェーンメタデータがまだありません。",
  "governance.drepDirectory.category.primary": "!!!プライマリー",
  "governance.drepDirectory.category.primary.tooltip": "!!!検証済みメタデータを持つ、デフォルトの推奨ビュー内のDRepです。",
  "governance.drepDirectory.category.threshold": "!!!しきい値",
  "governance.drepDirectory.category.threshold.tooltip": "!!!デフォルトの推奨ビュー内ですが、失効が近づいています。委任前にご確認ください。",
```

Run `yarn i18n:manage`; commit its rewrites with this task. Do NOT add any
`category.highValue` key (anchor-1 owns it — tokens :28).

#### Step 10: Verify, format, commit

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/eslint \
  source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx \
  source/renderer/app/components/governance/_shared/DRepCategoryBadge.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepCard.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetail.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  storybook/stories/governance/DRepCategoryBadge.stories.tsx --ext .ts,.tsx
node_modules/.bin/jest \
  source/renderer/app/components/governance/_shared/DRepCategoryBadge.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  --no-coverage --runInBand
node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts \
  --no-coverage --runInBand   # 23/23
yarn i18n:manage
node_modules/.bin/prettier --write <the changed .tsx/.scss files above>  # never .snap/.json
```

Commit (subject only):

```
feat(gov): task-119 add informational DRepCategoryBadge to cards and detail
```

### Acceptance (task-119)

- [ ] Primary / Threshold / Non-metadata render with §1a rules, labels, tooltip copy; High value absent everywhere
- [ ] Priority Threshold > Primary > Non-metadata enforced; the 7–12-with-metadata tie-break unit-tested; window edges 6/7/12/13 pinned
- [ ] Badge is presentational only: no callbacks, no import from `GovernanceStore` or any ordering code; cohort order byte-identical with and without badges (no cohort code touched)
- [ ] Rendered by `DRepCard` AND `DRepDetail`; committed snapshots at both call sites
- [ ] 6 keys per locale, `!!!`-prefixed, via `yarn i18n:manage`
- [ ] Story renders all three categories under the global toggle without overflow
- [ ] Floor suite 23/23; tsc zero errors; scoped eslint clean; one commit

---

## task-120: BMVG citation secondary line

**Files to edit/create:**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx` | EDIT |
| 2 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss` | EDIT (append) |
| 3 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx` | CREATE |
| 4 | `storybook/stories/governance/DRepDirectoryBanner.stories.tsx` | CREATE |
| 5 | `source/renderer/app/i18n/locales/en-US.json` | EDIT |
| 6 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT |

### Step-by-Step

#### Step 1: Edit `DRepDirectoryBanner.tsx` (as left by task-118 Step 6)

**1a.** In the `defineMessages` block, after the `reshuffle` message and before the
closing `});`, add:

```ts
  source: {
    id: 'governance.drepDirectory.cohortBanner.source',
    defaultMessage:
      '!!!Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis.',
    description: 'Secondary line crediting the BMVG cohort-sizing analysis',
  },
```

**1b.** In `interface Props`, after `onReshuffle: () => void;`, add:

```ts
  showSource?: boolean;
```

**1c.** In the destructuring, after `onReshuffle,`, add `showSource = true,`.

**1d.** Immediately after the closing `)}` of the `isCohortActive && (…cohortLine…)`
block, add:

```tsx
      {isCohortActive && showSource && (
        <p className={styles.sourceLine}>
          {intl.formatMessage(messages.source)}
        </p>
      )}
```

The citation renders whenever the cohort line renders (tokens :84 — "must never be
removed"); `showSource` exists ONLY so the acceptance-required "without" story variant
can exist. **No production call site may ever pass `showSource={false}`** —
`DRepDirectory.tsx` passes nothing and keeps the default `true`.

#### Step 2: Append to `DRepDirectoryBanner.scss`

```scss
.sourceLine {
  font-size: 12px;
  color: var(--theme-text-tertiary, #8e939e);
  margin: 0;
}
```

(The line wraps naturally at narrow widths — PRD P-12 records that the §5 optional
tooltip-collapse is not implemented.)

#### Step 3: Create `DRepDirectoryBanner.spec.tsx`

Full file contents (co-located, `DRepDirectory.spec.tsx` harness style):

```tsx
import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import DRepDirectoryBanner from './DRepDirectoryBanner';

const renderBanner = ({
  isCohortActive = true,
  locale = 'en-US',
  showSource,
}: {
  isCohortActive?: boolean;
  locale?: string;
  showSource?: boolean;
} = {}) => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepDirectoryBanner
        isCohortActive={isCohortActive}
        isRefreshing={false}
        lastFetchedAt={Date.now() - 60_000}
        onRefresh={jest.fn()}
        onReshuffle={jest.fn()}
        showSource={showSource}
      />
    </IntlProvider>
  );
};

describe('DRepDirectoryBanner', () => {
  afterEach(cleanup);

  it('renders the BMVG citation beneath the cohort line by default', () => {
    renderBanner();

    expect(screen.getByText(/Default view shows up to 200/)).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis.'
      )
    ).toBeInTheDocument();
  });

  it('hides the citation only via the story-only showSource flag', () => {
    renderBanner({ showSource: false });

    expect(screen.getByText(/Default view shows up to 200/)).toBeInTheDocument();
    expect(screen.queryByText(/Beyond MVG/)).not.toBeInTheDocument();
  });

  it('renders neither cohort line nor citation when the cohort is inactive', () => {
    renderBanner({ isCohortActive: false });

    expect(
      screen.queryByText(/Default view shows up to 200/)
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Beyond MVG/)).not.toBeInTheDocument();
  });

  it('renders the citation in ja-JP', () => {
    renderBanner({ locale: 'ja-JP' });

    expect(screen.getByText(/Beyond MVG/)).toBeInTheDocument();
    expect(screen.getByText(/ワンクリック委任分析/)).toBeInTheDocument();
  });
});
```

#### Step 4: Create `DRepDirectoryBanner.stories.tsx`

Full file contents (`storybook/stories/governance/DRepDirectoryBanner.stories.tsx`):

```tsx
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepDirectoryBanner from '../../../source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner';

const CENTERED_STYLE = {
  margin: '0 auto',
  maxWidth: 960,
  padding: 24,
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
const renderBanner = (props: {
  isCohortActive: boolean;
  showSource?: boolean;
}) => (
  <div style={CENTERED_STYLE}>
    <DRepDirectoryBanner
      isCohortActive={props.isCohortActive}
      isRefreshing={false}
      lastFetchedAt={Date.now() - 3 * 60 * 1000}
      onRefresh={action('onRefresh')}
      onReshuffle={action('onReshuffle')}
      showSource={props.showSource}
    />
  </div>
);

storiesOf('Governance / DRep Directory Banner', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('Cohort active — with BMVG citation (default)', () =>
    renderBanner({ isCohortActive: true })
  )
  .add('Cohort active — without citation slot', () =>
    renderBanner({ isCohortActive: true, showSource: false })
  )
  .add('Cohort inactive', () => renderBanner({ isCohortActive: false }));
```

#### Step 5: Locale JSONs (task-120 key; keep alphabetical key order)

`en-US.json` — insert directly after
`"governance.drepDirectory.cohortBanner.reshuffle"`:

```json
  "governance.drepDirectory.cohortBanner.source": "!!!Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis.",
```

`ja-JP.json` — same position:

```json
  "governance.drepDirectory.cohortBanner.source": "!!!コホートの規模はBeyond MVG(BMVG)Simplifiedワンクリック委任分析に基づいています。",
```

Run `yarn i18n:manage`; commit its rewrites with this task.

#### Step 6: Verify, format, commit

```bash
node_modules/.bin/tsc --noEmit
node_modules/.bin/eslint \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx \
  storybook/stories/governance/DRepDirectoryBanner.stories.tsx --ext .ts,.tsx
node_modules/.bin/jest \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  --no-coverage --runInBand
node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts \
  --no-coverage --runInBand   # 23/23
yarn i18n:manage
node_modules/.bin/prettier --write <the changed .tsx/.scss files above>
```

Commit (subject only):

```
feat(gov): task-120 add BMVG citation line to the cohort banner
```

### Acceptance (task-120)

- [ ] Citation renders as a secondary line beneath the primary cohort copy in en-US and ja-JP whenever the cohort line renders
- [ ] `cohortBanner.source` key in both locales, `!!!`-prefixed, via `yarn i18n:manage`
- [ ] Stories show the banner with and without the citation slot; default is with; production call site never passes `showSource`
- [ ] Floor suite 23/23; tsc zero errors; scoped eslint clean; one commit

---

## i18n Keys (whole slice — 9 per locale, all `!!!`-prefixed)

| Key | Task | en source (after `!!!`) |
|---|---|---|
| `governance.drepDirectory.cohortBanner` | 118 | Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power. |
| `governance.drepDirectory.cohortBanner.reshuffle` | 118 | Reshuffle order |
| `governance.drepDirectory.category.nonMetadata` | 119 | Non-metadata |
| `governance.drepDirectory.category.nonMetadata.tooltip` | 119 | Eligible for delegation but has no verified off-chain metadata yet. |
| `governance.drepDirectory.category.primary` | 119 | Primary |
| `governance.drepDirectory.category.primary.tooltip` | 119 | Inside the default Recommended view with verified metadata. |
| `governance.drepDirectory.category.threshold` | 119 | Threshold |
| `governance.drepDirectory.category.threshold.tooltip` | 119 | Inside the default Recommended view but approaching expiry — review before delegating. |
| `governance.drepDirectory.cohortBanner.source` | 120 | Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis. |

NOT added this slice: `cohortBanner.showAll` (slice-6 task-121),
`category.highValue` + tooltip (anchor-1), any status key (R4).

---

## Cross-Cutting Acceptance (All Tasks)

- [ ] `node_modules/.bin/tsc --noEmit` exits 0 after every task
- [ ] Sanitization floor suite 23/23 after every task; the suite file byte-identical
- [ ] Zero new `logger.*` / analytics / electron-store calls anywhere in the slice diff
      (grep the diff for `logger.`, `analytics`, `electron-store` — only pre-existing
      hits in `GovernanceStore.ts` :149/:188 may appear in context lines)
- [ ] Zero changes under `source/main/`; `git diff --stat` for each commit confirms
- [ ] `GovernanceStore.fetchDRepList` / `_enrichVotingPower` byte-identical (cohort is
      additive)
- [ ] No `.toNumber()`/`Number(` on any voting-power value in the diff
- [ ] Every new string `!!!`-prefixed in BOTH locales; no existing `!!!` removed
- [ ] One subject-only commit per task; never push

---

## References

- PRD: [slice-5-PRD.md](./slice-5-PRD.md) (R1–R8, P-1…P-14, invariants table)
- Precedent guide: [slice-4-implementation-guide.md](./slice-4-implementation-guide.md)
  (devcontainer/tooling conventions carried forward)
- Design contracts: [shared-design-tokens.md](../designs/shared-design-tokens.md)
  §1a :24-39, §5 :80-87, §9 :168-194/:220;
  [drep-discovery-design.md](../designs/drep-discovery-design.md) :217-224
- Research: [ux-refinement-findings.md](../research/ux-refinement-findings.md) (F-1,
  F-6, F-7, F-9), [slice-4-findings.md](../research/slice-4-findings.md) (F-6, F-7)
