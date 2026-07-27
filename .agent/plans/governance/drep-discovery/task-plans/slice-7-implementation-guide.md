# Slice-7 Implementation Guide: DRep Favorites (task-122)

> **Companion PRD:** [slice-7-PRD.md](./slice-7-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/workspaces/daedalus/.agent/worktrees/slice-7-task-122` (branch
> `wt/slice-7-task-122`, base `73f983a3a`) on 2026-07-27. Re-verify an anchor only
> if the file was touched by an earlier step of this same guide.

---

## Implementation Order

```
task-122 (electron-store key + LocalStorageApi + GovernanceStore favorites
          + card toggle + list/filters threading + favorites view + banner mode
          + empty state + route/tab + container wiring + i18n + Jest + stories)
```

Single task. Dependencies task-106 and task-121 are both `complete`.

## Cross-Cutting Renderer Note (applies to every step)

- **react-intl is 2.9.0**: use `injectIntl` / `intlShape` / `defineMessages` /
  `FormattedMessage`. Never `useIntl()` or any react-intl hook.
- **Locked invariants stated inline (do not skip):**
  - **#2 sanitization floor** — never pass a DRep ID, `abstain`/`no_confidence`
    literal, or any CIP-129/CIP-105 bech32 string to `logger.*` or analytics.
    The favorites electron-store record (`DREP-FAVORITES`, raw drepId strings) is
    a **documented exception** sanctioned by invariant #12, like `TOKEN-FAVORITES`
    and the task-168 snapshot. Every new code path in this slice makes ZERO
    logger/analytics calls — favorites load/persist failures are swallowed
    silently on purpose (logging them would carry ids). **No analytics event
    fires on favorite toggle** (deliberate divergence from `AssetsStore`). The
    23-test spy suite `tests/jest/security/governance-sanitization.spec.ts` must
    stay 23/23 and is NEVER edited.
  - **#4 store boundary** — favorites live in `GovernanceStore`. Do not touch
    `VotingStore`; the delegation handoff stays `location.state`-only.
  - **#7 default cohort binding** — do not modify `defaultCohort`,
    `displayedDRepList`, `top35DRepIds`, `showAllList`, or `filterDReps`
    semantics. The favorites view is a NEW derived pipeline; the Favorited
    checkbox only drives the existing `favoritedOnly` facet.
  - **#8 badges informational** — never import badge modules into filtering
    code. The new `isStaleFavorite` helper reads `entry.status` only and drives
    an inline caption, never ordering or filtering.
  - **#11 preliminary copy** — every NEW en-US and ja-JP string starts with
    `!!!`. Never strip an existing `!!!`.
  - **#12 favorites per-device** — persistence goes only through
    `LocalStorageApi` → electron-store. Never write favorites into wallet data,
    IPC payloads of other channels, or anything synced.
  - **#14 status grounding** — do not touch `DRepStatus`
    (`source/common/types/governance.types.ts:35`), `DRepStatusBadge.tsx`, or
    `DRepCategoryBadge.tsx`. No `retired` status value ships; stale favorites
    are a forward-compat mechanism (PRD R-3).
- **AC-2 guard**: the favorited predicate exists exactly once —
  `helpers.ts:215`. If you find yourself writing
  `favoriteDRepIds.has(...)` inside any list-filtering logic outside
  `filterDReps`, stop: call `filterDReps` instead. (`DRepCard`'s
  `favoriteDRepIds.has(entry.drepId)` for the star state in Step 9 is display
  state, not filtering — that is allowed.)
- **Code comments**: only where logic is not self-evident; 1–3 plain lines
  stating the why/invariant. No task IDs, no review labels, no ALL-CAPS tags,
  no change history.
- **Jest assertion style**: never `toHaveBeenCalledWith('str', { literal: 'obj' })`
  (prettier 2.1.2 oscillates) — use `expect.objectContaining({ … })` for object
  arguments. A single array argument like `toHaveBeenCalledWith(['id'])` is safe.
- **Verification commands** (run from the worktree root
  `/workspaces/daedalus/.agent/worktrees/slice-7-task-122`):
  - **`npx` DOES NOT WORK in this devcontainer.** Invoke every tool as
    `node_modules/.bin/<tool>` or `yarn <tool>`.
  - Typecheck: `node_modules/.bin/tsc --noEmit` — must exit 0 with ZERO errors
    (`yarn compile` is unreliable under Node 24 — do not use it).
  - Lint: `node_modules/.bin/eslint <touched paths> --ext .ts,.tsx`.
  - Focused Jest: `node_modules/.bin/jest <spec paths> --no-coverage --runInBand`.
  - Sanitization floor: `node_modules/.bin/jest
    tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand`
    → **23/23 at baseline and after the task; the suite file is never edited**.
  - Copy changes: `yarn i18n:manage` (it rewrites
    `source/renderer/app/i18n/defaultMessages.json` and
    `translations/messages.json` — those diffs ride with the task commit; never
    hand-edit or prettier those files or the locale JSONs).
  - Format: `node_modules/.bin/prettier --write` on the changed `.ts/.tsx/.scss`
    files ONLY (never JSON, locale files, or `.snap`).
- **Never commit `.scss.d.ts` files.** The global `declare module '*.scss'`
  types SCSS modules.
- **Commit**: exactly one, subject-only Conventional Commits, no body, no
  trailers. Suggested: `feat(gov): task-122 persist DRep favorites via Electron
  local store`. Never push, never open a PR.

---

## task-122: Persist DRep favorites via Electron local store

**Files to edit (no new files except none — all edits):**

| # | File | Action |
|---|---|---|
| 1 | `source/common/types/electron-store.types.ts` | EDIT (union member) |
| 2 | `source/common/config/electron-store.config.ts` | EDIT (key) |
| 3 | `source/renderer/app/api/utils/localStorage.ts` | EDIT (3 methods) |
| 4 | `source/renderer/app/stores/GovernanceStore.ts` | EDIT (observable + 2 actions + setup) |
| 5 | `source/renderer/app/components/governance/drep-directory/helpers.ts` | EDIT (append helper) |
| 6 | `source/renderer/app/components/governance/drep-directory/helpers.spec.ts` | EDIT (append describe) |
| 7 | `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` | EDIT |
| 8 | `source/renderer/app/components/governance/drep-directory/DRepCard.scss` | EDIT (append) |
| 9 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx` | EDIT |
| 10 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryFilters.tsx` | EDIT |
| 11 | `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx` | EDIT |
| 12 | `source/renderer/app/components/governance/_shared/DRepEmptyState.scss` | EDIT (append) |
| 13 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx` | EDIT |
| 14 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.scss` | EDIT (append) |
| 15 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` | EDIT |
| 16 | `source/renderer/app/routes-config.ts` | EDIT |
| 17 | `source/renderer/app/Routes.tsx` | EDIT |
| 18 | `source/renderer/app/containers/voting/Governance.tsx` | EDIT |
| 19 | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx` | EDIT |
| 20 | `source/renderer/app/i18n/locales/en-US.json` | EDIT (9 keys) |
| 21 | `source/renderer/app/i18n/locales/ja-JP.json` | EDIT (9 keys) |
| 22 | `tests/jest/governance/GovernanceStore.spec.ts` | EDIT (append describe) |
| 23 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | EDIT |
| 24 | `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx` | EDIT (append) |
| 25 | `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` | EDIT |
| 26 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | EDIT (2 mock fields only) |
| 27 | `storybook/stories/governance/DRepDirectory.stories.tsx` | EDIT |

Do **NOT** touch: anything under `source/main/`, the sanitization suite,
`VotingStore.ts`, `governance.types.ts`, `DRepStatusBadge.tsx`,
`DRepCategoryBadge.tsx`, `DRepIdDisplay.tsx`, `DRepSourceLabel.tsx`,
`DRepDetail*`, `DRepErrorBanner.tsx`, `DRepDirectorySearch.tsx`,
`seededShuffle.ts`, `AssetsStore.ts`, `DRepDirectoryBanner.stories.tsx` (its new
props are optional), `maskAnalyticsRoute.ts`, or any locale key that already
exists (including `governance.tabs.directory` — recorded drift, do not rename).

### Step-by-Step

#### Step 1: `source/common/types/electron-store.types.ts` — register the key type

In the `StorageKey` union (:2-31), insert a new member after
`| 'DOWNLOAD-MANAGER'` (line 12):

```ts
  | 'DOWNLOAD-MANAGER'
  | 'DREP-FAVORITES'
```

#### Step 2: `source/common/config/electron-store.config.ts` — register the key

In `STORAGE_KEYS` (:9-38), insert after the `DOWNLOAD_MANAGER` line (line 17):

```ts
  DOWNLOAD_MANAGER: 'DOWNLOAD-MANAGER',
  DREP_FAVORITES: 'DREP-FAVORITES',
```

No main-process change is needed: `requestElectronStore`
(`source/main/ipc/electronStoreConversation.ts:31-52`) is generic over
`StorageKey` and prefixes every key with `environment.network`, so the record is
per-device and per-network by construction.

#### Step 3: `source/renderer/app/api/utils/localStorage.ts` — API methods

Directly after the `unsetWalletTokenFavorites` method (:294-295, quoted below)

```ts
  unsetWalletTokenFavorites = async (): Promise<void> =>
    LocalStorageApi.unset(keys.TOKEN_FAVORITES);
```

insert:

```ts
  getDRepFavorites = (): Promise<string[]> =>
    LocalStorageApi.get(keys.DREP_FAVORITES, []);
  setDRepFavorites = (favorites: string[]): Promise<void> =>
    LocalStorageApi.set(keys.DREP_FAVORITES, favorites);
  unsetDRepFavorites = (): Promise<void> =>
    LocalStorageApi.unset(keys.DREP_FAVORITES);
```

#### Step 4: `source/renderer/app/stores/GovernanceStore.ts` — favorites state

**4a.** After the `cohortSeed` observable (:119-120, quoted)

```ts
  /** Session randomization seed; replaced only by reshuffleCohort(). */
  @observable cohortSeed: number = generateCohortSeed();
```

add:

```ts
  /**
   * Favorited DRep ids from the per-device Electron local store. Always
   * replaced with a fresh Set instance on change - never mutated in place -
   * so computeds, React dep arrays and observers see a new reference.
   */
  @observable favoriteDRepIds: Set<string> = new Set();
```

**4b.** After the `reshuffleCohort()` action (:315-318, ends with
`this.cohortSeed = generateCohortSeed();` and a closing brace), add two actions:

```ts
  /**
   * Loads persisted favorites. A failed or malformed read keeps the empty
   * set silently: favorites are non-critical per-device state, and logging
   * here is forbidden because the payload holds DRep ids.
   */
  @action
  async loadFavorites(): Promise<void> {
    try {
      const stored = await this.api.localStorage.getDRepFavorites();
      const ids = Array.isArray(stored)
        ? stored.filter((id): id is string => typeof id === 'string')
        : [];
      runInAction(() => {
        this.favoriteDRepIds = new Set(ids);
      });
    } catch (_error) {
      // Intentionally silent - see the method comment.
    }
  }

  /**
   * Toggles one favorite and persists the whole set. A persistence failure
   * keeps the in-memory state; the next successful write stores everything.
   * Never logged - the payload holds DRep ids.
   */
  @action
  toggleFavorite(drepId: string): void {
    const next = new Set(this.favoriteDRepIds);
    if (next.has(drepId)) {
      next.delete(drepId);
    } else {
      next.add(drepId);
    }
    this.favoriteDRepIds = next;
    this.api.localStorage.setDRepFavorites([...next]).catch(() => {
      // Intentionally silent - see the method comment.
    });
  }
```

**4c.** Replace the `setup()` body (:322-324, quoted)

```ts
  setup(): void {
    super.setup();
  }
```

with:

```ts
  setup(): void {
    super.setup();
    this.loadFavorites();
  }
```

(`loadFavorites` catches internally, so the un-awaited call cannot produce an
unhandled rejection.)

#### Step 5: `helpers.ts` — stale-favorite helper

Append at the end of
`source/renderer/app/components/governance/drep-directory/helpers.ts` (after
`sortDReps` closes):

```ts
/**
 * A favorited entry is stale once its status leaves the default-cohort
 * universe. Only the deferred retired status qualifies and no live entry
 * carries it yet; doNotList joins this check when anchor metadata lands.
 */
const STALE_FAVORITE_STATUSES: ReadonlySet<string> = new Set(['retired']);

export function isStaleFavorite(entry: AppDRepDirectoryEntry): boolean {
  return STALE_FAVORITE_STATUSES.has(entry.status);
}
```

(`ReadonlySet<string>.has` accepts the `'active' | 'inactive'` union without
casts; no `DRepStatus` change — invariant #14.)

#### Step 6: `helpers.spec.ts` — cover the helper

In the import block from `'./helpers'` (:10-21), add `isStaleFavorite,` to the
imported names, placed between `isDefaultFilterState,` and
`normalizeDRepQuery,` (alphabetical). Then append at the end of the file:

```ts
describe('isStaleFavorite', () => {
  it('is false for every current on-chain status', () => {
    expect(isStaleFavorite(buildEntry(1))).toBe(false);
    expect(
      isStaleFavorite(buildEntry(2, { drepActivity: 0, status: 'inactive' }))
    ).toBe(false);
  });
});
```

#### Step 7: `DRepCard.tsx` — favorite toggle + stale caption

**7a.** In the `messages` block (:14-40), add three entries before the closing
`});`:

```ts
  favoriteAdd: {
    id: 'governance.drepDirectory.card.favorite.add',
    defaultMessage: '!!!Add to favorites',
    description: 'Accessible label of the favorite toggle when not favorited',
  },
  favoriteRemove: {
    id: 'governance.drepDirectory.card.favorite.remove',
    defaultMessage: '!!!Remove from favorites',
    description: 'Accessible label of the favorite toggle when favorited',
  },
  staleCaption: {
    id: 'governance.drepFavorites.staleCaption',
    defaultMessage: '!!!This DRep is no longer in the default cohort.',
    description: 'Inline caption under a stale favorited DRep',
  },
```

**7b.** Replace the `Props` interface (:42-48, quoted)

```ts
interface Props {
  entry: AppDRepDirectoryEntry;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}
```

with:

```ts
interface Props {
  entry: AppDRepDirectoryEntry;
  isFavorite: boolean;
  onToggleFavorite: (drepId: string) => void;
  isStaleFavorite?: boolean;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}
```

**7c.** Update the component destructuring (:63-69) to add `isFavorite,`
`onToggleFavorite,` and `isStaleFavorite = false,` after `entry,`.

**7d.** Replace the `topRow` block (:83-87, quoted)

```tsx
      <div className={styles.topRow}>
        <DRepStatusBadge status={entry.status} />
        <DRepCategoryBadge entry={entry} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
```

with (toggle FIRST — tokens §10 tab order: favorite → view details → select):

```tsx
      <div className={styles.topRow}>
        <button
          type="button"
          className={styles.favoriteToggle}
          aria-pressed={isFavorite}
          aria-label={intl.formatMessage(
            isFavorite ? messages.favoriteRemove : messages.favoriteAdd
          )}
          title={intl.formatMessage(
            isFavorite ? messages.favoriteRemove : messages.favoriteAdd
          )}
          onClick={() => onToggleFavorite(entry.drepId)}
        >
          <span aria-hidden="true">{isFavorite ? '★' : '☆'}</span>
        </button>
        <DRepStatusBadge status={entry.status} />
        <DRepCategoryBadge entry={entry} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
      {isStaleFavorite && (
        <p className={styles.staleCaption}>
          {intl.formatMessage(messages.staleCaption)}
        </p>
      )}
```

#### Step 8: Append to `DRepCard.scss`

```scss
.favoriteToggle {
  background: transparent;
  border: none;
  cursor: pointer;
  font-size: 16px;
  line-height: 1;
  padding: 2px 4px;
  color: var(--theme-text-secondary, #6b7384);

  &[aria-pressed='true'] {
    color: var(--theme-warning-color, #f2a218);
  }
}

.staleCaption {
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
  margin: 4px 0 0;
}
```

#### Step 9: `DRepDirectoryList.tsx` — thread favorites

**9a.** Add to the imports (after the `DRepCard` import, :5):

```ts
import { isStaleFavorite } from './helpers';
```

**9b.** Replace the `Props` interface (:30-36, quoted)

```ts
interface Props {
  entries: AppDRepDirectoryEntry[];
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}
```

with:

```ts
interface Props {
  entries: AppDRepDirectoryEntry[];
  favoriteDRepIds: ReadonlySet<string>;
  onToggleFavorite: (drepId: string) => void;
  isFavoritesView?: boolean;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}
```

**9c.** Update the component destructuring (:38-44) to add `favoriteDRepIds,`
`onToggleFavorite,` `isFavoritesView = false,` and
`isStaleFavoriteEntry = isStaleFavorite,` after `entries,`.

**9d.** Replace the `DRepCard` render (:74-82, quoted)

```tsx
          <DRepCard
            key={entry.drepId}
            entry={entry}
            onSelectForDelegation={onSelectForDelegation}
            onViewDetails={onViewDetails}
            votingPowerState={votingPowerState}
          />
```

with:

```tsx
          <DRepCard
            key={entry.drepId}
            entry={entry}
            isFavorite={favoriteDRepIds.has(entry.drepId)}
            onToggleFavorite={onToggleFavorite}
            isStaleFavorite={isFavoritesView && isStaleFavoriteEntry(entry)}
            onSelectForDelegation={onSelectForDelegation}
            onViewDetails={onViewDetails}
            votingPowerState={votingPowerState}
          />
```

(The stale caption renders only in the favorites view — AC-5 scopes it to the
Favorites page. The star state is display state, not filtering; AC-2 holds.)

#### Step 10: `DRepDirectoryFilters.tsx` — Favorited checkbox

**10a.** In the `messages` block, after the `excludeTop35` entry (:65-69), add:

```ts
  favorited: {
    id: 'governance.drepDirectory.filter.favorited',
    defaultMessage: '!!!Favorited',
    description: 'Filter that shows only favorited DReps',
  },
```

**10b.** Immediately after the expiry `</select>` closes and BEFORE the
`{isShowAll && isRankingAvailable && (` conditional (the excludeTop35 Checkbox,
:192-202), insert an always-visible Checkbox:

```tsx
      <Checkbox
        className={styles.toggle}
        label={intl.formatMessage(messages.favorited)}
        checked={filters.favoritedOnly}
        onChange={(checked: boolean) =>
          onFiltersChange({ ...filters, favoritedOnly: checked })
        }
        skin={CheckboxSkin}
      />
```

No `filterDReps`/`helpers.ts` change: the `favoritedOnly` facet and predicate
already exist (slice-6 D-3). This is the control slice-6 deferred to task-122.

#### Step 11: `DRepEmptyState.tsx` — `noFavorites` variant

**11a.** In the `messages` block (:12-35), add before the closing `});`:

```ts
  noFavoritesTitle: {
    id: 'governance.drepFavorites.empty.title',
    defaultMessage: '!!!No favorites yet',
    description: 'Title of the empty favorites view',
  },
  noFavoritesBody: {
    id: 'governance.drepFavorites.empty.body',
    defaultMessage:
      '!!!DReps you favorite from the directory appear here. Favorites are stored on this device only.',
    description: 'Body of the empty favorites view; owns the per-device expectation',
  },
  backToDirectory: {
    id: 'governance.drepDirectory.backToDirectory',
    defaultMessage: '!!!Back to directory',
    description: 'CTA from the empty favorites view back to the directory',
  },
```

**11b.** Replace the variant union + comment (:37-39, quoted)

```ts
// Only noSync and noResults ship for now; the designed selfnode variant
// joins this union when its owning slice lands.
export type DRepEmptyStateVariant = 'noSync' | 'noResults';
```

with:

```ts
// noSync, noResults and noFavorites ship for now; the designed selfnode
// variant joins this union when its owning slice lands.
export type DRepEmptyStateVariant = 'noSync' | 'noResults' | 'noFavorites';
```

**11c.** Add `onBackToDirectory?: () => void;` to `Props` (after `onShowAll`)
and to the component destructuring.

**11d.** After the `if (variant === 'noResults') { … }` block closes (:79) and
before the final `return`, insert:

```tsx
  if (variant === 'noFavorites') {
    return (
      <div className={styles.container} data-variant={variant}>
        <p className={styles.title}>
          {intl.formatMessage(messages.noFavoritesTitle)}
        </p>
        <p className={styles.message}>
          {intl.formatMessage(messages.noFavoritesBody)}
        </p>
        <Link
          className={styles.actionLink}
          label={intl.formatMessage(messages.backToDirectory)}
          hasIconAfter={false}
          onClick={onBackToDirectory}
          skin={LinkSkin}
        />
      </div>
    );
  }
```

(The designed "prominent illustration" has no asset in the repo — text + CTA
only, per PRD R-2b.)

#### Step 12: Append to `DRepEmptyState.scss`

```scss
.title {
  font-size: 16px;
  font-weight: 600;
  margin: 0 0 4px;
}
```

#### Step 13: `DRepDirectoryBanner.tsx` — favorites mode

**13a.** In the `messages` block, after the `filtered` entry (:43-48), add.
NOTE on the apostrophe: the design copy uses the plain ASCII `'` in "you've"
(verified against `drep-discovery-design.md:109`); a lone ICU apostrophe not
followed by `{` or `}` renders literally, so no escaping is needed — but the JS
string must be double-quoted (prettier's `singleQuote: true` keeps double
quotes for strings containing `'`):

```ts
  favorites: {
    id: 'governance.drepFavorites.banner',
    defaultMessage:
      "!!!{n} DReps you've favorited. Favorites are stored on this device only.",
    description: 'Banner line of the favorites view, replacing the cohort claim',
  },
```

**13b.** Add to `Props` (:51-64), after `displayedCount?: number;`:

```ts
  // Favorites-view mode; both default off so existing call sites and the
  // banner stories keep compiling unchanged.
  isFavoritesView?: boolean;
  favoritesCount?: number;
```

and to the destructuring (:66-76): `isFavoritesView = false,`
`favoritesCount = 0,`.

**13c.** Guard the three existing conditional lines with `!isFavoritesView`:

- `:97` `{isCohortActive && !isFilteredView && (` →
  `{isCohortActive && !isFilteredView && !isFavoritesView && (`
- `:109` `{isCohortActive && !isFilteredView && showSource && (` →
  `{isCohortActive && !isFilteredView && !isFavoritesView && showSource && (`
- `:114` `{isFilteredView && (` → `{isFilteredView && !isFavoritesView && (`

**13d.** After the filtered-line block closes (the `)}` at :118), before the
closing `</div>`, add:

```tsx
      {isFavoritesView && (
        <p className={styles.favoritesLine}>
          {intl.formatMessage(messages.favorites, { n: favoritesCount })}
        </p>
      )}
```

#### Step 14: Append to `DRepDirectoryBanner.scss`

Mirror the existing `.filteredLine` rule (:48-52). Append:

```scss
.favoritesLine {
  font-size: 13px;
  color: var(--theme-text-secondary, #6b7384);
  margin: 0;
}
```

(Declarations copied from the live `.filteredLine` so the two replaced-banner
lines render identically.)

#### Step 15: `DRepDirectory.tsx` — favorites view mode

**15a.** `Props` (:79-97): after `favoriteDRepIds?: ReadonlySet<string>;` (:84),
add:

```ts
  view?: 'directory' | 'favorites';
  onToggleFavorite: (drepId: string) => void;
  onBackToDirectory?: () => void;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
```

**15b.** Destructuring (:99-117): after `favoriteDRepIds = EMPTY_DREP_ID_SET,`
(:104), add:

```ts
  view = 'directory',
  onToggleFavorite,
  onBackToDirectory,
  isStaleFavoriteEntry,
```

**15c.** After the `isRankingAvailable` const (:130), add:

```ts
  const isFavoritesView = view === 'favorites';
```

**15d.** After the `visibleEntries` memo closes (:166), add the favorites
pipeline:

```ts
  // The favorites view reuses the slice-6 favorited predicate over the full
  // membership, so favorited entries outside the default cohort stay visible.
  const favoritesEntries = useMemo(
    () =>
      filterDReps(
        showAllList,
        { ...DEFAULT_DREP_FILTER_STATE, favoritedOnly: true },
        { favoriteDRepIds, top35DRepIds }
      ),
    [favoriteDRepIds, showAllList, top35DRepIds]
  );
```

**15e.** Gate the exact-match-open effect off in the favorites view. In the
effect (:171-179), replace its first line

```ts
    if (queryKind !== 'exactValid') return;
```

with:

```ts
    if (isFavoritesView || queryKind !== 'exactValid') return;
```

and add `isFavoritesView` to the effect's dependency array (:179 —
`[queryKind, searchQuery, drepIndex, onViewDetails]` becomes
`[isFavoritesView, queryKind, searchQuery, drepIndex, onViewDetails]`).

**15f.** In `renderContent()`, replace the `default:` case opening (:260-262,
quoted)

```tsx
      default:
        return (
          <>
```

with:

```tsx
      default:
        if (isFavoritesView) {
          return favoritesEntries.length === 0 ? (
            <DRepEmptyState
              variant="noFavorites"
              onBackToDirectory={onBackToDirectory}
            />
          ) : (
            <DRepDirectoryList
              entries={favoritesEntries}
              favoriteDRepIds={favoriteDRepIds}
              onToggleFavorite={onToggleFavorite}
              isFavoritesView
              isStaleFavoriteEntry={isStaleFavoriteEntry}
              onSelectForDelegation={onSelectForDelegation}
              onViewDetails={onViewDetails}
              votingPowerState={votingPowerState}
            />
          );
        }
        return (
          <>
```

(The loading / noSync / failed / bare-empty cases above the `default:` remain
shared between both views on purpose.)

**15g.** In the directory branch's `DRepDirectoryList` render (:319-324), add
the two new required props:

```tsx
              <DRepDirectoryList
                entries={visibleEntries}
                favoriteDRepIds={favoriteDRepIds}
                onToggleFavorite={onToggleFavorite}
                onSelectForDelegation={onSelectForDelegation}
                onViewDetails={onViewDetails}
                votingPowerState={votingPowerState}
              />
```

**15h.** In the `DRepDirectoryBanner` render (:333-341), add:

```tsx
        isFavoritesView={isFavoritesView}
        favoritesCount={favoritesEntries.length}
```

(after `displayedCount={visibleEntries.length}`).

#### Step 16: `routes-config.ts` — favorites route

Replace the `GOVERNANCE` block (:39-43, quoted)

```ts
  GOVERNANCE: {
    ROOT: '/governance',
    DREPS: '/governance/dreps',
    DREP_DETAIL: '/governance/dreps/:drepId',
  },
```

with:

```ts
  GOVERNANCE: {
    ROOT: '/governance',
    DREPS: '/governance/dreps',
    DREP_DETAIL: '/governance/dreps/:drepId',
    FAVORITES: '/governance/favorites',
  },
```

#### Step 17: `Routes.tsx` — register the route

After the DREPS `TrackedRoute` (:234-239, quoted)

```tsx
            <TrackedRoute
              exact
              pageTitle="DRep Directory"
              path={ROUTES.GOVERNANCE.DREPS}
              component={DRepDirectoryPage}
            />
```

insert:

```tsx
            <TrackedRoute
              exact
              pageTitle="DRep Favorites"
              path={ROUTES.GOVERNANCE.FAVORITES}
              component={DRepDirectoryPage}
            />
```

(`/governance/favorites` never collides with `/governance/dreps/:drepId`; the
tracked page title carries no DRep id.)

#### Step 18: `containers/voting/Governance.tsx` — Favorites tab

**18a.** In the `messages` block (:11-17), add after `tabDirectory`:

```ts
  tabFavorites: {
    id: 'governance.drepDirectory.tabs.favorites',
    defaultMessage: '!!!Favorites',
    description: 'Label for the DRep favorites tab.',
  },
```

(The exact §9 id is used here; the shipped `governance.tabs.directory` id on the
sibling item is pre-existing drift and is NOT renamed — PRD R-2a.)

**18b.** Replace the `navItems` array (:42-47, quoted)

```ts
    const navItems: Array<NavButtonProps> = [
      {
        id: ROUTES.GOVERNANCE.DREPS,
        label: intl.formatMessage(messages.tabDirectory),
      },
    ];
```

with:

```ts
    const navItems: Array<NavButtonProps> = [
      {
        id: ROUTES.GOVERNANCE.DREPS,
        label: intl.formatMessage(messages.tabDirectory),
      },
      {
        id: ROUTES.GOVERNANCE.FAVORITES,
        label: intl.formatMessage(messages.tabFavorites),
      },
    ];
```

(`activeItem` matching (:48-51) is prefix-based and unambiguous:
`/governance/favorites` never starts with `/governance/dreps` and vice versa.
`handleNavItemClick` (:30-37) already pushes the route.)

#### Step 19: `DRepDirectoryPage.tsx` — container wiring

**19a.** After `handleViewDetails` (:68-73), add:

```ts
  handleBackToDirectory = () => {
    // Keep the inherited delegation-form return state across the tab switch.
    this.props.history.push(
      ROUTES.GOVERNANCE.DREPS,
      pickDelegationFormReturnState(this.props.location.state)
    );
  };
```

**19b.** In `render()`, replace the `<DRepDirectory` opening props block
(:83-88, quoted)

```tsx
      <DRepDirectory
        drepList={governanceStore.displayedDRepList}
        drepIndex={governanceStore.drepIndex}
        showAllList={governanceStore.showAllList}
        top35DRepIds={governanceStore.top35DRepIds}
```

with:

```tsx
      <DRepDirectory
        drepList={governanceStore.displayedDRepList}
        drepIndex={governanceStore.drepIndex}
        showAllList={governanceStore.showAllList}
        top35DRepIds={governanceStore.top35DRepIds}
        favoriteDRepIds={governanceStore.favoriteDRepIds}
        onToggleFavorite={(drepId) => governanceStore.toggleFavorite(drepId)}
        view={
          this.props.location.pathname.startsWith(ROUTES.GOVERNANCE.FAVORITES)
            ? 'favorites'
            : 'directory'
        }
        onBackToDirectory={this.handleBackToDirectory}
```

(The rest of the props block :88-99 stays unchanged.)

#### Step 20: `source/renderer/app/i18n/locales/en-US.json` — 9 keys

Insert each line in the file's existing alphabetical (case-sensitive) key order.
All values keep the leading `!!!`. Exact lines:

```json
"governance.drepDirectory.backToDirectory": "!!!Back to directory",
"governance.drepDirectory.card.favorite.add": "!!!Add to favorites",
"governance.drepDirectory.card.favorite.remove": "!!!Remove from favorites",
"governance.drepDirectory.filter.favorited": "!!!Favorited",
"governance.drepDirectory.tabs.favorites": "!!!Favorites",
"governance.drepFavorites.banner": "!!!{n} DReps you've favorited. Favorites are stored on this device only.",
"governance.drepFavorites.empty.body": "!!!DReps you favorite from the directory appear here. Favorites are stored on this device only.",
"governance.drepFavorites.empty.title": "!!!No favorites yet",
"governance.drepFavorites.staleCaption": "!!!This DRep is no longer in the default cohort.",
```

Placement guide: `backToDirectory` before `card.select` (:305);
`card.favorite.*` before `card.select`; `filter.favorited` between
`filter.expiry.thresholdWindow` (:330) and `filter.metadata` (:331);
`tabs.favorites` between `syncing` (:354) and `title` (:355); the four
`drepFavorites.*` keys after the last `governance.drepDirectory.*` key
(`title`, :355) and before `governance.tabs.directory` (:359 — `drepFavorites`
sorts after `drepDirectory` and before `tabs`).

#### Step 21: `source/renderer/app/i18n/locales/ja-JP.json` — 9 keys

Same placement rules; preliminary `!!!` placeholders (release-end review owns
final JA copy). Exact lines:

```json
"governance.drepDirectory.backToDirectory": "!!!ディレクトリに戻る",
"governance.drepDirectory.card.favorite.add": "!!!お気に入りに追加",
"governance.drepDirectory.card.favorite.remove": "!!!お気に入りから削除",
"governance.drepDirectory.filter.favorited": "!!!お気に入り",
"governance.drepDirectory.tabs.favorites": "!!!お気に入り",
"governance.drepFavorites.banner": "!!!お気に入りに追加したDRep：{n}件。お気に入りはこのデバイスにのみ保存されます。",
"governance.drepFavorites.empty.body": "!!!ディレクトリでお気に入りに追加したDRepがここに表示されます。お気に入りはこのデバイスにのみ保存されます。",
"governance.drepFavorites.empty.title": "!!!お気に入りはまだありません",
"governance.drepFavorites.staleCaption": "!!!このDRepは現在デフォルトコホートの対象外です。",
```

After Steps 20-21, run `yarn i18n:manage` and let the regenerated
`defaultMessages.json` / `translations/messages.json` diffs ride with the
commit. Never prettier these JSON files.

#### Step 22: `tests/jest/governance/GovernanceStore.spec.ts` — favorites suite

The file already mocks the governance channels and the renderer `logger`
(:17-32) and defines `flushAsync` (:38). Append at the end of the top-level
`describe('GovernanceStore', …)` block (inside it, after the last existing
test) — or as a sibling `describe` at file end; either compiles, sibling is
simpler:

```ts
describe('GovernanceStore favorites', () => {
  const FAVORITE_ID =
    'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0001';
  const OTHER_ID =
    'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0002';

  // One backing record shared by any number of store instances simulates the
  // per-device electron-store surviving an app restart.
  const buildBackedApi = (initial: unknown = []) => {
    const backing = { record: initial };
    const localStorage = {
      getDRepFavorites: jest.fn(async () => backing.record),
      setDRepFavorites: jest.fn(async (ids: string[]) => {
        backing.record = ids;
      }),
    };
    return { api: { localStorage }, backing, localStorage };
  };

  const buildStore = (api: unknown) =>
    new GovernanceStore(api as any, {} as any, {} as any);

  afterEach(() => {
    (logger.debug as jest.Mock).mockClear();
    (logger.info as jest.Mock).mockClear();
    (logger.warn as jest.Mock).mockClear();
    (logger.error as jest.Mock).mockClear();
  });

  it('loads persisted favorites into the observable set on setup', async () => {
    const { api } = buildBackedApi([FAVORITE_ID]);
    const store = buildStore(api);

    store.setup();
    await flushAsync();

    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(true);
    expect(store.favoriteDRepIds.size).toBe(1);
  });

  it('toggling adds then removes and persists the full array each time', async () => {
    const { api, localStorage } = buildBackedApi();
    const store = buildStore(api);
    store.setup();
    await flushAsync();

    store.toggleFavorite(FAVORITE_ID);
    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(true);
    await flushAsync();
    expect(localStorage.setDRepFavorites).toHaveBeenCalledWith([FAVORITE_ID]);

    store.toggleFavorite(FAVORITE_ID);
    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(false);
    await flushAsync();
    expect(localStorage.setDRepFavorites).toHaveBeenLastCalledWith([]);
  });

  it('replaces the set instance on toggle so observers see a new reference', async () => {
    const { api } = buildBackedApi();
    const store = buildStore(api);
    store.setup();
    await flushAsync();

    const before = store.favoriteDRepIds;
    store.toggleFavorite(FAVORITE_ID);

    expect(store.favoriteDRepIds).not.toBe(before);
  });

  it('restores favorites in a fresh store from the same backing record (app restart)', async () => {
    const { api } = buildBackedApi();
    const first = buildStore(api);
    first.setup();
    await flushAsync();
    first.toggleFavorite(FAVORITE_ID);
    first.toggleFavorite(OTHER_ID);
    await flushAsync();

    const second = buildStore(api);
    second.setup();
    await flushAsync();

    expect([...second.favoriteDRepIds].sort()).toEqual(
      [FAVORITE_ID, OTHER_ID].sort()
    );
  });

  it('degrades malformed records to the valid string subset', async () => {
    const { api } = buildBackedApi([FAVORITE_ID, 42, null, { a: 1 }]);
    const store = buildStore(api);

    store.setup();
    await flushAsync();

    expect([...store.favoriteDRepIds]).toEqual([FAVORITE_ID]);
  });

  it('keeps an empty set when the read rejects, without logging', async () => {
    const api = {
      localStorage: {
        getDRepFavorites: jest.fn(async () => {
          throw new Error(`read failed for ${FAVORITE_ID}`);
        }),
        setDRepFavorites: jest.fn(),
      },
    };
    const store = buildStore(api);

    store.setup();
    await flushAsync();

    expect(store.favoriteDRepIds.size).toBe(0);
    expect(logger.error).not.toHaveBeenCalled();
    expect(logger.warn).not.toHaveBeenCalled();
  });

  it('keeps in-memory state when persistence fails and never logs the payload', async () => {
    const api = {
      localStorage: {
        getDRepFavorites: jest.fn(async () => []),
        setDRepFavorites: jest.fn(async () => {
          throw new Error(`write failed for ${FAVORITE_ID}`);
        }),
      },
    };
    const store = buildStore(api);
    store.setup();
    await flushAsync();

    store.toggleFavorite(FAVORITE_ID);
    await flushAsync();

    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(true);
    const allLoggerCalls = JSON.stringify([
      (logger.debug as jest.Mock).mock.calls,
      (logger.info as jest.Mock).mock.calls,
      (logger.warn as jest.Mock).mock.calls,
      (logger.error as jest.Mock).mock.calls,
    ]);
    expect(allLoggerCalls).not.toContain(FAVORITE_ID);
  });
});
```

#### Step 23: `DRepDirectory.spec.tsx` — component coverage

**23a.** Extend `renderComponent` (:85-144). Add to the destructured defaults
(after `top35DRepIds = new Set<string>(),`):

```ts
  favoriteDRepIds = new Set<string>(),
  onToggleFavorite = jest.fn(),
  view = 'directory' as const,
  onBackToDirectory = jest.fn(),
  isStaleFavoriteEntry = undefined as
    | ((entry: AppDRepDirectoryEntry) => boolean)
    | undefined,
```

add matching optional fields to the parameter type annotation, and add to the
`<DRepDirectory` JSX (:124-140), after `top35DRepIds={top35DRepIds}`:

```tsx
          favoriteDRepIds={favoriteDRepIds}
          onToggleFavorite={onToggleFavorite}
          view={view}
          onBackToDirectory={onBackToDirectory}
          isStaleFavoriteEntry={isStaleFavoriteEntry}
```

Also return the new mocks from `renderComponent`'s return value if the harness
returns handles (follow the file's existing pattern for `onViewDetails`).

**23b.** Append a `describe('favorites', …)` block with these tests (build on
the harness helpers `realEntry`/`realDrepId` already in the file):

```ts
describe('favorites', () => {
  // Mirrors DRepIdDisplay's first8…last6 truncation; the exact truncated
  // string matches only the visible <code>, never the hidden tooltip copy.
  const truncatedDrepId = (n: number): string => {
    const id = realDrepId(n);
    return `${id.slice(0, 8)}…${id.slice(-6)}`;
  };

  it('renders the favorite toggle unpressed and fires onToggleFavorite with the row id', () => {
    const onToggleFavorite = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onToggleFavorite });

    const toggle = screen.getByRole('button', { name: /Add to favorites/ });
    expect(toggle).toHaveAttribute('aria-pressed', 'false');
    fireEvent.click(toggle);
    expect(onToggleFavorite).toHaveBeenCalledTimes(1);
    expect(onToggleFavorite).toHaveBeenCalledWith(realDrepId(1));
  });

  it('shows the pressed state and remove label for favorited rows', () => {
    renderComponent({
      drepList: [realEntry(1)],
      favoriteDRepIds: new Set([realDrepId(1)]),
    });

    const toggle = screen.getByRole('button', { name: /Remove from favorites/ });
    expect(toggle).toHaveAttribute('aria-pressed', 'true');
  });

  it('drives the favoritedOnly facet from the Favorited checkbox via the framework predicate', () => {
    renderComponent({
      drepList: [realEntry(1), realEntry(2)],
      favoriteDRepIds: new Set([realDrepId(2)]),
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
    fireEvent.click(screen.getByText(/Favorited/));

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
    expect(screen.queryByText(truncatedDrepId(1))).not.toBeInTheDocument();
  });

  it('renders favorited entries outside the cohort in the favorites view', () => {
    // Entry 2 is favorited but absent from the cohort list; the favorites
    // view draws from the full membership, so it must still render.
    renderComponent({
      drepList: [realEntry(1)],
      showAllList: [realEntry(1), realEntry(2)],
      favoriteDRepIds: new Set([realDrepId(2)]),
      view: 'favorites',
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
    expect(screen.queryByText(truncatedDrepId(1))).not.toBeInTheDocument();
    expect(screen.getByText(/DReps you've favorited/)).toBeInTheDocument();
  });

  it('hides search and filter controls in the favorites view', () => {
    renderComponent({
      drepList: [realEntry(1)],
      favoriteDRepIds: new Set([realDrepId(1)]),
      view: 'favorites',
    });

    expect(
      screen.queryByPlaceholderText(/Search by DRep ID/)
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Show all DReps/)).not.toBeInTheDocument();
  });

  it('shows the noFavorites empty state with a working back-to-directory action', () => {
    const onBackToDirectory = jest.fn();
    renderComponent({
      drepList: [realEntry(1)],
      view: 'favorites',
      onBackToDirectory,
    });

    expect(screen.getByText(/No favorites yet/)).toBeInTheDocument();
    expect(
      screen.getByText(/stored on this device only/)
    ).toBeInTheDocument();
    fireEvent.click(screen.getByText(/Back to directory/));
    expect(onBackToDirectory).toHaveBeenCalledTimes(1);
  });

  it('renders the stale caption only for entries the injected predicate marks stale', () => {
    renderComponent({
      drepList: [realEntry(1), realEntry(2)],
      favoriteDRepIds: new Set([realDrepId(1), realDrepId(2)]),
      view: 'favorites',
      isStaleFavoriteEntry: (entry: AppDRepDirectoryEntry) =>
        entry.drepId === realDrepId(2),
    });

    expect(
      screen.getAllByText(/no longer in the default cohort/)
    ).toHaveLength(1);
  });

  it('never renders the stale caption in the directory view', () => {
    renderComponent({
      drepList: [realEntry(1)],
      favoriteDRepIds: new Set([realDrepId(1)]),
      isStaleFavoriteEntry: () => true,
    });

    expect(
      screen.queryByText(/no longer in the default cohort/)
    ).not.toBeInTheDocument();
  });

  it('renders the favorites empty-state copy in ja-JP', () => {
    renderComponent({
      drepList: [realEntry(1)],
      view: 'favorites',
      locale: 'ja-JP',
    });

    expect(screen.getByText(/お気に入りはまだありません/)).toBeInTheDocument();
  });
});
```

Notes: the toggle name matchers use substring regex because en-US values carry
the `!!!` prefix. Row identity is asserted with `truncatedDrepId(n)` — the exact
`first8…last6` string (U+2026 ellipsis) that `DRepIdDisplay` renders for every
id longer than 18 chars (`DRepIdDisplay.tsx:35-42`) — plus `'!!!View details'`
card counts, the file's existing row-presence pattern (:564-568, :613). Never
assert with the full id or a bare prefix regex: the full id renders only as
tooltip/aria text (react-polymorph keeps the tooltip `tip` in the DOM as hidden
text), so a prefix regex like `new RegExp(realDrepId(2).slice(0, 8))` matches
both the visible `<code>` and the hidden tooltip copy and makes `getByText`
throw on multiple elements; the exact truncated string matches exactly one
element. The fixture ids diverge at char 8 (`realDrepId(1)` starts `drep1ygq`,
`realDrepId(2)` starts `drep1ygp` — verified live), so truncated strings are
unambiguous.

#### Step 24: `DRepDirectoryBanner.spec.tsx` — favorites mode

Extend `renderBanner` (:9-37) with optional `isFavoritesView` / `favoritesCount`
params (add to the type + pass to the component). Append:

```ts
  it('replaces the cohort and filtered lines with the favorites line in favorites mode', () => {
    renderBanner({
      isFavoritesView: true,
      favoritesCount: 3,
      isFilteredView: true,
      displayedCount: 9,
    });

    expect(
      screen.getByText(/3 DReps you've favorited\. Favorites are stored on this device only\./)
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/Default view shows up to 200/)
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/matching your filters/)
    ).not.toBeInTheDocument();
  });
```

(The regex apostrophe is the plain ASCII `'`, matching the copy.)

#### Step 25: `DRepDirectoryPage.spec.tsx` — container coverage

**25a.** Add to `buildGovernanceStore` (:37-51), keeping alphabetical field
order:

```ts
  favoriteDRepIds: new Set<string>(),
  toggleFavorite: jest.fn(),
```

**25b.** In `renderPage` (:53-82): accept an optional
`initialRoute = ROUTES.GOVERNANCE.DREPS` param, use it in
`createMemoryHistory({ initialEntries: [initialRoute] })`, and change the single
`<Route path={ROUTES.GOVERNANCE.DREPS} …>` to serve both routes:

```tsx
            <Route
              path={[ROUTES.GOVERNANCE.DREPS, ROUTES.GOVERNANCE.FAVORITES]}
              component={DRepDirectoryPage}
            />
```

Make sure `renderPage` returns the `governance` mock and `history` (follow the
file's existing return shape).

**25c.** Append tests:

```ts
  it('forwards favorite toggles to governanceStore.toggleFavorite with the row id', () => {
    const { governance } = renderPage();

    fireEvent.click(screen.getByRole('button', { name: /Add to favorites/ }));

    expect(governance.toggleFavorite).toHaveBeenCalledTimes(1);
    expect(governance.toggleFavorite).toHaveBeenCalledWith(drepEntry.drepId);
  });

  it('renders the favorites view on the favorites route', () => {
    renderPage({ initialRoute: ROUTES.GOVERNANCE.FAVORITES });

    expect(screen.getByText(/No favorites yet/)).toBeInTheDocument();
  });

  it('navigates back to the directory from the empty favorites state', () => {
    const { history } = renderPage({
      initialRoute: ROUTES.GOVERNANCE.FAVORITES,
    });

    fireEvent.click(screen.getByText(/Back to directory/));

    expect(history.location.pathname).toBe(ROUTES.GOVERNANCE.DREPS);
  });
```

#### Step 26: `VotingGovernancePage.spec.tsx` — 2 mock fields only

In the `governance` store mock (:87-101), add (alphabetical order):

```ts
    favoriteDRepIds: new Set<string>(),
    toggleFavorite: jest.fn(),
```

No other change to this file.

#### Step 27: `storybook/stories/governance/DRepDirectory.stories.tsx`

Global-locale rule: do NOT add any IntlProvider or per-locale story; the global
StoryWrapper decorator provides intl (see the comment at :132-134).

**27a.** Extend `renderDirectory` (:135-159) with a trailing options parameter:

```tsx
type FavoritesOptions = {
  view?: 'directory' | 'favorites';
  favoriteDRepIds?: Set<string>;
  onToggleFavorite?: (drepId: string) => void;
  onBackToDirectory?: () => void;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
};

const renderDirectory = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE,
  isCohortActive = false,
  favorites: FavoritesOptions = {}
) => (
  <DRepDirectory
    drepList={entries}
    drepIndex={new Map(entries.map((e) => [e.drepId, e]))}
    showAllList={entries}
    top35DRepIds={new Set<string>()}
    favoriteDRepIds={favorites.favoriteDRepIds ?? new Set<string>()}
    onToggleFavorite={favorites.onToggleFavorite ?? action('onToggleFavorite')}
    view={favorites.view ?? 'directory'}
    onBackToDirectory={
      favorites.onBackToDirectory ?? action('onBackToDirectory')
    }
    isStaleFavoriteEntry={favorites.isStaleFavoriteEntry}
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

The standalone `'Ranking unavailable'` story (:381-408) renders `<DRepDirectory>`
directly, so it also needs the one new REQUIRED prop added inline:
`onToggleFavorite={action('onToggleFavorite')}` (`favoriteDRepIds` is optional
and may be added as `new Set<string>()` for clarity).

Thread the same options through `renderCentered` (add the trailing parameter
and pass it along). Also thread favorites into the Connected flow: extend its
`withState` initial state with `favoriteDRepIds: [] as string[]`, add
`{ id: ROUTES.GOVERNANCE.FAVORITES, label: 'Favorites' }` to
`GOVERNANCE_NAV_ITEMS`, derive
`view = store.state.currentContentRoute === ROUTES.GOVERNANCE.FAVORITES ? 'favorites' : 'directory'`,
change the Navigation `activeItem`/`isActiveNavItem` to use
`store.state.currentContentRoute`, and pass
`{ view, favoriteDRepIds: new Set(store.state.favoriteDRepIds), onToggleFavorite: (id) => { action('onToggleFavorite')(id); store.set({ favoriteDRepIds: store.state.favoriteDRepIds.includes(id) ? store.state.favoriteDRepIds.filter((f) => f !== id) : [...store.state.favoriteDRepIds, id] }); }, onBackToDirectory: () => store.set({ currentContentRoute: ROUTES.GOVERNANCE.DREPS }) }`
into `renderDirectory`.

**27b.** Append four stories to the `storiesOf` chain:

```tsx
  .add(
    'Favorite toggle',
    withState({ favoriteDRepIds: [baseEntries[0].drepId] }, (store) => (
      <div style={CENTERED_STYLE}>
        {renderDirectory(
          GovernanceRefreshState.Loaded,
          baseEntries,
          null,
          DEFAULT_SYNC_STATE,
          true,
          {
            favoriteDRepIds: new Set(store.state.favoriteDRepIds),
            onToggleFavorite: (drepId: string) => {
              action('onToggleFavorite')(drepId);
              store.set({
                favoriteDRepIds: store.state.favoriteDRepIds.includes(drepId)
                  ? store.state.favoriteDRepIds.filter((id) => id !== drepId)
                  : [...store.state.favoriteDRepIds, drepId],
              });
            },
          }
        )}
      </div>
    ))
  )
  .add('Favorites view', () => (
    <div style={CENTERED_STYLE}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        true,
        {
          view: 'favorites',
          favoriteDRepIds: new Set(baseEntries.map((e) => e.drepId)),
        }
      )}
    </div>
  ))
  .add('Favorites view — empty', () => (
    <div style={CENTERED_STYLE}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        true,
        { view: 'favorites' }
      )}
    </div>
  ))
  // Synthetic staleness via the injected predicate: real Retired/doNotList
  // signals do not exist yet, so the story simulates the favorites-page
  // treatment (status badge + caption, never an auto-purge).
  .add('Favorites view — stale favorite', () => (
    <div style={CENTERED_STYLE}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        true,
        {
          view: 'favorites',
          favoriteDRepIds: new Set(baseEntries.map((e) => e.drepId)),
          isStaleFavoriteEntry: (entry: AppDRepDirectoryEntry) =>
            entry.drepId === baseEntries[1].drepId,
        }
      )}
    </div>
  ))
```

---

## Verification (run in this order, from the worktree root)

1. `node_modules/.bin/tsc --noEmit` → 0 errors.
2. `node_modules/.bin/eslint source/renderer/app/components/governance source/renderer/app/containers/governance source/renderer/app/containers/voting/Governance.tsx source/renderer/app/stores/GovernanceStore.ts source/renderer/app/api/utils/localStorage.ts source/common/config/electron-store.config.ts source/common/types/electron-store.types.ts source/renderer/app/routes-config.ts source/renderer/app/Routes.tsx storybook/stories/governance --ext .ts,.tsx` → 0 errors (pre-existing warnings acceptable).
3. Focused Jest, all green:
   `node_modules/.bin/jest source/renderer/app/components/governance/drep-directory/helpers.spec.ts tests/jest/governance/GovernanceStore.spec.ts source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx --no-coverage --runInBand`
4. Sanitization floor, file untouched:
   `node_modules/.bin/jest tests/jest/security/governance-sanitization.spec.ts --no-coverage --runInBand` → **23/23**, and
   `git diff --name-only | grep -c governance-sanitization` → 0.
5. Diff hygiene: `git diff | grep -nE "logger\.|analytics" ` over the favorites
   diff must show no NEW logger/analytics call in any added favorites code path.
6. `yarn i18n:manage` → clean; commit the regenerated
   `defaultMessages.json` / `translations/messages.json`.
7. `node_modules/.bin/prettier --write` on the touched `.ts/.tsx/.scss` files
   only (never JSON/locale/`.snap`).
8. Single commit: `feat(gov): task-122 persist DRep favorites via Electron local store`
   (subject only, no body, no trailers). Do not push.
