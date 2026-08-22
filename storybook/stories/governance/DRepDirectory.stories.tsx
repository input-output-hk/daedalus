import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import { withKnobs, select, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import type { ListViewMode } from '../../../source/renderer/app/types/listViewTypes';
import GovernanceShell, { GOVERNANCE_TABS } from './_utils/GovernanceShell';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import GovernanceWallets from '../../../source/renderer/app/components/governance/wallets/GovernanceWallets';
import DRepDirectory from '../../../source/renderer/app/components/governance/drep-directory/DRepDirectory';
import DRepDirectoryList from '../../../source/renderer/app/components/governance/drep-directory/DRepDirectoryList';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import BorderedBox from '../../../source/renderer/app/components/widgets/BorderedBox';
import type { SidebarMenus } from '../../../source/renderer/app/components/sidebar/types';
import {
  CATEGORIES_BY_NAME,
  SidebarCategoryInfo,
} from '../../../source/renderer/app/config/sidebarConfig';
import { ROUTES } from '../../../source/renderer/app/routes-config';
import GovernanceWithNavigation from '../../../source/renderer/app/components/governance/layouts/GovernanceWithNavigation';
import { makeDRepPopulation, drawCohortFrom } from './_utils/drepPopulation';
import { TESTNET } from '../../../source/common/types/environment.types';
import { GovernanceRefreshState } from '../../../source/renderer/app/stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';
import {
  DEFAULT_DREP_COHORT_CRITERIA,
  drawDRepCohort,
  nextDistinctDRepCohortSeed,
  selectDRepCohortPool,
} from '../../../source/renderer/app/components/governance/_shared/drepCohort';
import type {
  DRepCohortCriteria,
  DRepCohortCriterion,
} from '../../../source/renderer/app/components/governance/_shared/drepCohort';

type DirectoryError = { message: string; type: string } | null;

type DirectorySyncState = {
  isNodeInSync: boolean;
  syncProgress: number | null;
};

// Measured against mainnet on 2026-08-20: 1,062 registered DReps holding
// 5.257B ADA between them. Passing real totals is what lets the cards state a
// share at all, and what makes the 1.5% concentration threshold meaningful.
const TOTAL_DREP_STAKE = new BigNumber('5257000000000000');

// A spread rather than one repeated figure: the fixtures have to exercise both
// sides of the concentration threshold and every branch of the voting-power
// formatter, from three-digit millions down to a sub-thousand tail.
const VOTING_POWER_SPREAD = [
  '565800000000000', // 565.8M ADA, mainnet's largest DRep, 10.76%
  '87000000000000', //  87.0M ADA, the Target15 figure, just above 1.5%
  '70000000000000', //  70.0M ADA, just below the threshold
  '4200000000000', //    4.2M ADA
  '125000000000', //   125.0K ADA
  '940000000', //        940 ADA, small enough that its share rounds to zero
];

const DEFAULT_SYNC_STATE: DirectorySyncState = {
  isNodeInSync: true,
  syncProgress: 100,
};

// Names sampled from mainnet through Koios: 1,000 registered DReps, 404 with
// resolvable metadata. Given names run 1 to 76 characters (median 11, p95 24),
// and 4% carry non-Latin characters: CJK, katakana, Latin extended, the ada
// sign and emoji including regional-indicator flag pairs. A directory built
// only from ASCII names of comfortable length never shows what truncation,
// wrapping or font fallback actually do here.
const REAL_WORLD_NAMES = [
  'Porto Cripto DRep 🇧🇷 🇵🇹 (Atico & Bosco, from Cardanistas Stake Pool - CARDs)',
  'Sebastien Guillemot セバ',
  'Nimuë Lady of the Lake Pool',
  'Luc₳s @ 45B.io (Direct Voter)',
  'KOPI 咖啡 Singapore',
  'Jose Martinez | Atlas Network Transmissions',
  'No Treasury Withdrawals until ADA > $3',
  'Cardano Academy',
];

const baseEntries: AppDRepDirectoryEntry[] = [
  {
    anchor: {
      hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
      url: 'https://governance-preview.example.org/dreps/1.json',
    },
    verifiedName: 'Cardano Academy',
    doNotList: false,
    drepActivity: 12,
    drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k4',
    status: 'active',
    votingPower: new BigNumber('565800000000000'),
  },
  {
    anchor: null,
    verifiedName: null,
    doNotList: true,
    drepActivity: 4,
    drepId: 'drep1xj23tk3y_qyv7c9m2z89w3t8mvk9e2uwc3q8u6j7r2x5y9w0p1',
    status: 'inactive',
    votingPower: new BigNumber('940000000'),
  },
];

const CONNECTED_FLOW_WALLETS = [
  {
    walletId: 'wallet-1',
    walletName: 'Daily spending',
    currentDRep: {
      kind: 'drep' as const,
      drep: { raw: baseEntries[0].drepId } as any,
      source: 'onchain' as const,
    },
    drepEntry: baseEntries[0],
  },
  {
    walletId: 'wallet-2',
    walletName: 'Long-term savings',
    currentDRep: null,
    drepEntry: null,
  },
];

const buildEntry = (suffix: number): AppDRepDirectoryEntry => ({
  anchor:
    suffix % 3 === 0
      ? {
          hash: `6a5e200d2f3a10202020202020202020202020202020202020202020202020${String(
            suffix
          ).padStart(2, '0')}`,
          url: `https://governance-preview.example.org/dreps/${suffix}.json`,
        }
      : null,
  // Verified on the same entries that carry an anchor: the badge and the
  // metadata filter read one predicate, so the fixtures must not imply
  // otherwise.
  verifiedName:
    suffix % 3 === 0
      ? REAL_WORLD_NAMES[suffix % REAL_WORLD_NAMES.length]
      : null,
  doNotList: false,
  drepActivity: (suffix % 20) + 1,
  drepId: `drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k${String(
    suffix
  ).padStart(4, '0')}`,
  status: (['active', 'inactive'] as const)[suffix % 2],
  votingPower: new BigNumber(
    VOTING_POWER_SPREAD[suffix % VOTING_POWER_SPREAD.length]
  ),
});

// 0.08% and 1.33% of the total above: both under the 1.5% default ceiling,
// only one under the 0.5% option, so tightening that knob visibly narrows the
// pool rather than doing nothing.
const SUGGESTION_VOTING_POWER = ['4200000000000', '70000000000000'];

// The standard population for this screen: four hundred DReps in mainnet's
// proportions, from the shared generator. Stories draw their cohort out of it
// with the shipping selection rather than naming a list of their own, so what
// a story shows is what the app would produce from a chain of that shape.
const POPULATION = makeDRepPopulation(400, { seed: 11 });
const POPULATION_COHORT = drawCohortFrom(POPULATION);

const cohortEntry = (suffix: number): AppDRepDirectoryEntry => ({
  ...buildEntry(suffix),
  status: 'active',
  verifiedName: REAL_WORLD_NAMES[suffix % REAL_WORLD_NAMES.length],
  drepActivity: 12,
  votingPower: new BigNumber(SUGGESTION_VOTING_POWER[suffix % 2]),
});

// Forty DReps meeting every default criterion, and four missing exactly one
// each, so every toggle in the criteria panel changes what can be suggested.
// Asking for fifty exhausts the pool, which is what puts the relaxation
// disclosure on screen.
const suggestionPoolEntries: AppDRepDirectoryEntry[] = [
  ...Array.from({ length: 40 }, (_, i) => cohortEntry(i + 100)),
  { ...cohortEntry(200), status: 'inactive' },
  { ...cohortEntry(201), verifiedName: null },
  { ...cohortEntry(202), drepActivity: 2 },
  { ...cohortEntry(203), votingPower: new BigNumber(VOTING_POWER_SPREAD[0]) },
];

const dualIdEntries: AppDRepDirectoryEntry[] = [
  {
    ...baseEntries[0],
    drepId: 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras',
  },
  {
    ...baseEntries[1],
    drepId: 'drep1ygqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgweajrn',
  },
];

const SOCKET_ERROR: DirectoryError = {
  message: 'Cardano node socket path is not available.',
  type: 'SOCKET_UNAVAILABLE',
};

const SELFNODE_ERROR: DirectoryError = {
  message:
    'DRep data is unavailable in selfnode mode. A synced node is required.',
  type: 'SELFNODE_CLI_UNSUPPORTED',
};

const TIMEOUT_ERROR: DirectoryError = {
  message: 'DRep registration query timed out.',
  type: 'TIMEOUT',
};

const CONNECTED_FLOW_STYLE = {
  height: 780,
};

const GOVERNANCE_SIDEBAR_CATEGORIES: Array<SidebarCategoryInfo> = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.GOVERNANCE,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

const EMPTY_SIDEBAR_MENUS: SidebarMenus = {
  wallets: null,
};

type FavoritesOptions = {
  allDReps?: AppDRepDirectoryEntry[];
  cohortCriteria?: DRepCohortCriteria;
  onCohortCriteriaChange?: (criteria: DRepCohortCriteria) => void;
  relaxedCohortCriteria?: DRepCohortCriterion[];
  onReroll?: () => void;
  view?: 'directory' | 'favorites';
  favoriteDRepIds?: Set<string>;
  onToggleFavorite?: (drepId: string) => void;
  onBackToDirectory?: () => void;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  favoriteEntries?: AppDRepDirectoryEntry[];
  listViewMode?: ListViewMode;
  initialSearchQuery?: string;
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
const renderDirectory = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE,
  favorites: FavoritesOptions = {}
) => (
  <DRepDirectory
    suggestedDReps={entries}
    allDReps={favorites.allDReps ?? entries}
    cohortCriteria={favorites.cohortCriteria}
    onCohortCriteriaChange={favorites.onCohortCriteriaChange}
    relaxedCohortCriteria={favorites.relaxedCohortCriteria}
    favoriteDRepIds={favorites.favoriteDRepIds ?? new Set<string>()}
    onToggleFavorite={favorites.onToggleFavorite ?? action('onToggleFavorite')}
    view={favorites.view ?? 'directory'}
    onBackToDirectory={
      favorites.onBackToDirectory ?? action('onBackToDirectory')
    }
    isStaleFavoriteEntry={favorites.isStaleFavoriteEntry}
    favoriteEntries={favorites.favoriteEntries ?? []}
    initialSearchQuery={favorites.initialSearchQuery}
    listViewMode={favorites.listViewMode}
    totalDRepStake={TOTAL_DREP_STAKE}
    error={error}
    isNodeInSync={syncState.isNodeInSync}
    lastFetchedAt={Date.now() - 3 * 60 * 1000}
    onRefresh={action('onRefresh')}
    onReroll={favorites.onReroll ?? action('onReroll')}
    onLoadAllDReps={action('onLoadAllDReps')}
    onSelectForDelegation={action('onSelectForDelegation')}
    onViewDetails={action('onViewDetails')}
    refreshState={refreshState}
    syncProgress={syncState.syncProgress}
  />
);

const renderCentered = (
  refreshState: GovernanceRefreshState,
  entries: AppDRepDirectoryEntry[],
  error: DirectoryError = null,
  syncState: DirectorySyncState = DEFAULT_SYNC_STATE,
  favorites: FavoritesOptions = {}
) => (
  <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
    {renderDirectory(refreshState, entries, error, syncState, favorites)}
  </GovernanceShell>
);

const renderNonGovernancePlaceholder = (activeSidebarCategory: string) => (
  <BorderedBox>
    <h1 style={{ marginTop: 0 }}>Navigation Context</h1>
    <p style={{ marginBottom: 0 }}>
      Active sidebar route: {activeSidebarCategory}. Use the Governance icon to
      jump back into the connected DRep directory flow.
    </p>
  </BorderedBox>
);

// Maps a single knob value to the (refreshState, entries, error) triad so the
// connected flow can exercise every directory state without separate stories.
const DIRECTORY_STATE_OPTIONS = {
  Loaded: 'loaded',
  Empty: 'empty',
  Loading: 'loading',
  Refreshing: 'refreshing',
  'Refresh failed': 'refreshFailed',
  'Selfnode unavailable': 'selfnode',
  Error: 'error',
};

const resolveDirectoryState = (
  stateKey: string
): {
  refreshState: GovernanceRefreshState;
  entries: AppDRepDirectoryEntry[];
  error: DirectoryError;
} => {
  switch (stateKey) {
    case 'empty':
      return {
        refreshState: GovernanceRefreshState.Loaded,
        entries: [],
        error: null,
      };
    case 'loading':
      return {
        refreshState: GovernanceRefreshState.Loading,
        entries: [],
        error: null,
      };
    case 'refreshing':
      return {
        refreshState: GovernanceRefreshState.Refreshing,
        entries: baseEntries,
        // No error: a refresh in flight is not a refresh that failed, and
        // passing one here put the failure banner on both states.
        error: null,
      };
    case 'refreshFailed':
      return {
        refreshState: GovernanceRefreshState.Loaded,
        entries: baseEntries,
        error: TIMEOUT_ERROR,
      };
    case 'selfnode':
      return {
        refreshState: GovernanceRefreshState.Failed,
        entries: [],
        error: SELFNODE_ERROR,
      };
    case 'error':
      return {
        refreshState: GovernanceRefreshState.Failed,
        entries: [],
        error: SOCKET_ERROR,
      };
    case 'loaded':
    default:
      return {
        refreshState: GovernanceRefreshState.Loaded,
        entries: POPULATION_COHORT,
        error: null,
      };
  }
};

// No layout decorator here: the Connected flow story below builds the sidebar
// and top bar itself, and wrapping the file would nest a second set of chrome
// inside the first.
storiesOf('Governance / DRep Directory', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)
  // Full-app integrated flow: sidebar (Governance category active) + top bar +
  // the Governance "Directory" tab wrapping the live directory states. Mirrors
  // the "Voting / Governance > Connected flow" exemplar.
  .add(
    'Connected flow',
    withState(
      {
        activeSidebarCategory: ROUTES.GOVERNANCE.ROOT,
        currentContentRoute: ROUTES.GOVERNANCE.DREPS,
        favoriteDRepIds: [] as string[],
      },
      (store) => {
        const isGovernanceSection =
          store.state.currentContentRoute.indexOf(ROUTES.GOVERNANCE.ROOT) === 0;
        const isVotingCenter =
          store.state.currentContentRoute === ROUTES.GOVERNANCE.DASHBOARD;
        const view =
          store.state.currentContentRoute === ROUTES.GOVERNANCE.FAVORITES
            ? ('favorites' as const)
            : ('directory' as const);
        const { refreshState, entries, error } = resolveDirectoryState(
          select('Directory state', DIRECTORY_STATE_OPTIONS, 'loaded')
        );

        return (
          <div style={CONNECTED_FLOW_STYLE}>
            <SidebarLayout
              sidebar={
                <Sidebar
                  menus={EMPTY_SIDEBAR_MENUS}
                  categories={GOVERNANCE_SIDEBAR_CATEGORIES}
                  activeSidebarCategory={store.state.activeSidebarCategory}
                  isShowingSubMenus={false}
                  pathname={store.state.currentContentRoute}
                  network={TESTNET}
                  onActivateCategory={(category) => {
                    action('onActivateCategory')(category);

                    if (category === ROUTES.GOVERNANCE.ROOT) {
                      store.set({
                        activeSidebarCategory: ROUTES.GOVERNANCE.ROOT,
                        currentContentRoute: ROUTES.GOVERNANCE.DREPS,
                      });
                      return;
                    }

                    store.set({
                      activeSidebarCategory: category,
                      currentContentRoute: category,
                    });
                  }}
                  onAddWallet={action('onAddWallet')}
                  isShelleyActivated
                />
              }
              topbar={<TopBar isShelleyActivated />}
            >
              {/* The shipping layout, not an arrangement that resembles it.
                  Assembling the tabs and the page by hand here left the
                  windowed lists without the scrolling element that layout
                  publishes, so show-all rendered its first screen of cards and
                  then never learned that anyone had scrolled. */}
              {isGovernanceSection ? (
                <GovernanceWithNavigation
                  items={GOVERNANCE_TABS}
                  activeItem={store.state.currentContentRoute}
                  isActiveNavItem={(navItemId: string) =>
                    navItemId === store.state.currentContentRoute
                  }
                  onNavItemClick={(navItemId: string) => {
                    action('onNavItemClick')(navItemId);
                    store.set({ currentContentRoute: navItemId });
                  }}
                >
                  {isVotingCenter ? (
                    <GovernanceWallets
                      wallets={CONNECTED_FLOW_WALLETS}
                      favoriteDRepIds={new Set(store.state.favoriteDRepIds)}
                      totalDRepStake={TOTAL_DREP_STAKE}
                      onToggleFavorite={action('onToggleFavorite')}
                      onChangeDelegation={action('onChangeDelegation')}
                      onChooseDRep={() =>
                        store.set({
                          currentContentRoute: ROUTES.GOVERNANCE.DREPS,
                        })
                      }
                      onViewDetails={action('onViewDetails')}
                    />
                  ) : (
                    renderDirectory(
                      refreshState,
                      entries,
                      error,
                      DEFAULT_SYNC_STATE,
                      {
                        // Without this the directory's "all" list was the
                        // cohort, so showing all showed the same twenty.
                        allDReps: POPULATION,
                        view,
                        favoriteDRepIds: new Set(store.state.favoriteDRepIds),
                        onToggleFavorite: (drepId: string) => {
                          action('onToggleFavorite')(drepId);
                          store.set({
                            favoriteDRepIds:
                              store.state.favoriteDRepIds.includes(drepId)
                                ? store.state.favoriteDRepIds.filter(
                                    (id) => id !== drepId
                                  )
                                : [...store.state.favoriteDRepIds, drepId],
                          });
                        },
                        onBackToDirectory: () =>
                          store.set({
                            currentContentRoute: ROUTES.GOVERNANCE.DREPS,
                          }),
                      }
                    )
                  )}
                </GovernanceWithNavigation>
              ) : (
                renderNonGovernancePlaceholder(
                  store.state.activeSidebarCategory
                )
              )}
            </SidebarLayout>
          </div>
        );
      }
    )
  )
  // The cohort is drawn here by the shipping selection, out of the shared
  // population, under criteria the panel can change and a seed the reroll
  // button steps. Nothing about the suggestion is hand-picked.
  .add(
    'Loaded',
    withState({ criteria: DEFAULT_DREP_COHORT_CRITERIA, seed: 1 }, (store) => {
      const pool = selectDRepCohortPool(
        POPULATION,
        store.state.criteria,
        TOTAL_DREP_STAKE
      );
      const cohort = drawDRepCohort(pool, store.state.seed);

      return renderCentered(
        GovernanceRefreshState.Loaded,
        cohort,
        null,
        DEFAULT_SYNC_STATE,
        {
          allDReps: POPULATION,
          cohortCriteria: store.state.criteria,
          onCohortCriteriaChange: (criteria) => store.set({ criteria }),
          relaxedCohortCriteria: pool.relaxed,
          onReroll: () =>
            store.set({
              seed: nextDistinctDRepCohortSeed(
                pool,
                store.state.seed,
                new Set(cohort.map((entry) => entry.drepId))
              ),
            }),
        }
      );
    })
  )
  .add('Empty', () => renderCentered(GovernanceRefreshState.Loaded, []))
  .add('Error', () =>
    renderCentered(GovernanceRefreshState.Failed, [], SOCKET_ERROR)
  )
  .add('Selfnode unavailable', () =>
    renderCentered(GovernanceRefreshState.Failed, [], SELFNODE_ERROR)
  )
  .add('Loading', () => renderCentered(GovernanceRefreshState.Loading, []))
  .add('Refreshing', () =>
    renderCentered(GovernanceRefreshState.Refreshing, baseEntries)
  )
  .add('Refresh failed — retained snapshot', () =>
    renderCentered(GovernanceRefreshState.Loaded, baseEntries, TIMEOUT_ERROR)
  )
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
  // Voting power is what goes missing when the stake distribution fails to
  // load; the directory has no ranking of its own to lose.
  .add('Voting power unavailable', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      baseEntries.map((entry) => ({ ...entry, votingPower: null }))
    )
  )
  // Show-all over a full population: the list windows rather than paging, and
  // the mix is mainnet's rather than a set chosen to look tidy.
  .add('Show all — full population', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      POPULATION_COHORT,
      null,
      DEFAULT_SYNC_STATE,
      { allDReps: POPULATION }
    )
  )
  .add(
    'Favorite toggle',
    withState({ favoriteDRepIds: [baseEntries[0].drepId] }, (store) => (
      <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
        {renderDirectory(
          GovernanceRefreshState.Loaded,
          baseEntries,
          null,
          DEFAULT_SYNC_STATE,
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
      </GovernanceShell>
    ))
  )
  // Review item 2: the cohort is a random twenty, so a favorite is usually not
  // in it. Pinned above, they are reachable without switching to Show All.
  .add('Pinned favorites above the cohort', () => {
    const pinned = buildEntry(101);
    return (
      <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
        {renderDirectory(
          GovernanceRefreshState.Loaded,
          baseEntries,
          null,
          DEFAULT_SYNC_STATE,
          {
            favoriteEntries: [pinned],
            favoriteDRepIds: new Set([pinned.drepId]),
          }
        )}
      </GovernanceShell>
    );
  })
  .add('Pinned favorites — favorite already in the cohort', () => (
    <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        {
          favoriteEntries: [baseEntries[0]],
          favoriteDRepIds: new Set([baseEntries[0].drepId]),
        }
      )}
    </GovernanceShell>
  ))
  // The table mirrors the stake pools list view: same choice, same default.
  // Toggle it with the control in the filter row.
  .add('Table view', () => (
    <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        POPULATION_COHORT,
        null,
        DEFAULT_SYNC_STATE,
        { allDReps: POPULATION, listViewMode: 'table' }
      )}
    </GovernanceShell>
  ))
  .add('Favorites view', () => (
    <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        {
          view: 'favorites',
          favoriteDRepIds: new Set(baseEntries.map((e) => e.drepId)),
        }
      )}
    </GovernanceShell>
  ))
  .add('Favorites view — empty', () => (
    <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        { view: 'favorites' }
      )}
    </GovernanceShell>
  ))
  // The favorites treatment for a real verified doNotList entry: status badge
  // plus inline caption, never an auto-purge.
  .add('Favorites view — stale favorite', () => (
    <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        {
          view: 'favorites',
          favoriteDRepIds: new Set(baseEntries.map((e) => e.drepId)),
        }
      )}
    </GovernanceShell>
  ))
  // Search shown where it happens: the whole directory, with a query already
  // in the box. A bare results list left out the search field, the label above
  // the results and the controls that a user actually reaches search through.
  .add('Search results', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      dualIdEntries,
      null,
      DEFAULT_SYNC_STATE,
      { initialSearchQuery: 'Cardano' }
    )
  );
