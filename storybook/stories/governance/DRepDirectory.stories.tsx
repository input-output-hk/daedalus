import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import { withKnobs, select, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepDirectory from '../../../source/renderer/app/components/governance/drep-directory/DRepDirectory';
import Navigation from '../../../source/renderer/app/components/navigation/Navigation';
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
import { TESTNET } from '../../../source/common/types/environment.types';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../source/renderer/app/stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';

type DirectoryError = { message: string; type: string } | null;

type DirectorySyncState = {
  isNodeInSync: boolean;
  syncProgress: number | null;
};

const DEFAULT_SYNC_STATE: DirectorySyncState = {
  isNodeInSync: true,
  syncProgress: 100,
};

const baseEntries: AppDRepDirectoryEntry[] = [
  {
    anchor: {
      hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
      url: 'https://governance-preview.example.org/dreps/1.json',
    },
    drepActivity: 12,
    drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k4',
    status: 'active',
    votingPower: new BigNumber('23137980123456'),
  },
  {
    anchor: null,
    drepActivity: 4,
    drepId: 'drep1xj23tk3y_qyv7c9m2z89w3t8mvk9e2uwc3q8u6j7r2x5y9w0p1',
    status: 'inactive',
    votingPower: new BigNumber('940000000'),
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
  drepActivity: (suffix % 20) + 1,
  drepId: `drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k${String(
    suffix
  ).padStart(4, '0')}`,
  status: (['active', 'inactive'] as const)[suffix % 2],
  votingPower: new BigNumber(`${23137980123456 + suffix * 1000}`),
});

const paginatedEntries: AppDRepDirectoryEntry[] = Array.from(
  { length: 30 },
  (_, i) => buildEntry(i + 1)
);

const SOCKET_ERROR: DirectoryError = {
  message: 'Cardano node socket path is not available.',
  type: 'SOCKET_UNAVAILABLE',
};

const REFRESH_ERROR: DirectoryError = {
  message:
    'Showing the last successful directory snapshot while refresh retries.',
  type: 'QUERY_FAILED',
};

const CENTERED_STYLE = {
  margin: '0 auto',
  maxWidth: 960,
  padding: 24,
};

const CONNECTED_FLOW_STYLE = {
  height: 780,
};

const FLOW_CONTENT_STYLE = {
  padding: 32,
};

const FLOW_SECTION_STYLE = {
  display: 'flex',
  flexDirection: 'column' as const,
  gap: 24,
};

const GOVERNANCE_NAV_ITEMS = [
  {
    id: ROUTES.GOVERNANCE.DREPS,
    label: 'Directory',
  },
];

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

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
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
        error: REFRESH_ERROR,
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
        entries: paginatedEntries,
        error: null,
      };
  }
};

const drepStoryDecorator = (story: () => React.ReactNode) => (
  <StoryProvider>
    <StoryDecorator>{story()}</StoryDecorator>
  </StoryProvider>
);

storiesOf('Governance / DRep Directory', module)
  .addDecorator(drepStoryDecorator)
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
      },
      (store) => {
        const isGovernanceSection =
          store.state.currentContentRoute.indexOf(ROUTES.GOVERNANCE.ROOT) === 0;
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
              <div style={FLOW_CONTENT_STYLE}>
                {isGovernanceSection ? (
                  <div style={FLOW_SECTION_STYLE}>
                    <Navigation
                      items={GOVERNANCE_NAV_ITEMS}
                      activeItem={ROUTES.GOVERNANCE.DREPS}
                      isActiveNavItem={(navItemId: string) =>
                        navItemId === ROUTES.GOVERNANCE.DREPS
                      }
                      onNavItemClick={(navItemId: string) => {
                        action('onNavItemClick')(navItemId);
                        store.set({ currentContentRoute: navItemId });
                      }}
                    />
                    {renderDirectory(
                      refreshState,
                      entries,
                      error,
                      DEFAULT_SYNC_STATE,
                      refreshState === GovernanceRefreshState.Loaded ||
                        refreshState === GovernanceRefreshState.Refreshing
                    )}
                  </div>
                ) : (
                  renderNonGovernancePlaceholder(
                    store.state.activeSidebarCategory
                  )
                )}
              </div>
            </SidebarLayout>
          </div>
        );
      }
    )
  )
  .add('Loaded', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      baseEntries,
      null,
      DEFAULT_SYNC_STATE,
      true
    )
  )
  .add('Empty', () => renderCentered(GovernanceRefreshState.Loaded, []))
  .add('Error', () =>
    renderCentered(GovernanceRefreshState.Failed, [], SOCKET_ERROR)
  )
  .add('Loading', () => renderCentered(GovernanceRefreshState.Loading, []))
  .add('Refreshing', () =>
    renderCentered(
      GovernanceRefreshState.Refreshing,
      baseEntries,
      REFRESH_ERROR,
      DEFAULT_SYNC_STATE,
      true
    )
  )
  .add('Node syncing', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      baseEntries,
      null,
      {
        isNodeInSync: false,
        syncProgress: number('Sync progress (%)', 87, {
          max: 100,
          min: 0,
          range: true,
          step: 1,
        }),
      },
      true
    )
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
  .add('Ranking unavailable', () => (
    <div style={CENTERED_STYLE}>
      <DRepDirectory
        drepList={baseEntries.map((entry) => ({ ...entry, votingPower: null }))}
        error={null}
        isCohortActive={false}
        isNodeInSync
        lastFetchedAt={Date.now() - 3 * 60 * 1000}
        onRefresh={action('onRefresh')}
        onReshuffle={action('onReshuffle')}
        onSelectForDelegation={action('onSelectForDelegation')}
        onViewDetails={action('onViewDetails')}
        refreshState={GovernanceRefreshState.Loaded}
        syncProgress={100}
        votingPowerState={VotingPowerEnrichState.Failed}
      />
    </div>
  ))
  .add('Pagination — 30 entries', () =>
    renderCentered(
      GovernanceRefreshState.Loaded,
      paginatedEntries,
      null,
      DEFAULT_SYNC_STATE,
      true
    )
  );
