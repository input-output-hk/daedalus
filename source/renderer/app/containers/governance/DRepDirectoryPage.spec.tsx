import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../themes/daedalus';
import { themeOverrides } from '../../themes/overrides';
import { ROUTES } from '../../routes-config';
import { GovernanceRefreshState } from '../../stores/GovernanceStore';
import { DEFAULT_DREP_COHORT_CRITERIA } from '../../components/governance/_shared/drepCohort';
import DRepDirectoryPage from './DRepDirectoryPage';

const drepEntry = {
  anchor: null,
  verifiedName: null,
  doNotList: false,
  drepActivity: 12,
  drepId: 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b',
  status: 'active' as const,
  votingPower: new BigNumber('23137980123456'),
};

const buildGovernanceStore = (
  entry: typeof drepEntry = drepEntry,
  favoriteDRepIds: Set<string> = new Set<string>(),
  governanceOverrides: Record<string, unknown> = {}
) => ({
  error: null,
  favoriteDRepIds,
  lastFetchedAt: Date.now() - 60_000,
  delegationNavState: null,
  loadAllDReps: jest.fn(),
  refresh: jest.fn(),
  rerollCohort: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  allDReps: [entry],
  suggestedDReps: [entry],
  cohortCriteria: DEFAULT_DREP_COHORT_CRITERIA,
  cohortPool: {
    entries: [entry],
    criteria: DEFAULT_DREP_COHORT_CRITERIA,
    relaxed: [],
    strictSize: 1,
  },
  setCohortCriteria: jest.fn(),
  lookupDRep: jest.fn(() => entry),
  setDelegationNavState: jest.fn(),
  toggleFavorite: jest.fn(),
  // Favourites are pinned above the cohort, so the page resolves them by id
  // on mount rather than relying on the random sample containing them.
  favoriteEntries: [],
  ensureFavorites: jest.fn().mockResolvedValue(undefined),
  ...governanceOverrides,
});

const renderPage = ({
  isNodeInSync = true,
  syncProgress = 100,
  initialRoute = ROUTES.GOVERNANCE.DREPS,
  entry = drepEntry,
  favoriteDRepIds = new Set<string>(),
  governanceOverrides = {},
}: {
  isNodeInSync?: boolean;
  syncProgress?: number | null;
  initialRoute?: string;
  entry?: typeof drepEntry;
  favoriteDRepIds?: Set<string>;
  governanceOverrides?: Record<string, unknown>;
} = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore(
    entry,
    favoriteDRepIds,
    governanceOverrides
  );
  const history = createMemoryHistory({
    initialEntries: [initialRoute],
  });
  const view = render(
    <Provider
      stores={
        {
          governance,
          networkStatus,
          wallets: {
            all: [{ id: 'wallet-1' }],
            allWallets: [{ id: 'wallet-1' }],
          },
        } as any
      }
    >
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <Router history={history}>
            <Route
              path={[ROUTES.GOVERNANCE.DREPS, ROUTES.GOVERNANCE.FAVORITES]}
              component={DRepDirectoryPage}
            />
          </Router>
        </IntlProvider>
      </ThemeProvider>
    </Provider>
  );
  return { governance, networkStatus, history, ...view };
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

  it('renders the suggested list and rerolls the cohort in place', () => {
    const { governance } = renderPage();

    fireEvent.click(screen.getByText('!!!Show different suggestions'));

    // A reroll redraws from the list already in hand. Refetching would put a
    // request behind a control whose whole point is that it needs none.
    expect(governance.rerollCohort).toHaveBeenCalledTimes(1);
    expect(governance.refresh).not.toHaveBeenCalled();
    expect(governance.loadAllDReps).toHaveBeenCalled();
  });

  it('never triggers a store refresh from search interactions', () => {
    const { governance } = renderPage();

    const input = screen.getByPlaceholderText('!!!Search by DRep ID or name');
    fireEvent.change(input, { target: { value: 'drep1abcdefgh' } });
    fireEvent.change(input, { target: { value: `drep1${'q'.repeat(51)}` } });

    expect(governance.refresh).not.toHaveBeenCalled();
    expect(governance.rerollCohort).not.toHaveBeenCalled();
  });

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

  it('captions a doNotList favorite on the favorites route with no predicate injected', () => {
    renderPage({
      initialRoute: ROUTES.GOVERNANCE.FAVORITES,
      entry: { ...drepEntry, doNotList: true },
      favoriteDRepIds: new Set([drepEntry.drepId]),
    });

    expect(
      screen.getByText(/not actively accepting delegation/)
    ).toBeInTheDocument();
  });

  it('keeps the selfnode empty state across remounts', () => {
    const selfnodeOverrides = {
      suggestedDReps: [],
      allDReps: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      refreshState: GovernanceRefreshState.Failed,
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

  it('auto-favorites the DRep when it is selected for delegation', () => {
    const { governance } = renderPage();

    fireEvent.click(screen.getByRole('button', { name: '!!!Delegate' }));

    expect(governance.toggleFavorite).toHaveBeenCalledTimes(1);
    expect(governance.toggleFavorite).toHaveBeenCalledWith(drepEntry.drepId);
  });

  it('does not call toggleFavorite when the selected DRep is already favorited', () => {
    const { governance } = renderPage({
      favoriteDRepIds: new Set([drepEntry.drepId]),
    });

    fireEvent.click(screen.getByRole('button', { name: '!!!Delegate' }));

    expect(governance.toggleFavorite).not.toHaveBeenCalled();
  });

  it('does not call toggleFavorite when a sentinel (Abstain) is selected', () => {
    const { governance } = renderPage();

    // Sentinel cards use '!!!Select' (not '!!!Delegate')
    const sentinelButton = screen.getAllByRole('button', {
      name: '!!!Select',
    })[0];
    fireEvent.click(sentinelButton);

    expect(governance.toggleFavorite).not.toHaveBeenCalled();
  });
});
