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
  displayedDRepList: [drepEntry],
  drepIndex: new Map([[drepEntry.drepId, drepEntry]]),
  drepList: [drepEntry],
  error: null,
  isCohortActive: true,
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  reshuffleCohort: jest.fn(),
  showAllList: [drepEntry],
  top35DRepIds: new Set<string>(),
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
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <Router history={history}>
            <Route
              path={ROUTES.GOVERNANCE.DREPS}
              component={DRepDirectoryPage}
            />
          </Router>
        </IntlProvider>
      </ThemeProvider>
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

  it('renders the displayed list and forwards Reshuffle to the store', () => {
    const { governance } = renderPage();

    fireEvent.click(screen.getByText('!!!Reshuffle order'));

    expect(governance.reshuffleCohort).toHaveBeenCalledTimes(1);
    expect(governance.refresh).not.toHaveBeenCalled();
  });

  it('never triggers a store fetch from search interactions', () => {
    const { governance } = renderPage();

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    fireEvent.change(input, { target: { value: 'drep1abcdefgh' } });
    fireEvent.change(input, { target: { value: `drep1${'q'.repeat(51)}` } });

    expect(governance.refresh).not.toHaveBeenCalled();
    expect(governance.reshuffleCohort).not.toHaveBeenCalled();
  });
});
