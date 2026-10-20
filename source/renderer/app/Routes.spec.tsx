import React from 'react';
import { render, cleanup } from '@testing-library/react';
import { createMemoryHistory } from 'history';
import { Router } from 'react-router-dom';
import { Routes } from './Routes';
import { ROUTES } from './routes-config';
import Apps from './containers/dapp/Apps';
import DappConnectionsSettingsPage from './containers/settings/categories/DappConnectionsSettingsPage';

let mockRootHidesChildren = false;

jest.mock(
  './containers/Root',
  () =>
    function MockRoot({ children }: { children: React.ReactNode }) {
      return mockRootHidesChildren ? null : children;
    }
);
jest.mock(
  './containers/wallet/Wallet',
  () =>
    ({ children }: { children: React.ReactNode }) =>
      children
);
jest.mock(
  './containers/settings/Settings',
  () =>
    ({ children }: { children: React.ReactNode }) =>
      children
);
jest.mock('./analytics/TrackedRoute', () => {
  const { Route } = jest.requireActual('react-router-dom');
  return function (props: object) {
    return <Route {...props} />;
  };
});
jest.mock('./containers/dapp/Apps', () =>
  jest.fn(({ children }: { children: React.ReactNode }) => children)
);
jest.mock('./containers/dapp/AppsRootRedirect', () => jest.fn(() => null));
jest.mock('./containers/dapp/DappCatalogPage', () => jest.fn(() => null));
jest.mock('./containers/settings/categories/DappConnectionsSettingsPage', () =>
  jest.fn(() => null)
);
jest.mock(
  './containers/profile/InitialSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/GeneralSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/WalletsSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/StakePoolsSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/SupportSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/TermsOfUseSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/SecuritySettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/settings/categories/DisplaySettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/profile/TermsOfUsePage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/profile/DataLayerMigrationPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/profile/AnalyticsConsentPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/PaperWalletCreateCertificatePage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/Staking',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/DelegationCenterPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/StakingEpochsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/StakingInfoPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/StakingRewardsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/StakePoolsListPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/StakingCountdownPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/staking/RedeemItnRewardsContainer',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletAddPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletSummaryPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletSendPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletReceivePage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletTransactionsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletTokensPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletSettingsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/wallet/WalletUtxoPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/voting/VotingRegistrationPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/voting/Voting',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/voting/VotingGovernancePage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/voting/Governance',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/governance/DRepDirectoryPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/governance/DRepDetailPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/governance/GovernanceWalletsPage',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  './containers/governance/GovernanceRootRedirect',
  () =>
    function () {
      return null;
    }
);

describe('pilot route isolation', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    mockRootHidesChildren = false;
  });
  afterEach(cleanup);

  it.each([
    [ROUTES.APPS.ROOT, ROUTES.WALLETS.SUMMARY],
    ['/apps/wallet-a', ROUTES.WALLETS.SUMMARY],
    [ROUTES.SETTINGS.DAPP_CONNECTIONS, ROUTES.SETTINGS.GENERAL],
  ])(
    'redirects disabled %s without mounting pilot containers',
    (path, landing) => {
      global.cip30Enabled = false;
      const history = createMemoryHistory({ initialEntries: [path] });
      render(
        <Router history={history}>
          <Routes />
        </Router>
      );
      expect(history.location.pathname).toBe(landing);
      expect(Apps).not.toHaveBeenCalled();
      expect(DappConnectionsSettingsPage).not.toHaveBeenCalled();
    }
  );

  it.each([ROUTES.APPS.ROOT, '/apps/wallet-a'])(
    'redirects disabled %s even when Root hides routes for an empty profile',
    (path) => {
      global.cip30Enabled = false;
      mockRootHidesChildren = true;
      const history = createMemoryHistory({ initialEntries: [path] });
      render(
        <Router history={history}>
          <Routes />
        </Router>
      );
      expect(history.location.pathname).toBe(ROUTES.WALLETS.ROOT);
      expect(Apps).not.toHaveBeenCalled();
    }
  );

  it.each(['/apps/wallet-a', ROUTES.SETTINGS.DAPP_CONNECTIONS])(
    'keeps enabled %s reachable independently of launcher availability',
    (path) => {
      global.cip30Enabled = true;
      const history = createMemoryHistory({ initialEntries: [path] });
      render(
        <Router history={history}>
          <Routes />
        </Router>
      );
      expect(history.location.pathname).toBe(path);
      expect(
        path.startsWith('/apps') ? Apps : DappConnectionsSettingsPage
      ).toHaveBeenCalled();
    }
  );
});
