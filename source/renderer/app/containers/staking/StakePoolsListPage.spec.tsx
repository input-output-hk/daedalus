import React from 'react';
import { Provider } from 'mobx-react';
import { cleanup, render } from '@testing-library/react';
import '@testing-library/jest-dom';
import StakePoolsListPage from './StakePoolsListPage';

// The injected props come from the Provider.
const Page = StakePoolsListPage as unknown as React.ComponentType;

jest.mock('../../components/staking/stake-pools/StakePools', () => ({
  __esModule: true,
  default: () => null,
}));
jest.mock(
  '../../components/staking/stake-pools/StakePoolsRankingLoader',
  () => ({
    __esModule: true,
    default: () => null,
  })
);
jest.mock(
  '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog',
  () => ({ __esModule: true, default: () => null })
);
jest.mock('./dialogs/DelegationSetupWizardDialogContainer', () => ({
  __esModule: true,
  default: () => null,
}));
jest.mock('../../components/analytics/withAnalytics', () => ({
  withAnalytics: (Component: React.ComponentType) => Component,
}));

const renderPage = () => {
  const release = jest.fn();
  const acquireStakePools = jest.fn(() => release);
  const stores = {
    app: { openExternalLink: jest.fn() },
    uiDialogs: { isOpen: () => false },
    staking: {
      stakePools: [],
      stakePoolsRequest: { isExecuting: false },
      recentStakePools: [],
      getStakePoolById: jest.fn(),
      acquireStakePools,
    },
    wallets: { all: [] },
    networkStatus: { isSynced: true },
    profile: { currentLocale: 'en-US', currentTheme: 'light-blue' },
    analytics: {},
  };
  const view = render(
    <Provider stores={stores as any} actions={{ dialogs: {} } as any}>
      <Page />
    </Provider>
  );
  return { acquireStakePools, release, ...view };
};

describe('StakePoolsListPage', () => {
  afterEach(cleanup);

  it('asks for the stake pool list while it is open and withdraws the ask when it closes', () => {
    const { acquireStakePools, release, unmount } = renderPage();
    expect(acquireStakePools).toHaveBeenCalledTimes(1);
    expect(release).not.toHaveBeenCalled();
    unmount();
    expect(release).toHaveBeenCalledTimes(1);
  });
});
