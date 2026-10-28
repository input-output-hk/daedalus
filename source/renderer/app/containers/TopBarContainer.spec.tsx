import React from 'react';
import { Provider } from 'mobx-react';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import { ROUTES } from '../routes-config';
import TopBarContainer from './TopBarContainer';

// TopBar is replaced by a button that calls the handler the container hands
// down, so the spec covers what the container does with it.
jest.mock('../components/layout/TopBar', () => ({
  __esModule: true,
  default: ({
    activeWallet,
    onDelegateHeldRewards,
  }: {
    activeWallet: { id: string };
    onDelegateHeldRewards: (walletId: string) => void;
  }) => (
    <button
      type="button"
      onClick={() => onDelegateHeldRewards(activeWallet.id)}
    >
      delegate held rewards
    </button>
  ),
}));
jest.mock(
  '../components/widgets/NodeSyncStatusIcon',
  () =>
    function NodeSyncStatusIconMock() {
      return null;
    }
);
jest.mock(
  '../components/widgets/NewsFeedIcon',
  () =>
    function NewsFeedIconMock() {
      return null;
    }
);
jest.mock(
  '../components/widgets/TadaButton',
  () =>
    function TadaButtonMock() {
      return null;
    }
);
jest.mock(
  '../components/widgets/WalletTestEnvironmentLabel',
  () =>
    function WalletTestEnvironmentLabelMock() {
      return null;
    }
);
jest.mock('../features/discreet-mode', () => ({
  DiscreetToggleTopBar: () => null,
}));

// The injected container takes its props from the Provider.
const Container = TopBarContainer as unknown as React.ComponentType;

const WALLET_ID = 'a'.repeat(40);

const renderContainer = () => {
  const setDelegationNavState = jest.fn();
  const goToRoute = { trigger: jest.fn() };
  const stores = {
    sidebar: { isShowingSubMenus: false },
    app: {
      currentRoute: `${ROUTES.WALLETS.ROOT}/${WALLET_ID}/summary`,
      environment: { isMainnet: true, network: 'mainnet' },
      openExternalLink: jest.fn(),
    },
    networkStatus: {
      isSynced: true,
      syncPercentage: 100,
      isShelleyActivated: true,
      isAlonzoActivated: true,
      isAlonzoPending: false,
    },
    wallets: {
      active: { id: WALLET_ID },
      isWalletRoute: true,
      hasAnyWallets: true,
      hasRewardsWallets: false,
    },
    newsFeed: { newsFeedData: { unread: [] } },
    appUpdate: { displayAppUpdateNewsItem: false },
    staking: { stakingInfoWasOpen: true },
    governance: { setDelegationNavState },
    voting: { hasSubmittedVoteDelegation: () => false },
  };
  const actions = {
    router: { goToRoute },
    sidebar: { toggleSubMenus: { trigger: jest.fn() } },
    app: { toggleNewsFeed: { trigger: jest.fn() } },
    wallets: { transferFundsSetSourceWalletId: { trigger: jest.fn() } },
  };
  render(
    <Provider stores={stores} actions={actions}>
      <Container />
    </Provider>
  );
  return { setDelegationNavState, goToRoute };
};

describe('TopBarContainer', () => {
  afterEach(cleanup);

  it('selects the wallet in the delegation form state and opens the form', () => {
    const { setDelegationNavState, goToRoute } = renderContainer();
    screen.getByRole('button', { name: 'delegate held rewards' }).click();
    expect(setDelegationNavState).toHaveBeenCalledWith({
      selectedWalletId: WALLET_ID,
    });
    expect(goToRoute.trigger).toHaveBeenCalledWith({
      route: ROUTES.GOVERNANCE.DELEGATE,
    });
  });
});
