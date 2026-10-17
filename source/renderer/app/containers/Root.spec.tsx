import React from 'react';
import { Provider } from 'mobx-react';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import Root from './Root';

jest.mock('./loading/LoadingPage', () => ({
  __esModule: true,
  default: () => <div>loading page</div>,
}));
jest.mock('./wallet/WalletAddPage', () => ({
  __esModule: true,
  default: () => <div>add wallet page</div>,
}));
jest.mock('./splash/SplashNetworkPage', () => ({
  __esModule: true,
  default: () => <div>splash page</div>,
}));
jest.mock('./staking/RedeemItnRewardsContainer', () => ({
  __esModule: true,
  default: () => <div>redeem page</div>,
}));
jest.mock('./appUpdate/AppUpdateContainer', () => ({
  __esModule: true,
  default: () => <div>app update page</div>,
}));
jest.mock('../components/wallet/wallet-import/WalletImportFileDialog', () => ({
  __esModule: true,
  default: () => null,
}));

type Page = 'staking' | 'settings' | 'voting' | 'governance' | 'profile';

const makeStores = (loadingPhase: string, page: Page) => ({
  app: {
    environment: { isTest: false },
    isSetupPage: false,
  },
  appUpdate: { displayAppUpdateOverlay: false },
  backend: { loadingPhase },
  networkStatus: {
    isConnected: true,
    isNotEnoughDiskSpace: false,
    isSplashShown: false,
    isSystemTimeCorrect: true,
  },
  profile: {
    isProfilePage: page === 'profile',
    isSettingsPage: page === 'settings',
    isCurrentLocaleSet: true,
    areTermsOfUseAccepted: true,
  },
  staking: { isStakingPage: page === 'staking', redeemStep: null },
  voting: { isVotingPage: page === 'voting' },
  governance: { isGovernancePage: page === 'governance' },
  uiDialogs: { isOpen: () => false },
  wallets: { hasLoadedWallets: true, hasAnyWallets: true },
});

const renderRoot = (loadingPhase: string, page: Page) =>
  render(
    <Provider stores={makeStores(loadingPhase, page)} actions={{}}>
      <Root>
        <div>page content</div>
      </Root>
    </Provider>
  );

describe('Root', () => {
  afterEach(cleanup);

  it.each([
    'staking',
    'settings',
    'voting',
    'governance',
    'profile',
  ] as Array<Page>)(
    'shows the page content on %s while the node is syncing',
    (page) => {
      renderRoot('node-starting', page);
      expect(screen.getByText('page content')).toBeInTheDocument();
      expect(screen.queryByText('loading page')).toBeNull();
    }
  );

  it.each([
    'staking',
    'settings',
    'voting',
    'governance',
    'profile',
  ] as Array<Page>)(
    'shows the loading page instead of %s when the chain data is damaged',
    (page) => {
      renderRoot('chain-damaged', page);
      expect(screen.getByText('loading page')).toBeInTheDocument();
      expect(screen.queryByText('page content')).toBeNull();
    }
  );
});
