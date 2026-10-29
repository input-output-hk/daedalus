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
jest.mock('./appUpdate/AppUpdateContainer', () => ({
  __esModule: true,
  default: () => <div>app update page</div>,
}));
jest.mock('../components/wallet/wallet-import/WalletImportFileDialog', () => ({
  __esModule: true,
  default: () => null,
}));

type Page = 'staking' | 'settings' | 'voting' | 'governance' | 'profile';

// The pages shown without waiting for the node: staking, settings, voting
// and governance once connected, and the profile pages at any time
const pages: Array<Page> = [
  'staking',
  'settings',
  'voting',
  'governance',
  'profile',
];

// Each loading phase of a Mithril sync or a rebuild, on each of those pages
const recoveryCases: Array<[string, Page]> = pages.flatMap((page) =>
  ['chain-damaged', 'mithril-syncing', 'bootstrap-decision'].map(
    (phase): [string, Page] => [phase, page]
  )
);

const makeStores = (
  loadingPhase: string,
  page: Page,
  chainStoragePickerRequested = false
) => ({
  app: {
    environment: { isTest: false },
    isSetupPage: false,
  },
  appUpdate: { displayAppUpdateOverlay: false },
  backend: { loadingPhase, chainStoragePickerRequested },
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
  staking: { isStakingPage: page === 'staking' },
  voting: { isVotingPage: page === 'voting' },
  governance: { isGovernancePage: page === 'governance' },
  uiDialogs: { isOpen: () => false },
  wallets: { hasLoadedWallets: true, hasAnyWallets: true },
});

const renderRoot = (
  loadingPhase: string,
  page: Page,
  chainStoragePickerRequested = false
) =>
  render(
    <Provider
      stores={makeStores(loadingPhase, page, chainStoragePickerRequested)}
      actions={{}}
    >
      <Root>
        <div>page content</div>
      </Root>
    </Provider>
  );

describe('Root', () => {
  afterEach(cleanup);

  it.each(pages)(
    'shows the page content on %s while the node is syncing',
    (page) => {
      renderRoot('node-starting', page);
      expect(screen.getByText('page content')).toBeInTheDocument();
      expect(screen.queryByText('loading page')).toBeNull();
    }
  );

  it.each(recoveryCases)(
    'shows the loading page in the %s phase instead of %s',
    (phase, page) => {
      renderRoot(phase, page);
      expect(screen.getByText('loading page')).toBeInTheDocument();
      expect(screen.queryByText('page content')).toBeNull();
    }
  );

  it.each(pages)(
    'shows the location picker asked for from the Mithril error view instead of %s',
    (page) => {
      renderRoot('chain-storage-setup', page, true);
      expect(screen.getByText('loading page')).toBeInTheDocument();
      expect(screen.queryByText('page content')).toBeNull();
    }
  );

  it("shows the setup pages before a first run's location picker", () => {
    renderRoot('chain-storage-setup', 'profile');
    expect(screen.getByText('page content')).toBeInTheDocument();
    expect(screen.queryByText('loading page')).toBeNull();
  });

  it.each(pages)(
    'shows the page content on %s while the wallet is ready',
    (page) => {
      renderRoot('ready', page);
      expect(screen.getByText('page content')).toBeInTheDocument();
      expect(screen.queryByText('loading page')).toBeNull();
    }
  );
});
