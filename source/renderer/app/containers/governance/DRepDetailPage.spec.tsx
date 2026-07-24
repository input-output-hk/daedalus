import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import jaTranslations from '../../i18n/locales/ja-JP.json';
import { ROUTES } from '../../routes-config';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../stores/GovernanceStore';
import DRepDetailPage from './DRepDetailPage';

const DREP_ID = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';
// The route literal lands with the route-wiring task; deriving the path from
// the directory literal keeps this harness aligned with it.
const DETAIL_PATH = `${ROUTES.GOVERNANCE.DREPS}/:drepId`;

const baseEntry: AppDRepDirectoryEntry = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  drepActivity: 34,
  drepId: DREP_ID,
  status: 'active',
  votingPower: new BigNumber('23137980123456'),
};

const buildGovernanceStore = (overrides: Record<string, unknown> = {}) => ({
  drepIndex: new Map([[DREP_ID, baseEntry]]),
  drepList: [baseEntry],
  error: null,
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  votingPowerState: VotingPowerEnrichState.Loaded,
  ...overrides,
});

const renderPage = ({
  governanceOverrides = {},
  isNodeInSync = true,
  locale = 'en-US',
  locationState,
  syncProgress = 100,
}: {
  governanceOverrides?: Record<string, unknown>;
  isNodeInSync?: boolean;
  locale?: string;
  locationState?: Record<string, unknown>;
  syncProgress?: number | null;
} = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore(governanceOverrides);
  const history = createMemoryHistory({
    initialEntries: [
      {
        pathname: `${ROUTES.GOVERNANCE.DREPS}/${DREP_ID}`,
        state: locationState,
      },
    ],
  });
  const pushSpy = jest.spyOn(history, 'push');
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  const view = render(
    <Provider stores={{ governance, networkStatus } as any}>
      <IntlProvider locale={locale} messages={messages}>
        <Router history={history}>
          <Route path={DETAIL_PATH} component={DRepDetailPage} />
        </Router>
      </IntlProvider>
    </Provider>
  );
  return { governance, history, networkStatus, pushSpy, ...view };
};

describe('DRepDetailPage', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('renders the on-chain fields for a loaded entry', () => {
    renderPage();

    expect(screen.getByText('!!!DRep detail')).toBeInTheDocument();
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.getByText('!!!34 epochs')).toBeInTheDocument();
    expect(screen.getByText('₳ 23,137,980.123456')).toBeInTheDocument();
    expect(
      screen.getByText('!!!(23,137,980,123,456 lovelace)')
    ).toBeInTheDocument();
    expect(
      screen.getByText('!!!Vote positions are not available in this version.')
    ).toBeInTheDocument();
  });

  it('renders the anchor presence with the on-chain anchor reference label', () => {
    renderPage();

    expect(
      screen.getByText('https://governance-preview.example.org/dreps/1.json')
    ).toBeInTheDocument();
    expect(screen.getByText(baseEntry.anchor!.hash)).toBeInTheDocument();
    expect(
      screen.getByText('!!!On-chain anchor reference')
    ).toBeInTheDocument();
    // The anchor URL renders as inert text, never inside an anchor element.
    expect(
      screen
        .getByText('https://governance-preview.example.org/dreps/1.json')
        .closest('a')
    ).toBeNull();
  });

  it('renders the anchor-absent message when no anchor is recorded', () => {
    renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, anchor: null }]]),
      },
    });

    expect(
      screen.getByText('!!!No anchor is recorded on-chain for this DRep.')
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!On-chain anchor reference')
    ).not.toBeInTheDocument();
  });

  it('shows — with the unavailable tooltip when stake enrichment failed', () => {
    renderPage({
      governanceOverrides: {
        drepIndex: new Map([[DREP_ID, { ...baseEntry, votingPower: null }]]),
        votingPowerState: VotingPowerEnrichState.Failed,
      },
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Stake distribution unavailable this refresh.'
    );
  });

  it('refreshes on mount from an empty Idle store and shows the loading state', () => {
    const { governance } = renderPage({
      governanceOverrides: {
        drepIndex: new Map(),
        drepList: [],
        refreshState: GovernanceRefreshState.Idle,
      },
    });

    expect(governance.refresh).toHaveBeenCalledTimes(1);
    expect(screen.getByText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(
      screen.queryByText(/was not found in the latest on-chain data/)
    ).not.toBeInTheDocument();
  });

  it('shows the inline not-found error with a working Back to directory link', () => {
    const { pushSpy } = renderPage({
      governanceOverrides: { drepIndex: new Map() },
      locationState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    expect(
      screen.getByText(
        '!!!This DRep was not found in the latest on-chain data.'
      )
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Back to directory'));

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.GOVERNANCE.DREPS,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
  });

  it('forwards inherited state plus the byte-equal id on Select for delegation', () => {
    const { pushSpy } = renderPage({
      locationState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      },
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.VOTING.GOVERNANCE,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedDRepId: DREP_ID,
        selectedWalletId: 'wallet-1',
        voteType: 'drep',
      })
    );
  });

  it('falls back to the governance form route when no state was inherited', () => {
    const { pushSpy } = renderPage();

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.VOTING.GOVERNANCE,
      expect.objectContaining({ selectedDRepId: DREP_ID })
    );
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

  it('shows the copied confirmation after the copy button is clicked', async () => {
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage();

      fireEvent.click(screen.getByRole('button', { name: '!!!Copy DRep ID' }));

      expect(await screen.findByText('!!!DRep ID copied')).toBeInTheDocument();
      expect(writeText).toHaveBeenCalledWith(DREP_ID);
    } finally {
      delete (navigator as any).clipboard;
    }
  });

  it('renders the detail field labels in ja-JP', () => {
    renderPage({ locale: 'ja-JP' });

    expect(screen.getByText('!!!DRep詳細')).toBeInTheDocument();
    expect(screen.getByText('!!!ステータス')).toBeInTheDocument();
    expect(screen.getByText('!!!アンカー')).toBeInTheDocument();
    expect(screen.getByText('!!!34エポック')).toBeInTheDocument();
  });

  it('renders the category badge in the detail header (snapshot)', () => {
    renderPage();

    // baseEntry: anchor present, drepActivity 34 -> Primary.
    expect(
      screen.getByText('!!!Primary').closest('span[title]')
    ).toMatchSnapshot();
  });
});
