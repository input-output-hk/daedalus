import React from 'react';
import BigNumber from 'bignumber.js';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
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
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../themes/daedalus';
import { themeOverrides } from '../../themes/overrides';
import { ROUTES } from '../../routes-config';
import { HwDeviceStatuses } from '../../domains/Wallet';
import type { HwDeviceStatus } from '../../domains/Wallet';
import { GovernanceRefreshState } from '../../stores/GovernanceStore';
import { pickDelegationFormReturnState } from '../governance/delegationFormState';
import VotingGovernancePage from './VotingGovernancePage';
import DRepDirectoryPage from '../governance/DRepDirectoryPage';

// The wallet and vote-type dropdowns are react-polymorph-heavy; the flow tests
// assert the values they RECEIVE, so plain pass-through mocks are enough.
jest.mock('../../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock(props: { value: string | null }) {
    return <div data-testid="wallets-dropdown">{props.value || 'none'}</div>;
  };
});

jest.mock('../../components/widgets/forms/ItemsDropdown', () => {
  return function ItemsDropdownMock(props: { value: string }) {
    return <div data-testid="vote-type-dropdown">{props.value}</div>;
  };
});

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';
const WALLET_ID = 'wallet-1';

const softwareWallet = {
  id: WALLET_ID,
  name: 'Software Wallet',
  isHardwareWallet: false,
} as any;

const HW_WALLET_ID = 'hw-wallet-1';

const hardwareWallet = {
  id: HW_WALLET_ID,
  name: 'HW Flow Wallet',
  isHardwareWallet: true,
} as any;

const drepEntry = {
  anchor: null,
  drepActivity: 12,
  drepId: VALID_DREP_ID,
  status: 'active' as const,
  votingPower: new BigNumber('23137980123456'),
};

const DETAIL_STUB_PATH = '/governance/dreps/:drepId';

// Test-only stand-in for the future DRep detail route: it forwards the
// inherited { from, selectedWalletId, voteType } plus the route's DRep ID
// back to the form. It is registered only in this harness — production has
// no detail route yet.
function DetailRouteStub({
  history,
  location,
  match,
}: RouteComponentProps<{ drepId: string }>) {
  return (
    <button
      type="button"
      onClick={() => {
        const inherited = pickDelegationFormReturnState(location.state);
        history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE, {
          ...inherited,
          selectedDRepId: match.params.drepId,
        });
      }}
    >
      Stub select for delegation
    </button>
  );
}

type StoreOverrides = {
  hwDeviceStatus?: HwDeviceStatus;
  isTrezor?: boolean;
  wallets?: any[];
};

const buildStores = ({
  hwDeviceStatus = HwDeviceStatuses.READY,
  isTrezor = false,
  wallets = [softwareWallet],
}: StoreOverrides = {}) => ({
  app: {
    currentRoute: ROUTES.VOTING.GOVERNANCE,
    openExternalLink: jest.fn(),
  },
  governance: {
    drepList: [drepEntry],
    error: null,
    lastFetchedAt: Date.now() - 60_000,
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
  },
  hardwareWallets: {
    checkIsTrezorByWalletId: jest.fn(() => isTrezor),
    hwDeviceStatus,
  },
  networkStatus: { isSynced: true, syncPercentage: 100 },
  staking: { getStakePoolById: jest.fn(), stakePools: [] },
  voting: {
    delegateVotes: jest.fn(async () => ({ success: true as const })),
    initializeVPDelegationTx: jest.fn(async () => ({
      fees: new BigNumber('0.174257'),
      success: true as const,
    })),
  },
  wallets: { all: wallets },
});

type InitialEntry = { pathname: string; state?: Record<string, unknown> };

const renderFlow = (
  initialEntries: InitialEntry[],
  storeOverrides: StoreOverrides = {}
) => {
  const history = createMemoryHistory({ initialEntries });
  const pushSpy = jest.spyOn(history, 'push');
  const stores = buildStores(storeOverrides);
  const actions = { router: { goToRoute: { trigger: jest.fn() } } };
  render(
    <Provider stores={stores as any} actions={actions as any}>
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <Router history={history}>
            <Route
              path={ROUTES.VOTING.GOVERNANCE}
              component={VotingGovernancePage}
            />
            <Route
              exact
              path={ROUTES.GOVERNANCE.DREPS}
              component={DRepDirectoryPage}
            />
            <Route path={DETAIL_STUB_PATH} component={DetailRouteStub} />
          </Router>
        </IntlProvider>
      </ThemeProvider>
    </Provider>
  );
  return { actions, history, pushSpy, stores };
};

describe('DRep selection handoff via location.state', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('Browse DReps push carries { from, selectedWalletId, voteType } in location.state', () => {
    const { history, pushSpy } = renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
      },
    ]);

    fireEvent.click(screen.getByText('!!!Browse DReps'));

    expect(pushSpy).toHaveBeenCalledWith(
      ROUTES.GOVERNANCE.DREPS,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      })
    );
    expect(history.location.pathname).toBe(ROUTES.GOVERNANCE.DREPS);
  });

  it('list-row Select returns to the form and restores wallet, vote type, and DRep ID', () => {
    renderFlow([
      {
        pathname: ROUTES.GOVERNANCE.DREPS,
        state: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      },
    ]);

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('two-hop Form → Directory → Detail → Form restores wallet + vote type and pre-fills the ID', () => {
    const { history } = renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
      },
    ]);

    fireEvent.click(screen.getByText('!!!Browse DReps'));

    // Simulate the future "View details" push: the Directory forwards its
    // inherited state toward the detail path via the production picker.
    act(() => {
      history.push(
        `/governance/dreps/${VALID_DREP_ID}`,
        pickDelegationFormReturnState(history.location.state)
      );
    });

    fireEvent.click(screen.getByText('Stub select for delegation'));

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('propagates the selected DRep ID byte-for-byte: row select → confirmation → delegateVotes payload', async () => {
    const { stores } = renderFlow([
      {
        pathname: ROUTES.GOVERNANCE.DREPS,
        state: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      },
    ]);

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    // The confirmation renders the selected ID itself, byte-equal.
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);

    const passwordInput = document.querySelector('input[type="password"]');
    expect(passwordInput).not.toBeNull();
    fireEvent.change(passwordInput as Element, {
      target: { value: 'secret123' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));

    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledTimes(1)
    );
    expect(stores.voting.delegateVotes).toHaveBeenCalledWith(
      expect.objectContaining({
        chosenOption: VALID_DREP_ID,
        passphrase: 'secret123',
      })
    );
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: VALID_DREP_ID })
    );
  });
});

describe('Hardware-wallet delegate flow via location.state handoff', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  const hwEntry = {
    pathname: ROUTES.GOVERNANCE.DREPS,
    state: {
      from: ROUTES.VOTING.GOVERNANCE,
      selectedWalletId: HW_WALLET_ID,
      voteType: 'drep',
    },
  };

  it('propagates the selected DRep ID byte-for-byte into the HW signing payload (Ledger)', async () => {
    const { stores } = renderFlow([hwEntry], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      wallets: [hardwareWallet],
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
    // The HW confirmation collects no passphrase: signing happened on-device.
    expect(document.querySelector('input[type="password"]')).toBeNull();
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({
        chosenOption: VALID_DREP_ID,
        wallet: expect.objectContaining({
          id: HW_WALLET_ID,
          isHardwareWallet: true,
        }),
      })
    );

    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));

    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledTimes(1)
    );
    expect(stores.voting.delegateVotes).toHaveBeenCalledWith(
      expect.objectContaining({
        chosenOption: VALID_DREP_ID,
        passphrase: '',
        wallet: expect.objectContaining({ id: HW_WALLET_ID }),
      })
    );
  });

  it('keeps Confirm disabled until the device reports signing success', async () => {
    renderFlow([hwEntry], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      wallets: [hardwareWallet],
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
  });

  it('applies the Trezor status treatment for Trezor devices', async () => {
    const { stores } = renderFlow([hwEntry], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      isTrezor: true,
      wallets: [hardwareWallet],
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(stores.hardwareWallets.checkIsTrezorByWalletId).toHaveBeenCalledWith(
      HW_WALLET_ID
    );
    expect(screen.getByText('Enter passphrase if needed')).toBeInTheDocument();
  });
});
