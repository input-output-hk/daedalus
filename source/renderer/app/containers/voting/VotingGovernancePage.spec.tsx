import React from 'react';
import BigNumber from 'bignumber.js';
import { Provider } from 'mobx-react';
import { Route, Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
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
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../stores/GovernanceStore';
import VotingGovernancePage from './VotingGovernancePage';
import DRepDirectoryPage from '../governance/DRepDirectoryPage';
import DRepDetailPage from '../governance/DRepDetailPage';

// The wallet and vote-type dropdowns are react-polymorph-heavy, so both are
// mocked: the vote-type mock renders only the value the flow asserts, and the
// wallet mock also exposes onChange so a wallet selection can be driven.
jest.mock('../../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock(props: {
    onChange: (walletId: string) => void;
    value: string | null;
    wallets: Array<{ id: string }>;
  }) {
    return (
      <div data-testid="wallets-dropdown">
        {props.value || 'none'}
        {props.wallets.map((wallet) => (
          <button
            data-testid={`wallets-dropdown-option-${wallet.id}`}
            key={wallet.id}
            onClick={() => props.onChange(wallet.id)}
            type="button"
          />
        ))}
      </div>
    );
  };
});

jest.mock('../../components/widgets/forms/ItemsDropdown', () => {
  return function ItemsDropdownMock(props: { value: string }) {
    return <div data-testid="vote-type-dropdown">{props.value}</div>;
  };
});

const mockDialogProps: Array<Record<string, unknown>> = [];

// The recorder wraps the real dialog, so the rendered DOM the other flow tests
// assert on is unchanged; only the prop object is captured.
jest.mock(
  '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog',
  () => {
    const actual = jest.requireActual(
      '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog'
    );
    const { createElement } = jest.requireActual('react');
    return {
      __esModule: true,
      default: function DialogPropsRecorder(props: Record<string, unknown>) {
        mockDialogProps.push(props);
        return createElement(actual.default, props);
      },
    };
  }
);

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';
const OTHER_DREP_ID =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
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

const VOTING_WALLET_ID = 'voting-wallet-1';

const votingWallet = {
  id: VOTING_WALLET_ID,
  name: 'Voting Wallet',
  isHardwareWallet: false,
  currentVote: {
    kind: 'drep',
    drep: { raw: VALID_DREP_ID, credentialType: 'key' },
    source: 'onchain',
  },
} as any;

const abstainWallet = {
  id: 'abstain-wallet-1',
  name: 'Abstain Wallet',
  isHardwareWallet: false,
  currentVote: { kind: 'abstain' },
} as any;

const drepEntry = {
  anchor: null,
  drepActivity: 12,
  drepId: VALID_DREP_ID,
  status: 'active' as const,
  votingPower: new BigNumber('23137980123456'),
};

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
    displayedDRepList: [drepEntry],
    drepIndex: new Map([[VALID_DREP_ID, drepEntry]]),
    drepList: [drepEntry],
    error: null,
    favoriteDRepIds: new Set<string>(),
    isCohortActive: false,
    lastFetchedAt: Date.now() - 60_000,
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
    reshuffleCohort: jest.fn(),
    showAllList: [drepEntry],
    toggleFavorite: jest.fn(),
    top35DRepIds: new Set<string>(),
    votingPowerState: VotingPowerEnrichState.Loaded,
  },
  hardwareWallets: {
    checkIsTrezorByWalletId: jest.fn(() => isTrezor),
    hwDeviceStatus,
  },
  networkStatus: {
    isNodeInSync: true,
    isSynced: true,
    syncPercentage: 100,
    syncProgress: 100,
  },
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
  const tree = (currentStores: ReturnType<typeof buildStores>) => (
    <Provider stores={currentStores as any} actions={actions as any}>
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
            <Route
              path={ROUTES.GOVERNANCE.DREP_DETAIL}
              component={DRepDetailPage}
            />
          </Router>
        </IntlProvider>
      </ThemeProvider>
    </Provider>
  );
  const { rerender } = render(tree(stores));
  return {
    actions,
    history,
    pushSpy,
    rerenderWithWallets: (wallets: any[]) => {
      stores.wallets.all = wallets;
      rerender(tree(stores));
    },
    stores,
  };
};

const openConfirmation = async (drepId: string) => {
  const flow = renderFlow([
    {
      pathname: ROUTES.VOTING.GOVERNANCE,
      state: {
        selectedDRepId: drepId,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      },
    },
  ]);
  fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
  await screen.findByText('Confirm Transaction');
  return flow;
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
    renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType: 'drep' },
      },
    ]);

    fireEvent.click(screen.getByText('!!!Browse DReps'));
    fireEvent.click(screen.getByRole('button', { name: '!!!View details' }));
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('View details forwards { from, selectedWalletId, voteType } without selectedDRepId', () => {
    const { history, pushSpy } = renderFlow([
      {
        pathname: ROUTES.GOVERNANCE.DREPS,
        state: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      },
    ]);

    fireEvent.click(screen.getByRole('button', { name: '!!!View details' }));

    expect(history.location.pathname).toBe(
      `${ROUTES.GOVERNANCE.DREPS}/${VALID_DREP_ID}`
    );
    expect(pushSpy).toHaveBeenCalledWith(
      `${ROUTES.GOVERNANCE.DREPS}/${VALID_DREP_ID}`,
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      })
    );
    const forwardedState = pushSpy.mock.calls[0][1] as Record<string, unknown>;
    expect(forwardedState.selectedDRepId).toBeUndefined();
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

describe('Delegation form pre-fill from the selected wallet', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('seeds the DRep input from the wallet current on-chain vote', () => {
    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingWallet],
    });

    fireEvent.click(
      screen.getByTestId(`wallets-dropdown-option-${VOTING_WALLET_ID}`)
    );

    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('seeds the vote type and no DRep id from a sentinel on-chain vote', () => {
    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [abstainWallet],
    });

    fireEvent.click(
      screen.getByTestId('wallets-dropdown-option-abstain-wallet-1')
    );

    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent(
      'abstain'
    );
    expect(screen.queryByDisplayValue(VALID_DREP_ID)).toBeNull();
  });

  it('keeps the inherited directory id byte-identical when the wallet is picked afterwards', () => {
    renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedDRepId: VALID_DREP_ID },
      },
    ]);

    fireEvent.click(screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`));

    const drepInput = screen.getByDisplayValue(VALID_DREP_ID);
    expect(drepInput).toBeInTheDocument();
    expect((drepInput as HTMLInputElement).value).toBe(VALID_DREP_ID);
  });

  it('re-seeds an untouched form when a refreshed snapshot carries a new vote', () => {
    const { rerenderWithWallets } = renderFlow([
      { pathname: ROUTES.VOTING.GOVERNANCE },
    ]);

    fireEvent.click(screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`));
    expect(screen.queryByDisplayValue(VALID_DREP_ID)).toBeNull();

    rerenderWithWallets([
      {
        ...softwareWallet,
        currentVote: {
          kind: 'drep',
          drep: { raw: VALID_DREP_ID, credentialType: 'key' },
          source: 'onchain',
        },
      },
    ]);

    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('leaves a typed DRep id untouched when a refreshed snapshot carries a new vote', () => {
    const typedDRepId = 'drep1typedbytheuser';
    const { rerenderWithWallets } = renderFlow(
      [{ pathname: ROUTES.VOTING.GOVERNANCE }],
      { wallets: [votingWallet] }
    );

    fireEvent.click(
      screen.getByTestId(`wallets-dropdown-option-${VOTING_WALLET_ID}`)
    );
    fireEvent.change(screen.getByDisplayValue(VALID_DREP_ID), {
      target: { value: typedDRepId },
    });

    rerenderWithWallets([
      {
        ...votingWallet,
        currentVote: {
          kind: 'drep',
          drep: { raw: OTHER_DREP_ID, credentialType: 'key' },
          source: 'onchain',
        },
      },
    ]);

    expect(screen.getByDisplayValue(typedDRepId)).toBeInTheDocument();
    expect(screen.queryByDisplayValue(OTHER_DREP_ID)).toBeNull();
  });

  it('prefers the wallet current vote over the inherited vote type and DRep id on mount', () => {
    renderFlow(
      [
        {
          pathname: ROUTES.VOTING.GOVERNANCE,
          state: {
            selectedWalletId: VOTING_WALLET_ID,
            voteType: 'abstain',
            selectedDRepId: OTHER_DREP_ID,
          },
        },
      ],
      { wallets: [votingWallet] }
    );

    expect(screen.getByTestId('vote-type-dropdown')).toHaveTextContent('drep');
    expect(screen.getByDisplayValue(VALID_DREP_ID)).toBeInTheDocument();
  });
});

describe('Confirmation dialog identity derivation', () => {
  const SCRIPT_DREP_ID =
    'drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd';
  const LEGACY_DREP_ID =
    'drep1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg4v077e';

  beforeEach(() => {
    mockDialogProps.length = 0;
  });

  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('classifies a CIP-129 script DRep by its header byte', async () => {
    await openConfirmation(SCRIPT_DREP_ID);

    const props = mockDialogProps[mockDialogProps.length - 1];
    expect(props.drepIdentity).toEqual(
      expect.objectContaining({
        credentialType: 'script',
        raw: SCRIPT_DREP_ID,
      })
    );
    expect(props.chosenOption).toBe(SCRIPT_DREP_ID);
  });

  it('passes a null identity for an id the decoder rejects and still submits it byte-for-byte', async () => {
    const { stores } = await openConfirmation(LEGACY_DREP_ID);

    expect(mockDialogProps[mockDialogProps.length - 1].drepIdentity).toBeNull();
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: LEGACY_DREP_ID })
    );
  });
});

describe('Confirmation dialog prop contract', () => {
  const EXPECTED_DIALOG_PROPS = [
    'chosenOption',
    'drepIdentity',
    'fees',
    'hwDeviceStatus',
    'isTrezor',
    'onClose',
    'onExternalLinkClick',
    'onSubmit',
    'redirectToWallet',
    'selectedWallet',
  ];

  beforeEach(() => {
    mockDialogProps.length = 0;
  });

  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('hands the dialog exactly the current-target prop set', async () => {
    await openConfirmation(VALID_DREP_ID);

    const props = mockDialogProps[mockDialogProps.length - 1];
    expect(Object.keys(props).sort()).toEqual(
      [...EXPECTED_DIALOG_PROPS].sort()
    );
  });

  it('passes no historical vote-target prop', async () => {
    await openConfirmation(VALID_DREP_ID);

    const props = mockDialogProps[mockDialogProps.length - 1];
    ['previousVote', 'newVote', 'previousDRepId', 'currentVote'].forEach(
      (key) => {
        expect(props).not.toHaveProperty(key);
      }
    );
  });
});
