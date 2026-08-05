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
import { logger } from '../../utils/logging';
import { HwDeviceStatuses } from '../../domains/Wallet';
import type { HwDeviceStatus } from '../../domains/Wallet';
import { GovernanceRefreshState } from '../../stores/GovernanceStore';
import type { DelegationNavState } from '../../stores/GovernanceStore';
import VotingGovernancePage from './VotingGovernancePage';
import DRepDirectoryPage from '../governance/DRepDirectoryPage';
import DRepDetailPage from '../governance/DRepDetailPage';

// jsdom's Uint8Array constructor lives in a different realm than Node's
// Buffer, so the SDK's bech32 encoder rejects Buffer payloads; point the
// suite's global at Node's realm (decode paths are unaffected).
(global as { Uint8Array: unknown }).Uint8Array = Object.getPrototypeOf(
  Buffer.prototype
).constructor;

// The wallet dropdown is react-polymorph-heavy and is mocked to expose
// onChange so a wallet selection can be driven in tests.
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

const drepEntry = {
  anchor: null,
  verifiedName: null,
  doNotList: false,
  drepActivity: 12,
  drepId: VALID_DREP_ID,
  status: 'active' as const,
  votingPower: new BigNumber('23137980123456'),
};

const drepDetail = {
  ...drepEntry,
  metadata: null,
};

const VALID_DREP_ID_UPPERCASE =
  'DREP1YGQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ7VLC9N';

const currentVoteForValidDRep = {
  kind: 'drep' as const,
  drep: {
    raw: VALID_DREP_ID,
    cip129: VALID_DREP_ID,
    cip105: 'drep_vkh1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq667pyd',
    credentialHex: '00000000000000000000000000000000000000000000000000000000',
    credentialType: 'key' as const,
  },
  source: 'onchain' as const,
};

const votingSoftwareWallet = {
  ...softwareWallet,
  currentVote: currentVoteForValidDRep,
};

const votingHardwareWallet = {
  ...hardwareWallet,
  currentVote: currentVoteForValidDRep,
};

type StoreOverrides = {
  hwDeviceStatus?: HwDeviceStatus;
  isTrezor?: boolean;
  wallets?: any[];
  delegationNavState?: DelegationNavState | null;
};

const buildStores = ({
  hwDeviceStatus = HwDeviceStatuses.READY,
  isTrezor = false,
  wallets = [softwareWallet],
  delegationNavState = null,
}: StoreOverrides = {}) => {
  const governance = {
    error: null,
    favoriteDRepIds: new Set<string>(),
    fetchDRep: jest.fn().mockResolvedValue(drepDetail),
    fetchSuggestedDReps: jest.fn(),
    lastFetchedAt: Date.now() - 60_000,
    loadAllDReps: jest.fn(),
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
    allDReps: [drepEntry],
    allDRepsRefreshState: GovernanceRefreshState.Idle,
    suggestedDReps: [drepEntry],
    toggleFavorite: jest.fn(),
    delegationNavState: delegationNavState as DelegationNavState | null,
    setDelegationNavState: jest.fn(),
  };
  // Wire setDelegationNavState to actually mutate delegationNavState so that
  // route-change re-mounts read the updated value.
  governance.setDelegationNavState = jest.fn(
    (state: DelegationNavState | null) => {
      governance.delegationNavState = state;
    }
  );
  return {
    app: {
      currentRoute: ROUTES.VOTING.GOVERNANCE,
      openExternalLink: jest.fn(),
    },
    governance,
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
  };
};

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

const openConfirmation = async (
  drepId: string,
  storeOverrides: StoreOverrides = {},
  delegationNavStateExtras: Partial<DelegationNavState> = {}
) => {
  const flow = renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
    ...storeOverrides,
    delegationNavState: {
      selectedDRepId: drepId,
      selectedWalletId: WALLET_ID,
      voteType: 'drep',
      ...delegationNavStateExtras,
    },
  });
  fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
  await screen.findByText('Confirm Transaction');
  return flow;
};

describe('DRep selection handoff via GovernanceStore.delegationNavState', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('Browse DReps sets delegationNavState with { from, selectedWalletId, voteType } and navigates', () => {
    const { history, stores } = renderFlow(
      [{ pathname: ROUTES.VOTING.GOVERNANCE }],
      { delegationNavState: { selectedWalletId: WALLET_ID, voteType: 'drep' } }
    );

    fireEvent.click(screen.getByText('!!!Browse DReps'));

    expect(stores.governance.setDelegationNavState).toHaveBeenCalledWith(
      expect.objectContaining({
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      })
    );
    expect(history.location.pathname).toBe(ROUTES.GOVERNANCE.DREPS);
  });

  it('list-row Select returns to the form and restores wallet, vote type, and DRep ID', () => {
    renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      delegationNavState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      },
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByText('!!!Delegate to')).toBeInTheDocument();
    expect(screen.getByLabelText(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('two-hop Form → Directory → Detail → Form restores wallet + vote type and pre-fills the ID', async () => {
    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      delegationNavState: { selectedWalletId: WALLET_ID, voteType: 'drep' },
    });

    fireEvent.click(screen.getByText('!!!Browse DReps'));
    fireEvent.click(screen.getByRole('button', { name: '!!!View details' }));
    // Wait for fetchDRep to resolve so the detail page transitions out of Loading.
    await act(async () => {});
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByText('!!!Delegate to')).toBeInTheDocument();
    expect(screen.getByLabelText(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('View details navigates to the detail path without modifying delegationNavState', () => {
    const { history, stores } = renderFlow(
      [{ pathname: ROUTES.GOVERNANCE.DREPS }],
      {
        delegationNavState: {
          from: ROUTES.VOTING.GOVERNANCE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      }
    );

    fireEvent.click(screen.getByRole('button', { name: '!!!View details' }));

    expect(history.location.pathname).toBe(
      `${ROUTES.GOVERNANCE.DREPS}/${VALID_DREP_ID}`
    );
    // delegationNavState is not touched by handleViewDetails — the context is
    // already in the store for the detail page to inherit.
    expect(stores.governance.setDelegationNavState).not.toHaveBeenCalled();
  });

  it('propagates the selected DRep ID byte-for-byte: row select → confirmation → delegateVotes payload', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      delegationNavState: {
        from: ROUTES.VOTING.GOVERNANCE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      },
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    // The confirmation renders the selected ID itself, byte-equal.
    expect(screen.getAllByText(VALID_DREP_ID)[0].textContent).toBe(
      VALID_DREP_ID
    );

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

describe('Hardware-wallet delegate flow via GovernanceStore.delegationNavState handoff', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  const hwNavState: DelegationNavState = {
    from: ROUTES.VOTING.GOVERNANCE,
    selectedWalletId: HW_WALLET_ID,
    voteType: 'drep',
  };

  it('propagates the selected DRep ID byte-for-byte into the HW signing payload (Ledger)', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      wallets: [hardwareWallet],
      delegationNavState: hwNavState,
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(screen.getAllByText(VALID_DREP_ID)[0].textContent).toBe(
      VALID_DREP_ID
    );
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
    renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      wallets: [hardwareWallet],
      delegationNavState: hwNavState,
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
  });

  it('applies the Trezor status treatment for Trezor devices', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      isTrezor: true,
      wallets: [hardwareWallet],
      delegationNavState: hwNavState,
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

  const deviceStates: Array<[HwDeviceStatus, RegExp]> = [
    [
      HwDeviceStatuses.CONNECTING_FAILED,
      /Disconnect and reconnect your hardware wallet/,
    ],
    [HwDeviceStatuses.CONNECTING, /enter your PIN to unlock it/],
    [
      HwDeviceStatuses.LAUNCHING_CARDANO_APP,
      /Launch Cardano application on your device/,
    ],
  ];

  it('renders the current delegation with no device connected and blocks the same-vote submit', () => {
    const { stores } = renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      hwDeviceStatus: HwDeviceStatuses.CONNECTING_FAILED,
      wallets: [votingHardwareWallet],
      delegationNavState: {
        selectedWalletId: HW_WALLET_ID,
        voteType: 'drep',
        selectedDRepId: VALID_DREP_ID,
      },
    });

    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    expect(
      screen.getByText('!!!This wallet already votes for this DRep.')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
    expect(stores.voting.initializeVPDelegationTx).not.toHaveBeenCalled();
  });

  it.each(deviceStates)(
    'surfaces the %s device state in the confirmation dialog and keeps Confirm disabled',
    async (hwDeviceStatus, expectedCopy) => {
      renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
        hwDeviceStatus,
        wallets: [hardwareWallet],
        delegationNavState: hwNavState,
      });

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Select for delegation' })
      );
      fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

      await screen.findByText('Confirm Transaction');
      expect(screen.getByText(expectedCopy)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    }
  );
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

  it('passes the hash-guarded verified name and its anchor host to the dialog', async () => {
    const { stores } = await openConfirmation(
      VALID_DREP_ID,
      {},
      {
        selectedDRepVerifiedName: 'Daedalus Test DRep',
        selectedDRepAnchorUrl:
          'https://raw.githubusercontent.com/example/drep.jsonld',
      }
    );

    expect(mockDialogProps[mockDialogProps.length - 1].verifiedName).toEqual({
      host: 'raw.githubusercontent.com',
      name: 'Daedalus Test DRep',
    });
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: VALID_DREP_ID })
    );
  });

  it('passes a null verified name when the entry carries none', async () => {
    await openConfirmation(
      VALID_DREP_ID,
      {},
      {
        selectedDRepVerifiedName: null,
        selectedDRepAnchorUrl:
          'https://raw.githubusercontent.com/example/drep.jsonld',
      }
    );

    expect(mockDialogProps[mockDialogProps.length - 1].verifiedName).toBeNull();
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
    'verifiedName',
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

describe('Auto-favorite current delegation DRep', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('calls toggleFavorite with the delegated DRep id when a wallet with a DRep vote is selected', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingSoftwareWallet],
    });

    fireEvent.click(screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`));
    await act(async () => {});

    expect(stores.governance.toggleFavorite).toHaveBeenCalledWith(
      VALID_DREP_ID
    );
  });

  it('does not call toggleFavorite when the delegated DRep is already in favorites', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingSoftwareWallet],
    });
    stores.governance.favoriteDRepIds.add(VALID_DREP_ID);

    fireEvent.click(screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`));
    await act(async () => {});

    expect(stores.governance.toggleFavorite).not.toHaveBeenCalled();
  });
});

describe('Current-vote enrichment in the delegation form', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('shows the current delegation and disables submit while the form matches it', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: VALID_DREP_ID,
      },
    });
    await act(async () => {});

    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    expect(screen.getByText('!!!Expiring in 12 epochs')).toBeInTheDocument();
    expect(
      screen.getByText(
        "!!!This DRep's voting power will lapse in 12 epochs — consider re-delegating."
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Delegate to')).toBeInTheDocument();
    expect(
      screen.getByText('!!!This wallet already votes for this DRep.')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
    expect(stores.voting.initializeVPDelegationTx).not.toHaveBeenCalled();
  });

  it('resolves the directory entry for a CIP-105 delegation through its CIP-129 form', async () => {
    const CIP105_DREP_ID =
      'drep_vkh1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq667pyd';

    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [
        {
          ...softwareWallet,
          currentVote: {
            ...currentVoteForValidDRep,
            drep: { ...currentVoteForValidDRep.drep, raw: CIP105_DREP_ID },
          },
        },
      ],
      delegationNavState: { selectedWalletId: WALLET_ID, voteType: 'drep' },
    });
    await act(async () => {});

    expect(screen.getByText('!!!Expiring in 12 epochs')).toBeInTheDocument();
    expect(screen.queryByText('!!!DRep status is loading.')).toBeNull();
    expect(screen.getByLabelText(CIP105_DREP_ID)).toBeInTheDocument();
  });

  it('treats a target differing only in bech32 letter case as the current vote', () => {
    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: VALID_DREP_ID_UPPERCASE,
      },
    });

    expect(
      screen.getByText('!!!This wallet already votes for this DRep.')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
  });

  it('re-enables submit and opens the confirmation dialog when the target changes', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: OTHER_DREP_ID,
      },
    });

    const submit = screen.getByRole('button', { name: 'Submit' });
    expect(submit).not.toBeDisabled();
    fireEvent.click(submit);

    await screen.findByText('Confirm Transaction');
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: OTHER_DREP_ID })
    );
    expect(screen.getAllByText(OTHER_DREP_ID)[0].textContent).toBe(
      OTHER_DREP_ID
    );
  });

  it('keeps the vote target out of renderer logger payloads across the flow', async () => {
    const spies = [
      jest.spyOn(logger, 'debug').mockImplementation(() => undefined),
      jest.spyOn(logger, 'info').mockImplementation(() => undefined),
      jest.spyOn(logger, 'warn').mockImplementation(() => undefined),
      jest.spyOn(logger, 'error').mockImplementation(() => undefined),
    ];

    renderFlow([{ pathname: ROUTES.VOTING.GOVERNANCE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: OTHER_DREP_ID,
      },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
    await screen.findByText('Confirm Transaction');

    const logged = JSON.stringify(spies.map((spy) => spy.mock.calls));
    expect(logged).not.toContain(VALID_DREP_ID);
    expect(logged).not.toContain(VALID_DREP_ID_UPPERCASE);
    expect(logged).not.toContain(OTHER_DREP_ID);
    expect(logged).not.toContain('drep_vkh');
    expect(logged).not.toContain('drep_script');
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain('no_confidence');
  });
});
