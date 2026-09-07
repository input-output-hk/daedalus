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
import { GovernanceRefreshState } from '../../stores/GovernanceStore';
import type { DelegationNavState } from '../../stores/GovernanceStore';
import { DEFAULT_DREP_COHORT_CRITERIA } from '../../components/governance/_shared/drepCohort';
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

const drepEntry = {
  anchor: null,
  verifiedName: null,
  doNotList: false,
  drepActivity: 5,
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
  currentDRep: currentVoteForValidDRep,
};

type StoreOverrides = {
  wallets?: any[];
  delegationNavState?: DelegationNavState | null;
};

const buildStores = ({
  wallets = [softwareWallet],
  delegationNavState = null,
}: StoreOverrides = {}) => {
  const governance = {
    error: null,
    favoriteDRepIds: new Set<string>(),
    fetchDRep: jest.fn().mockResolvedValue(drepDetail),
    lastFetchedAt: Date.now() - 60_000,
    loadAllDReps: jest.fn(),
    refresh: jest.fn(),
    refreshState: GovernanceRefreshState.Loaded,
    allDReps: [drepEntry],
    suggestedDReps: [drepEntry],
    cohortCriteria: DEFAULT_DREP_COHORT_CRITERIA,
    cohortPool: {
      entries: [drepEntry],
      criteria: DEFAULT_DREP_COHORT_CRITERIA,
      relaxed: [],
      strictSize: 1,
    },
    setCohortCriteria: jest.fn(),
    rerollCohort: jest.fn(),
    lookupDRep: jest.fn(() => drepEntry),
    toggleFavorite: jest.fn(),
    favoriteEntries: [],
    ensureFavorites: jest.fn().mockResolvedValue(undefined),
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
      currentRoute: ROUTES.GOVERNANCE.DELEGATE,
      openExternalLink: jest.fn(),
    },
    governance,
    hardwareWallets: {},
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
    // allWallets is the Shelley-only collection the governance pages read:
    // a Byron wallet cannot delegate voting power.
    wallets: { all: wallets, allWallets: wallets },
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
              path={ROUTES.GOVERNANCE.DELEGATE}
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
  const flow = renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
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
      [{ pathname: ROUTES.GOVERNANCE.DELEGATE }],
      { delegationNavState: { selectedWalletId: WALLET_ID, voteType: 'drep' } }
    );

    fireEvent.click(screen.getByText('Browse DReps'));

    expect(stores.governance.setDelegationNavState).toHaveBeenCalledWith(
      expect.objectContaining({
        from: ROUTES.GOVERNANCE.DELEGATE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      })
    );
    expect(history.location.pathname).toBe(ROUTES.GOVERNANCE.DREPS);
  });

  it('list-row Select returns to the form and restores wallet, vote type, and DRep ID', () => {
    renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      delegationNavState: {
        from: ROUTES.GOVERNANCE.DELEGATE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByText('Delegate To')).toBeInTheDocument();
    expect(screen.getByLabelText(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('two-hop Form → Directory → Detail → Form restores wallet + vote type and pre-fills the ID', async () => {
    renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
      delegationNavState: { selectedWalletId: WALLET_ID, voteType: 'drep' },
    });

    fireEvent.click(screen.getByText('Browse DReps'));
    fireEvent.click(screen.getByRole('button', { name: 'View details' }));
    // Wait for fetchDRep to resolve so the detail page transitions out of Loading.
    await act(async () => {});
    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(screen.getByTestId('wallets-dropdown')).toHaveTextContent(WALLET_ID);
    expect(screen.getByText('Delegate To')).toBeInTheDocument();
    expect(screen.getByLabelText(VALID_DREP_ID)).toBeInTheDocument();
  });

  it('View details navigates to the detail path without modifying delegationNavState', () => {
    const { history, stores } = renderFlow(
      [{ pathname: ROUTES.GOVERNANCE.DREPS }],
      {
        delegationNavState: {
          from: ROUTES.GOVERNANCE.DELEGATE,
          selectedWalletId: WALLET_ID,
          voteType: 'drep',
        },
      }
    );

    fireEvent.click(screen.getByRole('button', { name: 'View details' }));

    expect(history.location.pathname).toBe(
      `${ROUTES.GOVERNANCE.DREPS}/${VALID_DREP_ID}`
    );
    // delegationNavState is not touched by handleViewDetails — the context is
    // already in the store for the detail page to inherit.
    expect(stores.governance.setDelegationNavState).not.toHaveBeenCalled();
  });

  it('propagates the selected DRep ID byte-for-byte to delegateVotes', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DREPS }], {
      delegationNavState: {
        from: ROUTES.GOVERNANCE.DELEGATE,
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
      },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledTimes(1)
    );
    expect(stores.voting.delegateVotes).toHaveBeenCalledWith({
      chosenOption: VALID_DREP_ID,
      wallet: expect.objectContaining({ id: WALLET_ID }),
    });
  });
});

describe('Auto-favorite current delegation DRep', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('calls toggleFavorite with the delegated DRep id when a wallet with a DRep vote is selected', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
      wallets: [votingSoftwareWallet],
    });

    fireEvent.click(screen.getByTestId(`wallets-dropdown-option-${WALLET_ID}`));
    await act(async () => {});

    expect(stores.governance.toggleFavorite).toHaveBeenCalledWith(
      VALID_DREP_ID
    );
  });

  it('does not call toggleFavorite when the delegated DRep is already in favorites', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
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
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: VALID_DREP_ID,
      },
    });
    await act(async () => {});

    expect(screen.getByText('Currently Delegated To')).toBeInTheDocument();
    expect(screen.getByText('Inactive Soon')).toBeInTheDocument();
    expect(screen.getByText('Delegate To')).toBeInTheDocument();
    expect(
      screen.getByText(/already delegates to this choice/)
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
    expect(stores.voting.initializeVPDelegationTx).not.toHaveBeenCalled();
  });

  it('resolves the directory entry for a CIP-105 delegation through its CIP-129 form', async () => {
    const CIP105_DREP_ID =
      'drep_vkh1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq667pyd';

    renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
      wallets: [
        {
          ...softwareWallet,
          currentDRep: {
            ...currentVoteForValidDRep,
            drep: { ...currentVoteForValidDRep.drep, raw: CIP105_DREP_ID },
          },
        },
      ],
      delegationNavState: { selectedWalletId: WALLET_ID, voteType: 'drep' },
    });
    await act(async () => {});

    expect(screen.getByText('Inactive Soon')).toBeInTheDocument();
    expect(screen.queryByText('DRep status is loading.')).toBeNull();
    expect(screen.getByLabelText(CIP105_DREP_ID)).toBeInTheDocument();
  });

  it('treats a target differing only in bech32 letter case as the current vote', () => {
    renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: VALID_DREP_ID_UPPERCASE,
      },
    });

    expect(
      screen.getByText(/already delegates to this choice/)
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Submit' })).toBeDisabled();
  });

  it('re-enables submit and delegates when the target changes', async () => {
    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
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

    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledWith({
        chosenOption: OTHER_DREP_ID,
        wallet: expect.objectContaining({ id: WALLET_ID }),
      })
    );
  });

  it('keeps the vote target out of renderer logger payloads across the flow', async () => {
    const spies = [
      jest.spyOn(logger, 'debug').mockImplementation(() => undefined),
      jest.spyOn(logger, 'info').mockImplementation(() => undefined),
      jest.spyOn(logger, 'warn').mockImplementation(() => undefined),
      jest.spyOn(logger, 'error').mockImplementation(() => undefined),
    ];

    const { stores } = renderFlow([{ pathname: ROUTES.GOVERNANCE.DELEGATE }], {
      wallets: [votingSoftwareWallet],
      delegationNavState: {
        selectedWalletId: WALLET_ID,
        voteType: 'drep',
        selectedDRepId: OTHER_DREP_ID,
      },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledTimes(1)
    );

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
