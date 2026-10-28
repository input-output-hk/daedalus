import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import VotingStore, {
  FundPhase,
  expectedInitializeVPDelegationTxErrors,
} from './VotingStore';
import type { CatalystFund } from '../api/voting/types';
import { EventCategories, noopAnalyticsTracker } from '../analytics';
import { logger } from '../utils/logging';
import ApiError from '../domains/ApiError';
import translations from '../i18n/locales/en-US.json';
import { daedalusTheme } from '../themes/daedalus';
import { themeOverrides } from '../themes/overrides';
import VotingPowerDelegation from '../components/voting/voting-governance/VotingPowerDelegation';
import { GenericApiError } from '../api/common/errors';
import { _createWalletFromServerData } from '../api/api';
import { loadRecordedExchange } from '../../../../tests/jest/support/walletReplay';

// api.ts builds a global.https agent when request.ts loads; these specs never
// send a request.
jest.mock('../api/utils/request', () => ({ request: jest.fn() }));

beforeEach(() => {
  global.cip30Enabled = true;
});
jest.mock('../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock() {
    return null;
  };
});

jest.mock('../components/widgets/forms/ItemsDropdown', () => {
  return function ItemsDropdownMock() {
    return null;
  };
});

const mockFundInfo = {
  current: {
    startTime: new Date('Jan 20, 2022, 11:00 UTC'),
    endTime: new Date('Feb 3, 2022, 11:00 UTC'),
    resultsTime: new Date('Feb 10, 2022'),
    registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  },
};

describe('VotingStore', () => {
  const api: Api = {
    ada: jest.fn(),
  } as any;
  const actions: ActionsMap = jest.fn() as any;

  const cases = [
    [undefined, null],
    [
      new Date(mockFundInfo.current.registrationSnapshotTime.getTime() - 60000),
      FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.registrationSnapshotTime, FundPhase.SNAPSHOT],
    [
      new Date(mockFundInfo.current.startTime.getTime() - 60000),
      FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.startTime, FundPhase.VOTING],
    [
      new Date(mockFundInfo.current.endTime.getTime() - 60000),
      FundPhase.VOTING,
    ],
    [mockFundInfo.current.endTime, FundPhase.TALLYING],
    [
      new Date(mockFundInfo.current.resultsTime.getTime() - 60000),
      FundPhase.TALLYING,
    ],
    [mockFundInfo.current.resultsTime, FundPhase.RESULTS],
  ];
  const votingStore = new VotingStore(api, actions, noopAnalyticsTracker);

  beforeAll(() => {
    votingStore.catalystFund = mockFundInfo as CatalystFund;
  });

  test.each(cases)(
    `should have correct fund phase for date %s - %s phase`,
    (date: Date, expected: FundPhase) => {
      votingStore._checkFundPhase(date);
      expect(votingStore.fundPhase).toEqual(expected);
    }
  );
});

const CIP129_KEY = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const REWARD_ACCOUNT_PATH = ['1852H', '1815H', '0H', '2', '0'];

const hwWallet = {
  id: 'hw-wallet-1',
  isDelegating: false,
  isHardwareWallet: true,
} as any;

const softwareWallet = {
  id: 'sw-wallet-1',
  isDelegating: false,
  isHardwareWallet: false,
} as any;

const buildAnalytics = () => ({
  disableTracking: jest.fn(),
  enableTracking: jest.fn(),
  sendEvent: jest.fn(),
  sendPageNavigationEvent: jest.fn(),
});

// The vote path asks the stake pool list for nothing: reading it throws.
const noStakePools = () => ({
  get stakePools(): never {
    throw new Error('the vote path read the stake pool list');
  },
});

const buildHardwareWallets = (overrides: Record<string, unknown> = {}) => ({
  selectVoteCoins: jest.fn(async () => ({
    coinSelection: { certificates: [] },
    fee: new BigNumber('0.180989'),
  })),
  updateTxSignRequest: jest.fn(),
  initiateTransaction: jest.fn(async () => undefined),
  _sendMoney: jest.fn(async () => ({ id: 'hw-tx-1' })),
  submitConstructedNativeTransaction: jest.fn(async () => ({ id: 'tx-1' })),
  sendMoneyRequest: { isExecuting: false },
  isTransactionPending: false,
  ...overrides,
});

const buildStore = (
  hardwareWallets: ReturnType<typeof buildHardwareWallets>
) => {
  const api = { ada: { delegateVotes: jest.fn() } };
  const analytics = buildAnalytics();
  const store = new VotingStore(api as any, {} as any, analytics as any);
  store.configure({
    hardwareWallets,
    staking: noStakePools(),
  } as any);
  return { analytics, api, store };
};

describe('VotingStore hardware-wallet delegation branches', () => {
  beforeEach(() => {
    // The renderer logger writes through global.electronLog, which does not
    // exist under Jest; stub it so error-path tests can run and be asserted.
    jest.spyOn(logger, 'error').mockImplementation(() => undefined);
  });

  afterEach(() => {
    jest.restoreAllMocks();
    jest.useRealTimers();
  });

  describe('delegateVotes', () => {
    it('submits the frozen vote through the hardware native approval path', async () => {
      const hardwareWallets = buildHardwareWallets();
      const { analytics, store } = buildStore(hardwareWallets);

      await expect(
        store.delegateVotes({
          chosenOption: CIP129_KEY,
          passphrase: '',
          wallet: hwWallet,
        })
      ).resolves.toEqual({ success: true, transactionId: 'tx-1' });
      expect(
        hardwareWallets.submitConstructedNativeTransaction
      ).toHaveBeenCalledWith({
        walletId: hwWallet.id,
        action: 'drep-delegation',
        data: { encoding: 'base16', vote: CIP129_KEY },
      });
      expect(analytics.sendEvent).toHaveBeenCalledWith(
        EventCategories.VOTING,
        'Casted governance vote',
        'drep'
      );
    });

    it('returns a generic error code and sends no analytics when HW submission fails', async () => {
      const hardwareWallets = buildHardwareWallets({
        submitConstructedNativeTransaction: jest.fn(async () => {
          throw new Error('signing rejected on device');
        }),
      });
      const { analytics, store } = buildStore(hardwareWallets);

      const result = await store.delegateVotes({
        chosenOption: CIP129_KEY,
        passphrase: '',
        wallet: hwWallet,
      });

      expect(result).toEqual({ success: false, errorCode: 'generic' });
      expect(analytics.sendEvent).not.toHaveBeenCalled();
      expect(logger.error).toHaveBeenCalledWith(
        'VotingStore: error while delegating vote with HW',
        expect.objectContaining({ errorCode: 'generic' })
      );
    });
  });
});

const DelegationForm =
  VotingPowerDelegation as unknown as React.ComponentType<any>;

describe('same-vote server error in the delegation form', () => {
  afterEach(cleanup);

  it('renders the server same_vote copy when the wallet has no matching current vote', async () => {
    const wallet = {
      currentDRep: null,
      id: 'sw-wallet-2',
      isHardwareWallet: false,
      name: 'Form Wallet',
    } as any;
    const submitTransaction = jest.fn(async () => ({
      errorCode: 'same_vote' as const,
      success: false as const,
    }));

    render(
      React.createElement(
        ThemeProvider,
        {
          theme: daedalusTheme,
          skins: SimpleSkins,
          variables: SimpleDefaults,
          themeOverrides,
        },
        React.createElement(
          IntlProvider,
          { locale: 'en-US', messages: translations },
          React.createElement(DelegationForm, {
            getStakePoolById: jest.fn(),
            initialFormState: {
              selectedDRepId: CIP129_KEY,
              selectedWalletId: wallet.id,
              voteType: 'drep',
            },
            submitTransaction,
            onBrowseDRepsClick: jest.fn(),
            onExternalLinkClick: jest.fn(),
            onCancel: jest.fn(),
            onSuccess: jest.fn(),
            stakePools: [],
            wallets: [wallet],
          })
        )
      )
    );

    const submit = screen.getByRole('button', { name: 'Submit' });
    expect(submit).not.toBeDisabled();
    fireEvent.click(submit);

    expect(
      await screen.findByText(
        translations['voting.governance.transactionError.sameVote']
      )
    ).toBeInTheDocument();
    expect(submitTransaction).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: CIP129_KEY })
    );
  });
});

describe('delegateVotes transaction id', () => {
  const originalCapability = global.cip30Enabled;
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  it('is null when the hardware send returns no transaction', async () => {
    global.cip30Enabled = false;
    const hardwareWallets = buildHardwareWallets({
      _sendMoney: jest.fn(async () => undefined),
    });
    const { store } = buildStore(hardwareWallets);
    await expect(
      store.delegateVotes({ chosenOption: 'abstain', wallet: hwWallet })
    ).resolves.toEqual({ success: true, transactionId: null });
  });

  it('is the id of the constructed software transaction on the cip30 build', async () => {
    global.cip30Enabled = true;
    const { store } = buildStore(buildHardwareWallets());
    const submitConstructedNativeTransaction = jest.fn(async () => ({
      id: 'native-tx',
    }));
    store.configure({
      wallets: { submitConstructedNativeTransaction },
    } as unknown as Parameters<VotingStore['configure']>[0]);
    await expect(
      store.delegateVotes({ chosenOption: 'abstain', wallet: softwareWallet })
    ).resolves.toEqual({ success: true, transactionId: 'native-tx' });
    expect(submitConstructedNativeTransaction).toHaveBeenCalledWith({
      walletId: softwareWallet.id,
      action: 'drep-delegation',
      data: { encoding: 'base16', vote: 'abstain' },
    });
  });
});

describe('upstream governance store dispatch', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });

  it.each([CIP129_KEY, 'abstain', 'no_confidence'])(
    'submits %s with the collected password while holding the wallet lock',
    async (chosenOption) => {
      let locked = false;
      const delegateVotes = jest.fn(async () => {
        expect(locked).toBe(true);
        return { id: 'upstream-transaction' };
      });
      const api = { ada: { delegateVotes } };
      const store = new VotingStore(
        api as unknown as Api,
        {} as ActionsMap,
        noopAnalyticsTracker
      );
      const withWalletSendLock = jest.fn(
        async (_walletId: string, work: () => Promise<unknown>) => {
          locked = true;
          try {
            return await work();
          } finally {
            locked = false;
          }
        }
      );
      store.configure({
        transactions: { withWalletSendLock },
      } as unknown as Parameters<VotingStore['configure']>[0]);
      await expect(
        store.delegateVotes({
          chosenOption,
          wallet: softwareWallet,
          passphrase: 'real-password',
        })
      ).resolves.toEqual({
        success: true,
        transactionId: 'upstream-transaction',
      });
      expect(delegateVotes).toHaveBeenCalledWith({
        dRepId: chosenOption,
        walletId: softwareWallet.id,
        passphrase: 'real-password',
      });
      expect(locked).toBe(false);
    }
  );

  it('prepares a hardware vote with a vote-only construct and submits it through the proxy', async () => {
    const hardwareWallets = buildHardwareWallets();
    const { store } = buildStore(hardwareWallets);
    await expect(
      store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: hwWallet,
      })
    ).resolves.toEqual({ success: true, fees: new BigNumber('0.180989') });
    expect(hardwareWallets.selectVoteCoins).toHaveBeenCalledWith({
      walletId: hwWallet.id,
      vote: CIP129_KEY,
    });
    expect(hardwareWallets.initiateTransaction).toHaveBeenCalledWith({
      walletId: hwWallet.id,
    });
    expect(hardwareWallets.updateTxSignRequest).not.toHaveBeenCalled();
    await expect(
      store.delegateVotes({
        chosenOption: CIP129_KEY,
        wallet: hwWallet,
      })
    ).resolves.toEqual({ success: true, transactionId: 'hw-tx-1' });
    expect(hardwareWallets._sendMoney).toHaveBeenCalledWith({
      selectedWalletId: hwWallet.id,
      isDelegationTransaction: true,
    });
    expect(
      hardwareWallets.submitConstructedNativeTransaction
    ).not.toHaveBeenCalled();
  });

  it('prices a software vote with a vote-only construct and no stake pool or join selection', async () => {
    const constructTransaction = jest
      .fn()
      .mockResolvedValue({ fee: new BigNumber('0.173245') });
    const selectCoins = jest.fn();
    const api = { ada: { constructTransaction, selectCoins } };
    const store = new VotingStore(
      api as unknown as Api,
      {} as ActionsMap,
      noopAnalyticsTracker
    );
    store.configure({
      hardwareWallets: {},
      staking: noStakePools(),
    } as unknown as Parameters<VotingStore['configure']>[0]);
    await expect(
      store.initializeVPDelegationTx({
        chosenOption: 'abstain',
        wallet: softwareWallet,
      })
    ).resolves.toEqual({ success: true, fees: new BigNumber('0.173245') });
    expect(constructTransaction).toHaveBeenCalledWith({
      walletId: softwareWallet.id,
      data: { encoding: 'base16', vote: 'abstain' },
    });
    expect(selectCoins).not.toHaveBeenCalled();
  });
});

describe('vote error codes from recorded wallet answers', () => {
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    jest.spyOn(logger, 'error').mockImplementation(() => undefined);
    jest.spyOn(logger, 'debug').mockImplementation(() => undefined);
  });
  afterEach(() => {
    jest.restoreAllMocks();
    global.cip30Enabled = originalCapability;
  });

  const answer = (
    backend: 'upstream' | 'cip30' | 'ledger-vote',
    name: string
  ) =>
    new GenericApiError(
      JSON.parse(loadRecordedExchange(backend, name).response.body)
    );
  const walletFrom = (backend: 'upstream', name: string) => {
    const capability = global.cip30Enabled;
    global.cip30Enabled = false;
    try {
      return _createWalletFromServerData(
        JSON.parse(loadRecordedExchange(backend, name).response.body)
      );
    } finally {
      global.cip30Enabled = capability;
    }
  };
  const lockedWallet = () => walletFrom('upstream', 'wallet-funds-held');
  const freeWallet = () => walletFrom('upstream', 'wallet-retired-drep');
  const notEnoughMoney = () =>
    new ApiError({
      code: 'not_enough_money',
      message: 'constructed from the wallet error shape',
    }).result();

  describe('initializeVPDelegationTx on the mainnet build', () => {
    const run = async (error: unknown, wallet: any) => {
      global.cip30Enabled = false;
      const constructTransaction = jest.fn().mockRejectedValue(error);
      const api = { ada: { constructTransaction } };
      const store = new VotingStore(
        api as any,
        {} as any,
        buildAnalytics() as any
      );
      store.configure({
        hardwareWallets: {},
        staking: noStakePools(),
      } as any);
      return store.initializeVPDelegationTx({
        chosenOption: 'abstain',
        wallet,
      });
    };

    it('gives pending_transaction when a pending transaction holds the inputs', async () => {
      const wallet = lockedWallet();
      expect(wallet.hasFundsHeldByPendingTransaction).toBe(true);
      await expect(
        run(answer('upstream', 'vote-construct-funds-held'), wallet)
      ).resolves.toEqual({ success: false, errorCode: 'pending_transaction' });
    });

    it('keeps no_utxos_available when nothing is pending', async () => {
      const wallet = freeWallet();
      expect(wallet.hasFundsHeldByPendingTransaction).toBe(false);
      await expect(
        run(answer('upstream', 'vote-construct-funds-held'), wallet)
      ).resolves.toEqual({ success: false, errorCode: 'no_utxos_available' });
    });

    it('gives same_vote when the pinned wallet already records the vote for a DRep that registered again', async () => {
      await expect(
        run(answer('upstream', 'vote-construct-reregistered'), freeWallet())
      ).resolves.toEqual({ success: false, errorCode: 'same_vote' });
    });

    it('gives not_found when the wallet registry does not hold the DRep', async () => {
      await expect(
        run(answer('ledger-vote', 'vote-construct-retired'), freeWallet())
      ).resolves.toEqual({ success: false, errorCode: 'not_found' });
    });
  });

  describe('delegateVotes', () => {
    const submit = async ({
      cip30,
      hardware,
      error,
      wallet,
    }: {
      cip30: boolean;
      hardware?: boolean;
      error: unknown;
      wallet: any;
    }) => {
      global.cip30Enabled = cip30;
      const delegateVotes = jest.fn().mockRejectedValue(error);
      const api = { ada: { delegateVotes } };
      const store = new VotingStore(
        api as any,
        {} as any,
        buildAnalytics() as any
      );
      const reject = jest.fn().mockRejectedValue(error);
      store.configure({
        hardwareWallets: {
          _sendMoney: reject,
          submitConstructedNativeTransaction: reject,
        },
        wallets: { submitConstructedNativeTransaction: reject },
        transactions: {
          withWalletSendLock: (_id: string, work: () => Promise<unknown>) =>
            work(),
        },
      } as any);
      const target = { ...wallet, isHardwareWallet: !!hardware };
      return store.delegateVotes({
        chosenOption: 'abstain',
        passphrase: 'Secret-passphrase-1',
        wallet: Object.assign(
          Object.create(Object.getPrototypeOf(wallet)),
          wallet,
          target
        ),
      });
    };

    const cases: Array<
      [string, boolean, boolean, () => unknown, string, boolean]
    > = [
      [
        'mainnet submit, same_vote',
        false,
        false,
        () => answer('upstream', 'vote-same-drep'),
        'same_vote',
        false,
      ],
      [
        'mainnet submit, no_utxos_available while pending',
        false,
        false,
        () => answer('upstream', 'vote-submit-funds-held'),
        'pending_transaction',
        true,
      ],
      [
        'mainnet submit, no_utxos_available with nothing pending',
        false,
        false,
        () => answer('upstream', 'vote-submit-funds-held'),
        'no_utxos_available',
        false,
      ],
      [
        'mainnet submit, not_found',
        false,
        false,
        () => answer('ledger-vote', 'vote-retired-drep'),
        'not_found',
        false,
      ],
      [
        'mainnet submit, not_enough_money',
        false,
        false,
        notEnoughMoney,
        'not_enough_money',
        false,
      ],
      [
        'mainnet hardware, same_vote',
        false,
        true,
        () => answer('upstream', 'vote-same-drep'),
        'same_vote',
        false,
      ],
      [
        'cip30 software, same_vote at construct',
        true,
        false,
        () => answer('cip30', 'vote-construct-retired'),
        'same_vote',
        false,
      ],
      [
        'cip30 software, not_found at construct',
        true,
        false,
        () => answer('ledger-vote', 'vote-construct-retired'),
        'not_found',
        false,
      ],
      [
        'cip30 hardware, same_vote at construct',
        true,
        true,
        () => answer('cip30', 'vote-construct-retired'),
        'same_vote',
        false,
      ],
      [
        'cip30 hardware, not_found at construct',
        true,
        true,
        () => answer('ledger-vote', 'vote-construct-retired'),
        'not_found',
        false,
      ],
    ];

    it.each(cases)(
      '%s',
      async (_label, cip30, hardware, error, expected, locked) => {
        const wallet = locked ? lockedWallet() : freeWallet();
        await expect(
          submit({ cip30, hardware, error: error(), wallet })
        ).resolves.toEqual({ success: false, errorCode: expected });
      }
    );
  });
});

describe('delegations submitted for rewards held back', () => {
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  const build = (ledgerVote: { status: string }) => {
    const wallet: any = observable({
      id: 'sw-wallet-1',
      isHardwareWallet: false,
      ledgerVote,
    });
    const recent = observable({
      transactions: [] as Array<{ id: string; state: string }>,
    });
    const api = { ada: {} };
    const store = new VotingStore(
      api as unknown as Api,
      {} as ActionsMap,
      noopAnalyticsTracker
    );
    store.configure({
      wallets: {
        getWalletById: () => wallet,
        submitConstructedNativeTransaction: jest
          .fn()
          .mockResolvedValue({ id: 'vote-tx' }),
      },
      transactions: {
        _getTransactionsRecentRequest: () => ({ result: recent }),
      },
    } as unknown as Parameters<VotingStore['configure']>[0]);
    return { store, wallet, recent };
  };

  it('records a submitted delegation for a wallet the ledger holds none for', async () => {
    const { store, wallet } = build({ status: 'not_delegated' });
    await store.delegateVotes({ chosenOption: 'abstain', wallet });
    expect(store.hasSubmittedVoteDelegation(wallet.id)).toBe(true);
  });

  it.each([['delegated'], ['unknown']])(
    'records nothing for a wallet whose ledger delegation is %s',
    async (status) => {
      const { store, wallet } = build({ status });
      await store.delegateVotes({ chosenOption: 'abstain', wallet });
      expect(store.hasSubmittedVoteDelegation(wallet.id)).toBe(false);
    }
  );

  it('forgets the delegation once the ledger shows one', async () => {
    const { store, wallet } = build({ status: 'not_delegated' });
    await store.delegateVotes({ chosenOption: 'abstain', wallet });
    store._clearSubmittedVoteDelegations();
    expect(store.hasSubmittedVoteDelegation(wallet.id)).toBe(true);
    runInAction(() => {
      wallet.ledgerVote = { status: 'delegated' };
    });
    store._clearSubmittedVoteDelegations();
    expect(store.hasSubmittedVoteDelegation(wallet.id)).toBe(false);
  });

  it.each(['failed', 'expired'])(
    'forgets the delegation when its transaction %s',
    async (state) => {
      const { store, wallet, recent } = build({ status: 'not_delegated' });
      await store.delegateVotes({ chosenOption: 'abstain', wallet });
      runInAction(() => {
        recent.transactions.push({ id: 'vote-tx', state });
      });
      store._clearSubmittedVoteDelegations();
      expect(store.hasSubmittedVoteDelegation(wallet.id)).toBe(false);
    }
  );

  it('keeps the delegation while its transaction is pending', async () => {
    const { store, wallet, recent } = build({ status: 'not_delegated' });
    await store.delegateVotes({ chosenOption: 'abstain', wallet });
    runInAction(() => {
      recent.transactions.push({ id: 'vote-tx', state: 'pending' });
    });
    store._clearSubmittedVoteDelegations();
    expect(store.hasSubmittedVoteDelegation(wallet.id)).toBe(true);
  });
});
