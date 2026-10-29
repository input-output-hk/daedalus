import BigNumber from 'bignumber.js';
import WalletsStore from './WalletsStore';
import type Wallet from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { AnalyticsTracker } from '../analytics';
import { WalletSyncStateStatuses } from '../domains/Wallet';

const wallet = (id: string) =>
  ({
    id,
    isLegacy: false,
    isRestoring: false,
    isNotResponding: false,
  }) as Wallet;

const createStore = (
  wallets: Wallet[],
  currentRoute: string,
  withdrawRewardsFor: (walletId: string) => boolean = () => true
) => {
  const apiMethod = jest.fn();
  const apiNamespace = new Proxy({}, { get: () => apiMethod });
  const api = new Proxy({}, { get: () => apiNamespace });
  const store = new WalletsStore(api as any, {} as any, {} as any);
  const app = { currentRoute };

  store.configure({
    app,
    addresses: { lastGeneratedAddress: null },
    walletSettings: { withdrawRewardsFor },
  } as any);
  store.walletsRequest.result = wallets;
  store._setActiveWallet = jest.fn(({ walletId }) => {
    store.active = wallets.find(({ id }) => id === walletId) || null;
  });
  store._unsetActiveWallet = jest.fn(() => {
    store.active = null;
  });
  store.goToWalletRoute = jest.fn();

  return { app, store };
};

describe('WalletsStore dApp routes', () => {
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  it('exposes no dApp wallet authority outside pilot packages', () => {
    global.cip30Enabled = false;
    const active = wallet('wallet-a');
    const { store } = createStore([active], '/wallets/wallet-a/summary');
    store.active = active;
    expect(store.eligibleDappWallets).toEqual([]);
    expect(store.activeDappWallet).toBeNull();
  });

  it('does not retarget a direct invalid dApp hash', () => {
    const { store } = createStore([wallet('wallet-a')], '/apps/missing');

    store._updateActiveWalletOnRouteChanges();

    expect(store._unsetActiveWallet).toHaveBeenCalledTimes(1);
    expect(store._setActiveWallet).not.toHaveBeenCalled();
    expect(store.goToWalletRoute).not.toHaveBeenCalled();
  });

  it('selects the wallet named by each dApp route', () => {
    const { app, store } = createStore(
      [wallet('wallet-a'), wallet('wallet-b')],
      '/apps/wallet-a'
    );

    store._updateActiveWalletOnRouteChanges();
    app.currentRoute = '/apps/wallet-b';
    store._updateActiveWalletOnRouteChanges();

    expect(store._setActiveWallet).toHaveBeenNthCalledWith(1, {
      walletId: 'wallet-a',
    });
    expect(store._setActiveWallet).toHaveBeenNthCalledWith(2, {
      walletId: 'wallet-b',
    });
    expect(store.goToWalletRoute).not.toHaveBeenCalled();
  });

  it.each([
    ['Byron', { isLegacy: true }],
    ['restoring', { isRestoring: true }],
    ['nonresponding', { isNotResponding: true }],
  ])('blocks %s wallets from dApp launch', (_state, override) => {
    const active = Object.assign(wallet('wallet-a'), override);
    const { store } = createStore([active], '/apps/wallet-a');
    store.active = active;

    expect(store.activeDappWallet).toBeNull();
  });

  it('blocks a deleted wallet from dApp launch', () => {
    const { store } = createStore([], '/apps/wallet-a');
    store.active = null;

    expect(store.activeDappWallet).toBeNull();
  });

  it('refreshes and removes an upstream wallet without connection services', async () => {
    global.cip30Enabled = false;
    const upstreamWallet = Object.assign(wallet('wallet-a'), {
      syncState: { status: WalletSyncStateStatuses.READY },
    });
    let backendWallets = [upstreamWallet];
    // Only backend and unrelated UI effects are replaced; refresh/delete requests run normally.
    const api = {
      ada: {
        getWallets: async () => backendWallets,
        deleteWallet: async () => {
          backendWallets = [];
          return true;
        },
      },
    } as unknown as Api;
    const trigger = jest.fn();
    const actions = {
      walletsLocal: {
        refreshWalletsLocalData: { trigger },
        unsetWalletLocalData: { trigger },
      },
      wallets: { refreshWalletsDataSuccess: { trigger } },
      router: { goToRoute: { trigger } },
      dialogs: { closeActiveDialog: { trigger } },
    } as unknown as ActionsMap;
    const analytics = { sendEvent: jest.fn() } as unknown as AnalyticsTracker;
    const store = new WalletsStore(api, actions, analytics);
    // Deliberately omit dapp: disabled packages cannot depend on its IPC lifecycle.
    const stores = {
      networkStatus: { isConnected: true },
      addresses: {
        _getAddressesAllRequest: jest.fn(),
        _refreshAddresses: jest.fn(),
      },
      transactions: {
        _getTransactionsRecentRequest: jest.fn(),
        _getTransactionsAllRequest: jest.fn(),
        _getWithdrawalsRequest: jest.fn(),
        _refreshTransactionData: jest.fn(),
      },
      hardwareWallets: { _unsetHardwareWalletLocalData: jest.fn() },
    } as unknown as StoresMap;
    store.configure(stores);
    await store.refreshWalletsData();
    expect(store.all.map(({ id }) => id)).toEqual(['wallet-a']);
    await store._deleteWallet({ walletId: 'wallet-a', isLegacy: false });
    expect(store.all).toEqual([]);
    // The wallet's local data, which holds its withdraw-rewards choice, goes.
    expect(trigger).toHaveBeenCalledWith({ walletId: 'wallet-a' });
    expect(store.isDeleting).toBe(false);
    expect(store.active).toBeNull();
  });
});

describe('WalletsStore withdrawalDecisionFor', () => {
  const withReward = (id: string, ledgerVote: unknown, reward = 5) =>
    ({
      id,
      isLegacy: false,
      ledgerVote,
      reward: new BigNumber(reward),
    }) as unknown as Wallet;

  it('decides from the wallet the id names', () => {
    const { store } = createStore(
      [
        withReward('held', { status: 'not_delegated' }),
        withReward('free', { status: 'unknown' }),
      ],
      '/wallets/free/summary'
    );
    expect(store.withdrawalDecisionFor('held', 'payment')).toMatchObject({
      kind: 'omit',
      reason: 'ledger_not_delegated',
    });
    expect(store.withdrawalDecisionFor('held', 'undelegation').kind).toBe(
      'delegate_vote_first'
    );
    expect(store.withdrawalDecisionFor('free', 'payment')).toMatchObject({
      kind: 'withdraw',
      withdrawal: 'self',
    });
  });

  it('passes the withdraw choice through', () => {
    const { store } = createStore(
      [withReward('free', { status: 'unknown' })],
      '/wallets/free/summary'
    );
    expect(
      store.withdrawalDecisionFor('free', 'payment', {
        withdrawRewards: false,
      })
    ).toMatchObject({ kind: 'omit', reason: 'declined' });
  });

  it('takes the withdraw choice from the wallet settings when none is passed', () => {
    const { store } = createStore(
      [
        withReward('off', { status: 'unknown' }),
        withReward('on', { status: 'unknown' }),
      ],
      '/wallets/on/summary',
      (walletId) => walletId === 'on'
    );
    expect(store.withdrawalDecisionFor('off', 'payment')).toMatchObject({
      kind: 'omit',
      reason: 'declined',
    });
    expect(store.withdrawalDecisionFor('on', 'payment')).toMatchObject({
      kind: 'withdraw',
      withdrawal: 'self',
    });
  });

  it('lets an explicit choice win over the wallet settings', () => {
    const { store } = createStore(
      [
        withReward('off', { status: 'unknown' }),
        withReward('on', { status: 'unknown' }),
      ],
      '/wallets/on/summary',
      (walletId) => walletId === 'on'
    );
    expect(
      store.withdrawalDecisionFor('off', 'payment', { withdrawRewards: true })
        .kind
    ).toBe('withdraw');
    expect(
      store.withdrawalDecisionFor('on', 'payment', { withdrawRewards: false })
    ).toMatchObject({ kind: 'omit', reason: 'declined' });
  });

  it('ignores the wallet settings for an undelegation', () => {
    const { store } = createStore(
      [withReward('free', { status: 'unknown' })],
      '/wallets/free/summary',
      () => false
    );
    expect(store.withdrawalDecisionFor('free', 'undelegation').kind).toBe(
      'withdraw'
    );
  });

  it('throws for a wallet the store does not hold', () => {
    const { store } = createStore([], '/wallets');
    expect(() => store.withdrawalDecisionFor('missing', 'payment')).toThrow();
  });
});

describe('WalletsStore _sendMoney withdrawal', () => {
  const originalCapability = global.cip30Enabled;
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  const payment = { receiver: 'addr1receiver', amount: '2000000' };
  const mountSend = () => {
    const createTransaction = jest.fn().mockResolvedValue({ id: 'tx' });
    const api = { ada: { createTransaction } };
    const store = new WalletsStore(
      api as any,
      { dialogs: { closeActiveDialog: { trigger: jest.fn() } } } as any,
      { sendEvent: jest.fn() } as any
    );
    const active = {
      id: 'wallet',
      isLegacy: false,
      isHardwareWallet: false,
    } as Wallet;
    store.configure({
      transactions: {
        withWalletSendLock: (_walletId: string, run: () => Promise<unknown>) =>
          run(),
      },
      addresses: { all: [{ id: payment.receiver }] },
    } as any);
    store.walletsRequest.result = [active];
    store.active = active;
    store.goToWalletRoute = jest.fn();
    store.refreshWalletsData = jest.fn();
    const submit = jest.fn().mockResolvedValue({ id: 'tx' });
    store.submitConstructedNativeTransaction = submit;
    return { store, createTransaction, submit };
  };

  it.each(['self', null] as const)(
    'passes the withdrawal %s to the mainnet payment request',
    async (withdrawal) => {
      global.cip30Enabled = false;
      const { store, createTransaction, submit } = mountSend();
      await store._sendMoney({ ...payment, passphrase: 'secret', withdrawal });
      expect(createTransaction).toHaveBeenCalledTimes(1);
      expect(createTransaction.mock.calls[0][0].withdrawal).toBe(withdrawal);
      expect(submit).not.toHaveBeenCalled();
    }
  );

  it('spreads the withdrawal into the cip30 construct body when it is self', async () => {
    global.cip30Enabled = true;
    const { store, createTransaction, submit } = mountSend();
    await store._sendMoney({ ...payment, withdrawal: 'self' });
    expect(createTransaction).not.toHaveBeenCalled();
    expect(submit.mock.calls[0][0].data.withdrawal).toBe('self');
  });

  it('leaves the withdrawal key out of the cip30 construct body when it is null', async () => {
    global.cip30Enabled = true;
    const { store, submit } = mountSend();
    await store._sendMoney({ ...payment, withdrawal: null });
    expect('withdrawal' in submit.mock.calls[0][0].data).toBe(false);
  });
});
