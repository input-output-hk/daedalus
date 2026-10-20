import BigNumber from 'bignumber.js';
import AdaApi from '../api/api';
import WalletsStore from './WalletsStore';
import TransactionsStore from './TransactionsStore';
import StakingStore from './StakingStore';
import VotingStore from './VotingStore';
import { request } from '../api/utils/request';
import { walletInputSelectionChannel } from '../ipc/collateral';
import { createTransaction } from '../api/transactions/requests/createTransaction';
import { constructTransaction } from '../api/transactions/requests/constructTransaction';
import { getTransactionFee } from '../api/transactions/requests/getTransactionFee';
import { selectCoins } from '../api/transactions/requests/selectCoins';
import { joinStakePool } from '../api/staking/requests/joinStakePool';
import { quitStakePool } from '../api/staking/requests/quitStakePool';
import { getDelegationFee } from '../api/staking/requests/getDelegationFee';
import type Wallet from '../domains/Wallet';
import { WalletUnits } from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { AnalyticsTracker } from '../analytics';
import type { AssetToken } from '../api/assets/types';
import type { RequestConfig } from '../api/common/types';
import { DELEGATION_ACTIONS } from '../config/stakingConfig';

jest.mock('../api/utils/request', () => ({ request: jest.fn() }));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));
jest.mock('../ipc/collateral', () => ({
  walletInputSelectionChannel: { request: jest.fn() },
}));

const walletId = 'ab'.repeat(20);
const transactionId = 'cd'.repeat(32);
const config: RequestConfig = {
  hostname: 'localhost',
  port: 8090,
  ca: new Uint8Array(),
  cert: new Uint8Array(),
  key: new Uint8Array(),
};
const http = request as jest.Mock;
const collateral = walletInputSelectionChannel.request as jest.Mock;
const payment = {
  address: 'addr1-upstream-recipient',
  amount: {
    quantity: new BigNumber(2000000),
    unit: WalletUnits.LOVELACE as const,
  },
};
const backendTransaction = {
  id: transactionId,
  amount: { quantity: 2200000, unit: 'lovelace' },
  fee: { quantity: 200000, unit: 'lovelace' },
  deposit_taken: { quantity: 0, unit: 'lovelace' },
  deposit_returned: { quantity: 0, unit: 'lovelace' },
  pending_since: { time: '2026-10-20T00:00:00Z' },
  direction: 'outgoing',
  inputs: [],
  outputs: [payment],
  withdrawals: [],
  status: 'pending',
};

const createStores = (isLegacy = false) => {
  // These stores only consume the ADA API, dialog close action, and analytics event boundary.
  const api = { ada: new AdaApi(false, config) } as unknown as Api;
  const actions = {
    dialogs: { closeActiveDialog: { trigger: jest.fn() } },
  } as unknown as ActionsMap;
  const analytics = { sendEvent: jest.fn() } as unknown as AnalyticsTracker;
  const wallets = new WalletsStore(api, actions, analytics);
  const transactions = new TransactionsStore(api, actions, analytics);
  const staking = new StakingStore(api, actions, analytics);
  const voting = new VotingStore(api, actions, analytics);
  // Unrelated stores are not initialized by this transaction-only harness.
  const stores = {
    wallets,
    transactions,
    staking,
    voting,
  } as unknown as StoresMap;
  wallets.configure(stores);
  transactions.configure(stores);
  staking.configure(stores);
  voting.configure(stores);
  wallets.active = { id: walletId, isLegacy } as Wallet;
  wallets.walletsRequest.result = [wallets.active];
  wallets.refreshWalletsData = jest.fn();
  wallets.goToWalletRoute = jest.fn();
  return { wallets, transactions, staking, voting, actions };
};

describe('upstream native backend compatibility', () => {
  const originalGate = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = false;
    http.mockReset();
    collateral.mockReset();
    collateral.mockRejectedValue(new Error('Pilot IPC must not be used'));
  });
  afterAll(() => {
    global.cip30Enabled = originalGate;
  });

  it.each([
    [
      'create',
      createTransaction,
      {
        walletId,
        data: { payments: [payment], passphrase: 'approved-password' },
      },
      'POST',
      `/v2/wallets/${walletId}/transactions/`,
    ],
    [
      'construct',
      constructTransaction,
      { walletId, data: { payments: [payment], encoding: 'base16' } },
      'POST',
      `/v2/wallets/${walletId}/transactions-construct`,
    ],
    [
      'fee',
      getTransactionFee,
      { walletId, data: { payments: [payment] } },
      'POST',
      `/v2/wallets/${walletId}/payment-fees`,
    ],
    [
      'selection',
      selectCoins,
      { walletId, data: { payments: [payment] } },
      'POST',
      `/v2/wallets/${walletId}/coin-selections/random`,
    ],
    [
      'delegate',
      joinStakePool,
      { walletId, stakePoolId: 'pool1', passphrase: 'approved-password' },
      'PUT',
      `/v2/stake-pools/pool1/wallets/${walletId}`,
    ],
    [
      'undelegate',
      quitStakePool,
      { walletId, passphrase: 'approved-password' },
      'DELETE',
      `/v2/stake-pools/*/wallets/${walletId}`,
    ],
    [
      'delegation fee',
      getDelegationFee,
      { walletId },
      'GET',
      `/v2/wallets/${walletId}/delegation-fees`,
    ],
  ] as const)(
    '%s sends an upstream contract without collateral IPC',
    async (_name, invoke, params, method, path) => {
      http.mockResolvedValue({});
      await (invoke as Function)(config, params);
      expect(http).toHaveBeenCalledTimes(1);
      const [options, query, body] = http.mock.calls[0];
      expect(options).toEqual({ method, path, ...config });
      expect(query).toEqual({});
      let expectedBody: unknown;
      if ('data' in params) expectedBody = params.data;
      else if ('passphrase' in params)
        expectedBody = { passphrase: params.passphrase };
      expect(body).toEqual(expectedBody);
      expect(collateral).not.toHaveBeenCalled();
    }
  );

  it.each([false, true])(
    'submits approved software Send with the correct Shelley/Byron contract (legacy=%s)',
    async (isLegacy) => {
      http.mockResolvedValue(backendTransaction);
      const { wallets, transactions, actions } = createStores(isLegacy);
      const lease = await transactions.acquireWalletSendLock(walletId);
      // Send reads the asset identity; metadata loading is outside this HTTP contract.
      const asset = {
        policyId: 'ef'.repeat(28),
        assetName: '01',
      } as AssetToken;
      const send = wallets._sendMoney({
        receiver: payment.address,
        amount: '2000000',
        passphrase: 'approved-password',
        assets: [asset],
        assetsAmounts: ['7'],
      });
      await Promise.resolve();
      expect(http).not.toHaveBeenCalled();
      expect(actions.dialogs.closeActiveDialog.trigger).not.toHaveBeenCalled();
      lease.release();
      await send;
      expect(http).toHaveBeenCalledTimes(1);
      const [options, , body] = http.mock.calls[0];
      expect(options).toEqual({
        method: 'POST',
        path: `/v2/${isLegacy ? 'byron-wallets' : 'wallets'}/${walletId}/transactions/`,
        ...config,
      });
      expect(body.passphrase).toBe('approved-password');
      expect(body.payments[0].amount).toEqual({
        quantity: 2000000,
        unit: 'lovelace',
      });
      expect(body.payments[0].assets[0].quantity.toFixed()).toBe('7');
      expect(body).not.toHaveProperty('preferred_collateral');
      expect(collateral).not.toHaveBeenCalled();
      expect(wallets.refreshWalletsData).toHaveBeenCalledTimes(1);
      expect(actions.dialogs.closeActiveDialog.trigger).toHaveBeenCalledTimes(
        1
      );
    }
  );

  it('keeps a rejected password recoverable and releases the wallet queue', async () => {
    http
      .mockRejectedValueOnce({
        code: 'wrong_encryption_passphrase',
        message: 'Wrong passphrase',
      })
      .mockResolvedValue(backendTransaction);
    const { wallets, transactions, actions } = createStores();
    await expect(
      wallets._sendMoney({
        receiver: payment.address,
        amount: '2000000',
        passphrase: 'wrong-password',
      })
    ).rejects.toBeDefined();
    expect(actions.dialogs.closeActiveDialog.trigger).not.toHaveBeenCalled();
    const lease = await transactions.acquireWalletSendLock(walletId);
    lease.release();
    await wallets._sendMoney({
      receiver: payment.address,
      amount: '2000000',
      passphrase: 'approved-password',
    });
    expect(http).toHaveBeenCalledTimes(2);
    expect(http.mock.calls[1][2].passphrase).toBe('approved-password');
    expect(actions.dialogs.closeActiveDialog.trigger).toHaveBeenCalledTimes(1);
  });

  it('rejects direct exact execution before any backend access', async () => {
    const { wallets } = createStores();
    await expect(
      wallets.submitConstructedNativeTransaction({
        walletId,
        data: { encoding: 'base16', payments: [payment] },
        action: 'payment',
      })
    ).rejects.toThrow('Exact native transactions are unavailable');
    expect(http).not.toHaveBeenCalled();
    expect(collateral).not.toHaveBeenCalled();
  });

  it.each([
    'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy',
    'abstain',
    'no_confidence',
  ])(
    'authorizes the exact governance choice %s through upstream HTTP',
    async (chosenOption) => {
      const { wallets, voting, transactions } = createStores();
      const wallet = wallets.active;
      const unapproved = await voting.delegateVotes({ wallet, chosenOption });
      expect(unapproved.success).toBe(false);
      expect(http).not.toHaveBeenCalled();
      http.mockResolvedValue(backendTransaction);
      const lease = await transactions.acquireWalletSendLock(walletId);
      const submitted = voting.delegateVotes({
        wallet,
        chosenOption,
        passphrase: 'approved-password',
      });
      await Promise.resolve();
      expect(http).not.toHaveBeenCalled();
      lease.release();
      await expect(submitted).resolves.toEqual({ success: true });
      expect(http).toHaveBeenCalledTimes(1);
      expect(http.mock.calls[0][0]).toMatchObject({
        method: 'PUT',
        path: `/v2/dreps/${chosenOption}/wallets/${walletId}`,
      });
      expect(http.mock.calls[0][2]).toEqual({
        passphrase: 'approved-password',
      });
      expect(collateral).not.toHaveBeenCalled();
    }
  );

  it('ignores stale single-address preferences when migrating to an upstream wallet', async () => {
    const { wallets } = createStores();
    wallets.active.singleAddressMode = true;
    wallets.transferFundsSourceWalletId = 'legacy_source';
    wallets.transferFundsTargetWalletId = walletId;
    http
      .mockResolvedValueOnce(
        Array.from({ length: 22 }, (_, index) => ({
          id: `unused-address-${index}`,
          state: 'unused',
          derivation_path: ['1852H', '1815H', '0H', '0', String(index)],
        }))
      )
      .mockResolvedValueOnce([]);
    await wallets._transferFunds({ spendingPassword: 'approved-password' });
    expect(http.mock.calls[0][0]).toMatchObject({
      method: 'GET',
      path: `/v2/wallets/${walletId}/addresses`,
    });
    expect(http.mock.calls[0][1]).toEqual({ state: 'unused' });
    expect(http.mock.calls[1][0]).toMatchObject({
      method: 'POST',
      path: '/v2/byron-wallets/source/migrations',
    });
    expect(http.mock.calls[1][2]).toEqual({
      passphrase: 'approved-password',
      addresses: Array.from(
        { length: 20 },
        (_, index) => `unused-address-${21 - index}`
      ),
    });
    expect(collateral).not.toHaveBeenCalled();
  });

  it.each(['delegate', 'redelegate', 'undelegate'] as const)(
    'serializes approved %s through upstream HTTP and observes backend confirmation',
    async (operation) => {
      jest.useFakeTimers();
      try {
        const { wallets, transactions, staking } = createStores();
        const recentRequest =
          transactions._getTransactionsRecentRequest(walletId);
        transactions.transactionsRequests = [
          {
            walletId,
            isLegacy: false,
            recentRequest,
            allRequest: transactions._getTransactionsAllRequest(walletId),
            withdrawalsRequest: transactions._getWithdrawalsRequest(walletId),
          },
        ];
        const flush = async () => {
          for (let phase = 0; phase < 15; phase += 1) {
            await Promise.resolve();
            jest.advanceTimersByTime(2);
          }
        };
        http.mockResolvedValueOnce(backendTransaction);
        const lease = await transactions.acquireWalletSendLock(walletId);
        const params = {
          walletId,
          stakePoolId: operation === 'redelegate' ? 'pool-new' : 'pool-first',
          passphrase: 'approved-password',
          isHardwareWallet: false,
        };
        const submitted =
          operation === 'undelegate'
            ? staking._quitStakePool(params)
            : staking._joinStakePool(params);
        await flush();
        expect(http).not.toHaveBeenCalled();
        lease.release();
        await flush();
        await submitted;
        expect(http.mock.calls[0][0]).toMatchObject({
          method: operation === 'undelegate' ? 'DELETE' : 'PUT',
          path: `/v2/stake-pools/${operation === 'undelegate' ? '*' : params.stakePoolId}/wallets/${walletId}`,
        });
        expect(http.mock.calls[0][2]).toEqual({
          passphrase: 'approved-password',
        });
        expect(staking.isDelegationTransactionPending).toBe(true);
        http.mockResolvedValueOnce([
          {
            ...backendTransaction,
            status: 'in_ledger',
            inserted_at: { time: '2026-10-20T00:01:00Z' },
          },
        ]);
        const observation = recentRequest.execute({
          walletId,
          isLegacy: false,
        }).promise;
        await flush();
        await observation;
        staking.checkDelegationTransaction({ walletId, transactionId });
        expect(staking.isDelegationTransactionPending).toBe(false);
        expect(wallets.refreshWalletsData).toHaveBeenCalled();
        expect(collateral).not.toHaveBeenCalled();
      } finally {
        jest.clearAllTimers();
        jest.useRealTimers();
      }
    }
  );

  it.each([undefined, [{ quantity: 2000000, unit: 'lovelace' }]])(
    'displays the upstream undelegation refund without requiring returned-deposit fields (%s)',
    async (depositsReturned) => {
      http.mockResolvedValue({
        inputs: [
          {
            id: transactionId,
            index: 0,
            address: payment.address,
            amount: { quantity: 10000000, unit: 'lovelace' },
            derivation_path: [],
          },
        ],
        outputs: [],
        change: [
          {
            address: payment.address,
            amount: { quantity: 11800000, unit: 'lovelace' },
            derivation_path: [],
          },
        ],
        deposits_taken: [],
        ...(depositsReturned ? { deposits_returned: depositsReturned } : {}),
        certificates: [
          { certificate_type: 'quit_pool', reward_account_path: [] },
        ],
      });
      const selection = await new AdaApi(false, config).selectCoins({
        walletId,
        walletBalance: new BigNumber(10),
        availableBalance: new BigNumber(10),
        rewardsBalance: new BigNumber(0),
        delegation: {
          walletId,
          delegationAction: DELEGATION_ACTIONS.QUIT,
          poolId: '*',
        },
      });
      expect(selection.depositsReclaimed.toFixed()).toBe('2');
      expect(selection.fee.toFixed()).toBe('0.2');
      expect(collateral).not.toHaveBeenCalled();
    }
  );

  it('retains pilot input preferences independently of launcher visibility', async () => {
    global.cip30Enabled = true;
    collateral.mockResolvedValue({
      preferred_collateral: [{ transaction_id: transactionId, index: 0 }],
    });
    http.mockResolvedValue({});
    await createTransaction(config, {
      walletId,
      data: { payments: [payment], passphrase: 'approved-password' },
    });
    expect(http.mock.calls[0][2].preferred_collateral).toEqual([
      { transaction_id: transactionId, index: 0 },
    ]);
  });
});
