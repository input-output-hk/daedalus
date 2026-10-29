import { DELEGATION_ACTIONS } from '../../../source/renderer/app/config/stakingConfig';
import type AdaApiClass from '../../../source/renderer/app/api/api';
import {
  balancesFor,
  decideWithdrawal,
} from '../../../source/renderer/app/utils/governance/rewardWithdrawal';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../support/walletReplay';
import type {
  RecordedExchange,
  ReplayRoute,
  WalletReplay,
} from '../support/walletReplay';

jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

provideRequestGlobals();
const AdaApi = require('../../../source/renderer/app/api/api')
  .default as typeof AdaApiClass;

const load = (name: string) => loadRecordedExchange('ledger-vote', name);
const PASSPHRASE = 'Secret-passphrase-1';

type PaymentBody = {
  payments: Array<{ address: string; amount: { quantity: number } }>;
};
const payment = (exchange: RecordedExchange) => {
  const body = exchange.request.body as PaymentBody;
  return {
    address: body.payments[0].address,
    amount: body.payments[0].amount.quantity,
  };
};
const walletIdOf = (exchange: RecordedExchange) =>
  exchange.request.path.split('/')[3];

describe('Requests built from a recorded wallet response', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  // Reads the wallet as Daedalus does, then lets the withdrawal decision
  // choose the withdrawal and the balances the payment is checked against.
  const readWallet = async (
    walletName: string,
    extraRoutes: Array<ReplayRoute> = [],
    withdrawRewards?: boolean
  ) => {
    const walletExchange = load(walletName);
    replay = await startWalletReplay([
      {
        method: 'GET',
        path: /\/v2\/wallets\/[0-9a-f]+$/,
        reply: walletExchange,
      },
      ...extraRoutes,
    ]);
    const api = new AdaApi(false, replay.config);
    const wallet = await api.getWallet({
      walletId: walletIdOf(walletExchange),
      isLegacy: false,
    });
    const decision = decideWithdrawal({
      purpose: 'payment',
      isLegacy: wallet.isLegacy,
      ledgerVote: wallet.ledgerVote,
      reward: wallet.reward,
      withdrawRewards,
    });
    replay.received.length = 0;
    return {
      api,
      wallet,
      decision,
      balances: balancesFor(wallet, decision.withdrawal),
    };
  };

  describe('a wallet holding rewards without a vote delegation', () => {
    it('decides to omit the withdrawal', async () => {
      const { decision, wallet } = await readWallet('wallet-held-rewards');
      expect(wallet.reward.gt(0)).toBe(true);
      expect(decision).toEqual({
        kind: 'omit',
        withdrawal: null,
        reason: 'ledger_not_delegated',
      });
    });

    it('prices the payment with the body the wallet accepted', async () => {
      const fee = load('payment-fee-nowdrl');
      const { api, wallet, decision, balances } = await readWallet(
        'wallet-held-rewards',
        [{ method: 'POST', path: /\/payment-fees$/, reply: fee }]
      );
      await api.calculateTransactionFee({
        walletId: wallet.id,
        ...payment(fee),
        ...balances,
        isLegacy: wallet.isLegacy,
        withdrawal: decision.withdrawal,
      });
      expect(replay.received).toHaveLength(1);
      expect(replay.received[0].body).toEqual(fee.request.body);
      expect(replay.received[0].body).not.toHaveProperty('withdrawal');
    });

    it('sends the payment with the body the wallet accepted', async () => {
      const sent = load('payment-nowdrl');
      const { api, wallet, decision } = await readWallet(
        'wallet-held-rewards',
        [{ method: 'POST', path: /\/transactions\/?$/, reply: sent }]
      );
      await api.createTransaction({
        walletId: wallet.id,
        ...payment(sent),
        passphrase: PASSPHRASE,
        isLegacy: wallet.isLegacy,
        hasAssetsRemainingAfterTransaction: false,
        withdrawal: decision.withdrawal,
      });
      expect(replay.received).toHaveLength(1);
      expect(replay.received[0].body).toEqual({
        ...(sent.request.body as object),
        passphrase: PASSPHRASE,
      });
      expect(replay.received[0].body).not.toHaveProperty('withdrawal');
    });

    it('selects hardware payment coins with the body the wallet accepted', async () => {
      const selection = load('hardware-selection-nowdrl');
      const { api, wallet, decision, balances } = await readWallet(
        'wallet-held-rewards',
        [
          {
            method: 'POST',
            path: /\/coin-selections\/random$/,
            reply: selection,
          },
        ]
      );
      await api.selectCoins({
        walletId: wallet.id,
        ...balances,
        payments: {
          walletId: wallet.id,
          ...payment(selection),
          withdrawal: decision.withdrawal,
        },
      });
      expect(replay.received).toHaveLength(1);
      expect(replay.received[0].body).toEqual(selection.request.body);
      expect(replay.received[0].body).not.toHaveProperty('withdrawal');
    });
  });

  describe.each([['wallet-abstain'], ['wallet-drep']])(
    'the %s wallet, whose vote is delegated',
    (walletName) => {
      it('decides to withdraw', async () => {
        const { decision, wallet } = await readWallet(walletName);
        expect(wallet.reward.gt(0)).toBe(true);
        expect(decision).toEqual({
          kind: 'withdraw',
          withdrawal: 'self',
          reason: 'ledger_delegated',
        });
      });

      it('prices the payment with the body the wallet accepted', async () => {
        const fee = load('payment-fee-delegated');
        const { api, wallet, decision, balances } = await readWallet(
          walletName,
          [{ method: 'POST', path: /\/payment-fees$/, reply: fee }]
        );
        await api.calculateTransactionFee({
          walletId: wallet.id,
          ...payment(fee),
          ...balances,
          isLegacy: wallet.isLegacy,
          withdrawal: decision.withdrawal,
        });
        expect(replay.received).toHaveLength(1);
        expect(replay.received[0].body).toEqual(fee.request.body);
        expect(replay.received[0].body).toHaveProperty('withdrawal', 'self');
      });
    }
  );

  describe('a wallet whose vote is delegated, with the control off', () => {
    it('decides to leave the rewards out and counts only the other funds', async () => {
      const { decision, wallet, balances } = await readWallet(
        'wallet-drep',
        [],
        false
      );
      expect(decision).toEqual({
        kind: 'omit',
        withdrawal: null,
        reason: 'declined',
      });
      expect(balances.walletBalance.toString()).toBe(
        wallet.amount.minus(wallet.reward).toString()
      );
    });

    it('prices the payment with the body the wallet accepted without a withdrawal', async () => {
      const fee = load('payment-fee-nowdrl');
      const { api, wallet, decision, balances } = await readWallet(
        'wallet-drep',
        [{ method: 'POST', path: /\/payment-fees$/, reply: fee }],
        false
      );
      await api.calculateTransactionFee({
        walletId: wallet.id,
        ...payment(fee),
        ...balances,
        isLegacy: wallet.isLegacy,
        withdrawal: decision.withdrawal,
      });
      expect(replay.received).toHaveLength(1);
      expect(replay.received[0].body).toEqual(fee.request.body);
      expect(replay.received[0].body).not.toHaveProperty('withdrawal');
    });

    it('sends the payment with the body the wallet accepted without a withdrawal', async () => {
      const sent = load('payment-nowdrl');
      const { api, wallet, decision } = await readWallet(
        'wallet-drep',
        [{ method: 'POST', path: /\/transactions\/?$/, reply: sent }],
        false
      );
      await api.createTransaction({
        walletId: wallet.id,
        ...payment(sent),
        passphrase: PASSPHRASE,
        isLegacy: wallet.isLegacy,
        hasAssetsRemainingAfterTransaction: false,
        withdrawal: decision.withdrawal,
      });
      expect(replay.received[0].body).toEqual({
        ...(sent.request.body as object),
        passphrase: PASSPHRASE,
      });
      expect(replay.received[0].body).not.toHaveProperty('withdrawal');
    });

    it('selects hardware payment coins with the body the wallet accepted without a withdrawal', async () => {
      const selection = load('hardware-selection-nowdrl');
      const { api, wallet, decision, balances } = await readWallet(
        'wallet-drep',
        [
          {
            method: 'POST',
            path: /\/coin-selections\/random$/,
            reply: selection,
          },
        ],
        false
      );
      await api.selectCoins({
        walletId: wallet.id,
        ...balances,
        payments: {
          walletId: wallet.id,
          ...payment(selection),
          withdrawal: decision.withdrawal,
        },
      });
      expect(replay.received[0].body).toEqual(selection.request.body);
      expect(replay.received[0].body).not.toHaveProperty('withdrawal');
    });
  });

  describe('the two undelegation steps', () => {
    it('delegates the vote to Abstain with the body the wallet accepted', async () => {
      const vote = load('vote-abstain');
      const { api, wallet } = await readWallet('wallet-held-rewards', [
        { method: 'PUT', path: /\/v2\/dreps\/abstain\/wallets\//, reply: vote },
      ]);
      await api.delegateVotes({
        dRepId: 'abstain',
        walletId: wallet.id,
        passphrase: PASSPHRASE,
      });
      expect(replay.received).toHaveLength(1);
      expect(replay.received[0].method).toBe('PUT');
      expect(replay.received[0].path).toBe(vote.request.path);
      expect(replay.received[0].body).toEqual({ passphrase: PASSPHRASE });
    });

    it('selects the hardware quit with the body the wallet accepted', async () => {
      const quit = load('hardware-quit-selection');
      const body = quit.request.body as {
        delegation_action: { action: string; pool: string };
      };
      const { api, wallet, balances } = await readWallet('wallet-abstain', [
        { method: 'POST', path: /\/coin-selections\/random$/, reply: quit },
      ]);
      await api.selectCoins({
        walletId: wallet.id,
        ...balances,
        delegation: {
          walletId: wallet.id,
          poolId: body.delegation_action.pool,
          delegationAction: DELEGATION_ACTIONS.QUIT,
        },
      });
      expect(replay.received).toHaveLength(1);
      expect(replay.received[0].path).toBe(quit.request.path);
      expect(replay.received[0].body).toEqual(quit.request.body);
    });
  });
});
