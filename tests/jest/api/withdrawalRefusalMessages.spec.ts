import BigNumber from 'bignumber.js';
import type AdaApiClass from '../../../source/renderer/app/api/api';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../support/walletReplay';
import type { RecordedExchange, WalletReplay } from '../support/walletReplay';

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
const {
  _createWalletFromServerData,
} = require('../../../source/renderer/app/api/api');
const {
  definiteSubmissionFailure,
} = require('../../../source/renderer/app/api/utils/submissionFailure');
const {
  balancesFor,
} = require('../../../source/renderer/app/utils/governance/rewardWithdrawal');

const WITHDRAWAL_MESSAGE = 'api.errors.withdrawalNotPossibleWithoutVote';

// A wallet answer built from the wallet's error shape, for a case that no
// harness run has recorded.
const constructedAnswer = (
  status: number,
  body: Record<string, unknown>
): RecordedExchange => ({
  backend: 'upstream',
  wallet: 'constructed',
  recorded: 'constructed',
  chain: null,
  request: { method: '', path: '', body: null },
  response: { status, body: JSON.stringify(body) },
});

const balances = {
  walletBalance: new BigNumber(100),
  availableBalance: new BigNumber(100),
  rewardsBalance: new BigNumber(0),
};

const paymentRequest = (exchange: RecordedExchange) => {
  const body = exchange.request.body as {
    payments: Array<{ address: string; amount: { quantity: number } }>;
  };
  return {
    walletId: exchange.request.path.split('/')[3],
    address: body.payments[0].address,
    amount: body.payments[0].amount.quantity,
    passphrase: 'Secret-passphrase-1',
    isLegacy: false,
    hasAssetsRemainingAfterTransaction: false,
    withdrawal: 'self' as const,
  };
};

describe('AdaApi refusal messages', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const replayRoute = async (
    method: string,
    path: RegExp,
    reply: RecordedExchange
  ) => {
    replay = await startWalletReplay([{ method, path, reply }]);
    return new AdaApi(false, replay.config);
  };

  describe('createTransaction', () => {
    it('maps the wallet refusal of a withdrawal to the withdrawal message', async () => {
      const exchange = loadRecordedExchange('ledger-vote', 'payment-refused');
      const api = await replayRoute('POST', /\/transactions\/?$/, exchange);
      const error = await api
        .createTransaction(paymentRequest(exchange))
        .catch((e) => e);
      expect(error.id).toBe(WITHDRAWAL_MESSAGE);
    });

    it('maps a send refused for lack of inputs to the pending transaction message with the wallet id', async () => {
      // The wallet's answer to a payment send while a refused transaction holds
      // the inputs: HTTP 403 no_utxos_available.
      const exchange = constructedAnswer(403, {
        code: 'no_utxos_available',
        message:
          'Unable to create a transaction because the wallet has no unspent transaction outputs (UTxOs) available.',
      });
      const api = await replayRoute('POST', /\/transactions\/?$/, exchange);
      const request = paymentRequest(
        loadRecordedExchange('ledger-vote', 'payment-refused')
      );
      const error = await api.createTransaction(request).catch((e) => e);
      expect(error.id).toBe('api.errors.fundsHeldByPendingTransaction');
      expect(error.values.walletId).toBe(request.walletId);
    });

    it('maps a too-short passphrase to the incorrect password message', async () => {
      const exchange = constructedAnswer(400, {
        code: 'bad_request',
        message: 'passphrase is too short: expected at least 10 characters',
      });
      const api = await replayRoute('POST', /\/transactions\/?$/, exchange);
      const error = await api
        .createTransaction({
          ...paymentRequest(
            loadRecordedExchange('ledger-vote', 'payment-refused')
          ),
        })
        .catch((e) => e);
      expect(error.id).toBe('api.errors.IncorrectPasswordError');
    });
  });

  describe('calculateTransactionFee', () => {
    it('maps the wallet refusal of a withdrawal to the withdrawal message', async () => {
      const exchange = loadRecordedExchange('ledger-vote', 'payment-fee');
      const api = await replayRoute('POST', /\/payment-fees$/, exchange);
      const error = await api
        .calculateTransactionFee({
          ...paymentRequest(exchange),
          ...balances,
        })
        .catch((e) => e);
      expect(error.id).toBe(WITHDRAWAL_MESSAGE);
    });
  });

  describe('selectCoins', () => {
    it('maps the refusal of a hardware payment selection to the withdrawal message', async () => {
      const exchange = loadRecordedExchange(
        'ledger-vote',
        'hardware-selection'
      );
      const { walletId, address, amount } = paymentRequest(exchange);
      const api = await replayRoute(
        'POST',
        /\/coin-selections\/random$/,
        exchange
      );
      const error = await api
        .selectCoins({
          walletId,
          payments: { walletId, address, amount, withdrawal: 'self' } as never,
          ...balances,
        })
        .catch((e) => e);
      expect(error.id).toBe(WITHDRAWAL_MESSAGE);
    });

    it('maps the refusal of a hardware undelegation selection to the withdrawal message', async () => {
      const exchange = loadRecordedExchange(
        'ledger-vote',
        'hardware-quit-refused'
      );
      const api = await replayRoute(
        'POST',
        /\/coin-selections\/random$/,
        exchange
      );
      const error = await api
        .selectCoins({
          walletId: exchange.request.path.split('/')[3],
          delegation: { delegationAction: 'quit', poolId: 'pool' } as never,
          ...balances,
        })
        .catch((e) => e);
      expect(error.id).toBe(WITHDRAWAL_MESSAGE);
    });
  });

  describe('quitStakePool', () => {
    it('maps the wallet refusal of a withdrawing quit to the withdrawal message', async () => {
      const exchange = loadRecordedExchange('ledger-vote', 'quit-refused');
      const api = await replayRoute('DELETE', /\/stake-pools\//, exchange);
      const error = await api
        .quitStakePool({
          walletId: exchange.request.path.split('/').pop(),
          passphrase: 'Secret-passphrase-1',
        })
        .catch((e) => e);
      expect(error.id).toBe(WITHDRAWAL_MESSAGE);
    });

    it('maps the node refusal of a withdrawing quit to the withdrawal message', async () => {
      const exchange = loadRecordedExchange('upstream', 'quit-refused');
      const api = await replayRoute('DELETE', /\/stake-pools\//, exchange);
      const error = await api
        .quitStakePool({
          walletId: exchange.request.path.split('/').pop(),
          passphrase: 'Secret-passphrase-1',
        })
        .catch((e) => e);
      expect(error.id).toBe(WITHDRAWAL_MESSAGE);
    });
  });

  describe('constructTransaction', () => {
    it.each([
      ['a payment', 'construct-payment-refused'],
      ['an undelegation', 'quit-construct-refused'],
    ])(
      'maps the refusal of a withdrawing %s construct to the withdrawal message',
      async (_label, name) => {
        const exchange = loadRecordedExchange('ledger-vote', name);
        const api = await replayRoute(
          'POST',
          /\/transactions-construct$/,
          exchange
        );
        const error = await api
          .constructTransaction({
            walletId: exchange.request.path.split('/')[3],
            data: exchange.request.body as never,
          })
          .catch((e) => e);
        expect(error.id).toBe(WITHDRAWAL_MESSAGE);
      }
    );
  });

  describe('with the rewards held', () => {
    const heldWallet = () => {
      const fixture = loadRecordedExchange(
        'ledger-vote',
        'wallet-held-rewards'
      );
      return _createWalletFromServerData(JSON.parse(fixture.response.body));
    };

    it('reports that only funds other than the rewards count when the fee step is refused for lack of funds', async () => {
      const exchange = loadRecordedExchange(
        'ledger-vote',
        'payment-fee-held-not-enough'
      );
      const wallet = heldWallet();
      expect(wallet.heldReward.gt(0)).toBe(true);
      const body = exchange.request.body as {
        payments: Array<{ address: string; amount: { quantity: number } }>;
      };
      const api = await replayRoute('POST', /\/payment-fees$/, exchange);
      const error = await api
        .calculateTransactionFee({
          walletId: exchange.request.path.split('/')[3],
          address: body.payments[0].address,
          amount: body.payments[0].amount.quantity,
          isLegacy: false,
          withdrawal: null,
          ...balancesFor(wallet, null),
        })
        .catch((e) => e);
      expect(error.id).toBe('api.errors.notEnoughFundsRewardsHeld');
    });

    it('reports it from the pre-check when the estimate succeeds but the spendable balance falls short', async () => {
      const estimate = loadRecordedExchange(
        'ledger-vote',
        'payment-fee-nowdrl'
      );
      const wallet = heldWallet();
      const spendable = balancesFor(wallet, null).walletBalance;
      const api = await replayRoute('POST', /\/payment-fees$/, estimate);
      const error = await api
        .calculateTransactionFee({
          walletId: estimate.request.path.split('/')[3],
          address: (
            estimate.request.body as {
              payments: Array<{ address: string }>;
            }
          ).payments[0].address,
          // lovelace, as the request carries it: the whole spendable balance,
          // which with the estimated fee exceeds it
          amount: spendable.times(1000000).toNumber(),
          isLegacy: false,
          withdrawal: null,
          ...balancesFor(wallet, null),
        })
        .catch((e) => e);
      expect(error.id).toBe('api.errors.notEnoughFundsRewardsHeld');
    });
  });

  describe('submission after signing on the cip30 builds', () => {
    it('classifies the node refusal of a software submission as the withdrawal rule', async () => {
      const exchange = loadRecordedExchange('cip30', 'submit-refused');
      const api = await replayRoute('POST', /\/transactions-submit$/, exchange);
      const error = await api
        .submitTransaction({
          walletId: exchange.request.path.split('/')[3],
          transaction: (exchange.request.body as { transaction: string })
            .transaction,
        })
        .catch((e) => e);
      expect(definiteSubmissionFailure(error)).toBe(
        'withdrawal_not_possible_without_vote'
      );
    });

    it('classifies the refusal of a hardware submission as definite', async () => {
      const exchange = loadRecordedExchange('cip30', 'dapp-submission-refused');
      const api = await replayRoute(
        'POST',
        /\/transaction-submission$/,
        exchange
      );
      const error = await api
        .submitDappTransaction({
          walletId: exchange.request.path.split('/')[3],
          request: exchange.request.body as never,
        })
        .catch((e) => e);
      expect(definiteSubmissionFailure(error)).toBe('dapp_submission_failed');
    });
  });
});
