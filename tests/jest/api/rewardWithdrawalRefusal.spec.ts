import type AdaApiClass from '../../../source/renderer/app/api/api';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../support/walletReplay';
import type { WalletReplay } from '../support/walletReplay';

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

// cardano-wallet v2026-10-07 answering a payment and a hardware-wallet
// submission from a reward account whose DRep retired. The ledger refuses the
// withdrawal (ConwayWdrlNotDelegatedToDRep) and the wallet reports it as
// HTTP 500 created_invalid_transaction.
const refusedPayment = loadRecordedExchange('upstream', 'payment-refused');
const refusedSubmission = loadRecordedExchange('upstream', 'proxy-refused');

const DREP_MESSAGE = 'api.errors.withdrawalNotPossibleWithoutVote';

const paymentRequest = () => {
  const { request } = refusedPayment;
  const body = request.body as {
    payments: Array<{ address: string; amount: { quantity: number } }>;
  };
  return {
    walletId: request.path.split('/')[3],
    address: body.payments[0].address,
    amount: body.payments[0].amount.quantity,
    passphrase: 'Secret-passphrase-1',
    isLegacy: false,
    hasAssetsRemainingAfterTransaction: false,
    withdrawal: 'self' as const,
  };
};

describe('AdaApi', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const replayRefusals = async () => {
    replay = await startWalletReplay([
      {
        method: 'POST',
        path: /^\/v2\/wallets\/[0-9a-f]{40}\/transactions\/?$/,
        reply: refusedPayment,
      },
      {
        method: 'POST',
        path: /^\/v2\/proxy\/transactions$/,
        reply: refusedSubmission,
      },
    ]);
    return new AdaApi(false, replay.config);
  };

  it('hands the recorded refusal to its error mapping as created_invalid_transaction naming ConwayWdrlNotDelegatedToDRep', async () => {
    expect(refusedPayment.response.status).toBe(500);
    const api = await replayRefusals();
    const error = await api.createTransaction(paymentRequest()).catch((e) => e);
    expect(error.values).toMatchObject({
      code: 'created_invalid_transaction',
      message: expect.stringContaining('ConwayWdrlNotDelegatedToDRep'),
    });
  });

  it('createTransaction reports the refused withdrawal with the DRep delegation message', async () => {
    const api = await replayRefusals();
    const error = await api.createTransaction(paymentRequest()).catch((e) => e);
    expect(error.id).toBe(DREP_MESSAGE);
  });

  it('createExternalTransaction reports the refused withdrawal with the DRep delegation message', async () => {
    expect(refusedSubmission.response.status).toBe(500);
    const api = await replayRefusals();
    const error = await api
      .createExternalTransaction({
        signedTransactionBlob: Buffer.from('84a0a0f5f6', 'hex'),
      })
      .catch((e) => e);
    expect(error.id).toBe(DREP_MESSAGE);
  });
});
