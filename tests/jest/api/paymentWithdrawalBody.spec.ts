import BigNumber from 'bignumber.js';
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

const fee = loadRecordedExchange('ledger-vote', 'payment-fee-nowdrl');
const selection = loadRecordedExchange(
  'ledger-vote',
  'hardware-selection-nowdrl'
);
const payment = loadRecordedExchange('ledger-vote', 'payment-nowdrl');

type PaymentBody = {
  payments: Array<{ address: string; amount: { quantity: number } }>;
};
const recorded = (exchange: typeof fee) => {
  const body = exchange.request.body as PaymentBody;
  return {
    walletId: exchange.request.path.split('/')[3],
    address: body.payments[0].address,
    amount: body.payments[0].amount.quantity,
  };
};
const balances = {
  walletBalance: new BigNumber(100),
  availableBalance: new BigNumber(100),
  rewardsBalance: new BigNumber(10),
};

describe('AdaApi payment requests', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const replayRecordings = async () => {
    replay = await startWalletReplay([
      {
        method: 'POST',
        path: /\/payment-fees$/,
        reply: fee,
      },
      {
        method: 'POST',
        path: /\/coin-selections\/random$/,
        reply: selection,
      },
      { method: 'POST', path: /\/transactions\/?$/, reply: payment },
    ]);
    return new AdaApi(false, replay.config);
  };

  it('prices a payment with the withdrawal key when it is self', async () => {
    const api = await replayRecordings();
    await api.calculateTransactionFee({
      ...recorded(fee),
      ...balances,
      isLegacy: false,
      withdrawal: 'self',
    });
    expect(replay.received[0].body).toEqual({
      ...(fee.request.body as object),
      withdrawal: 'self',
    });
  });

  it('selects hardware payment coins with the withdrawal key when it is self', async () => {
    const api = await replayRecordings();
    await api.selectCoins({
      walletId: recorded(selection).walletId,
      ...balances,
      payments: { ...recorded(selection), withdrawal: 'self' },
    });
    expect(replay.received[0].body).toEqual({
      ...(selection.request.body as object),
      withdrawal: 'self',
    });
  });

  it('sends a payment with the withdrawal key when it is self', async () => {
    const api = await replayRecordings();
    await api.createTransaction({
      ...recorded(payment),
      passphrase: 'Secret-passphrase-1',
      isLegacy: false,
      hasAssetsRemainingAfterTransaction: false,
      withdrawal: 'self',
    });
    expect(replay.received[0].body).toMatchObject({ withdrawal: 'self' });
  });
});
