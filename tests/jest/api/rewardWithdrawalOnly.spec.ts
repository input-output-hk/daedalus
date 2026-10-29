import type AdaApiClass from '../../../source/renderer/app/api/api';
import { messages } from '../../../source/renderer/app/api/errors';
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

const BODY = { encoding: 'base16', withdrawal: 'self' } as const;
const walletIdOf = (exchange: RecordedExchange) =>
  exchange.request.path.split('/')[3];
const recorded = (exchange: RecordedExchange) =>
  JSON.parse(exchange.response.body) as {
    coin_selection: {
      withdrawals: Array<{ amount: { quantity: number } }>;
      outputs: Array<unknown>;
    };
    fee: { quantity: number };
    transaction: string;
  };
const sum = (values: Array<number>) => values.reduce((a, b) => a + b, 0);

describe('The withdrawal-only construct built from a recorded wallet response', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const construct = async (exchange: RecordedExchange) => {
    replay = await startWalletReplay([
      { method: 'POST', path: /\/transactions-construct$/, reply: exchange },
    ]);
    const api = new AdaApi(false, replay.config);
    return api.constructTransaction({
      walletId: walletIdOf(exchange),
      data: BODY,
    });
  };

  describe.each(['ledger-vote', 'upstream', 'cip30'] as const)(
    'on the %s recording',
    (set) => {
      it('sends the body the wallet accepted', async () => {
        const exchange = loadRecordedExchange(set, 'withdrawal-only-construct');
        await construct(exchange);
        expect(replay.received).toHaveLength(1);
        expect(replay.received[0].body).toEqual(exchange.request.body);
        expect(replay.received[0].body).toEqual(BODY);
      });

      it('reads the withdrawal, the fee and the transaction the wallet answered', async () => {
        const exchange = loadRecordedExchange(set, 'withdrawal-only-construct');
        const answer = recorded(exchange);
        const constructed = await construct(exchange);
        expect(constructed.transaction).toBe(answer.transaction);
        expect(constructed.fee.times(1000000).toNumber()).toBe(
          answer.fee.quantity
        );
        expect(constructed.coinSelection.withdrawals).toHaveLength(1);
        expect(
          sum(
            constructed.coinSelection.withdrawals.map(({ amount }) =>
              Number(amount.quantity)
            )
          )
        ).toBe(
          sum(
            answer.coin_selection.withdrawals.map(
              ({ amount }) => amount.quantity
            )
          )
        );
        // Everything the transaction pays goes back to the wallet.
        expect(
          constructed.coinSelection.outputs.every(
            ({ derivationPath }) => derivationPath.length > 0
          )
        ).toBe(true);
      });

      it('reads a construct that left the rewards out as one with no withdrawal', async () => {
        const exchange = loadRecordedExchange(set, 'withdrawal-only-none');
        const constructed = await construct(exchange);
        expect(replay.received[0].body).toEqual(BODY);
        expect(constructed.coinSelection.withdrawals).toHaveLength(0);
      });
    }
  );

  it('reads the wallet refusal of a withdrawal without a vote delegation as the ledger rule', async () => {
    const exchange = loadRecordedExchange(
      'ledger-vote',
      'withdrawal-only-refused'
    );
    await expect(construct(exchange)).rejects.toMatchObject({
      id: messages.withdrawalNotPossibleWithoutVote.id,
    });
  });
});
