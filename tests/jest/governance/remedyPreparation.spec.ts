import type AdaApiClass from '../../../source/renderer/app/api/api';
import type VotingStoreClass from '../../../source/renderer/app/stores/VotingStore';
import { noopAnalyticsTracker } from '../../../source/renderer/app/analytics';
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
const VotingStore = require('../../../source/renderer/app/stores/VotingStore')
  .default as typeof VotingStoreClass;

// The wallet's recorded answers to the vote preparation and submission the
// mainnet build sends, served while every request that reads the stake pool
// database is held, as it is while the wallet works on that database.
const construct = loadRecordedExchange('ledger-vote', 'vote-construct');
const submitted = loadRecordedExchange('ledger-vote', 'vote-abstain');

const CHOICES = [
  'abstain',
  'no_confidence',
  'drep1yt0vqun45g7gls09krxat8fqjtnjpy2d0wra7n0pd2ss0mghzumd5',
];

describe('The vote preparation while the stake pool database is busy', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const build = async () => {
    replay = await startWalletReplay([
      { method: 'GET', path: /^\/v2\/stake-pools/, reply: 'hold' },
      { method: 'GET', path: /^\/v2\/settings$/, reply: 'hold' },
      { method: 'GET', path: /^\/v2\/dreps$/, reply: 'hold' },
      { method: 'POST', path: /\/coin-selections\/random$/, reply: 'hold' },
      {
        method: 'POST',
        path: /^\/v2\/wallets\/[^/]+\/transactions-construct$/,
        reply: construct,
      },
      {
        method: 'PUT',
        path: /^\/v2\/dreps\/[^/]+\/wallets\/[^/]+$/,
        reply: submitted,
      },
    ]);
    const store = new VotingStore(
      { ada: new AdaApi(false, replay.config) } as any,
      {} as any,
      noopAnalyticsTracker
    );
    store.configure({
      hardwareWallets: {},
      // The vote path must not look at the stake pool list.
      staking: Object.defineProperty({}, 'stakePools', {
        get: () => {
          throw new Error('the vote path read the stake pool list');
        },
      }),
      transactions: {
        withWalletSendLock: (_id: string, work: () => Promise<unknown>) =>
          work(),
      },
    } as any);
    return store;
  };

  const wallet = {
    id: 'wallet-1',
    isHardwareWallet: false,
    ledgerVote: { status: 'not_delegated' },
  } as any;

  it.each(CHOICES)(
    'prices and submits %s with a vote-only construct and the DRep submission',
    async (chosenOption) => {
      global.cip30Enabled = false;
      const store = await build();

      const prepared = await store.initializeVPDelegationTx({
        chosenOption,
        wallet,
      });
      expect(prepared).toEqual({
        success: true,
        fees: expect.objectContaining({}),
      });
      expect(prepared.success && prepared.fees.toString()).toBe('0.173245');

      const result = await store.delegateVotes({
        chosenOption,
        wallet,
        passphrase: 'password',
      });
      expect(result.success).toBe(true);

      expect(
        replay!.received.map(({ method, path }) => `${method} ${path}`)
      ).toEqual([
        'POST /v2/wallets/wallet-1/transactions-construct',
        `PUT /v2/dreps/${chosenOption}/wallets/wallet-1`,
      ]);
      expect(replay!.received[0].body).toEqual({
        encoding: 'base16',
        vote: chosenOption,
      });
    }
  );
});
