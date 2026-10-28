import { Cardano } from '@cardano-sdk/core';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../../../../tests/jest/support/walletReplay';
import type { WalletReplay } from '../../../../tests/jest/support/walletReplay';
import { HwDeviceStatuses } from '../domains/Wallet';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';

// Only the enum names the certificate mappers read.
jest.mock('@trezor/connect', () => {
  const names = new Proxy({}, { get: (_target, name) => String(name) });
  return { PROTO: { CardanoCertificateType: names, CardanoDRepType: names } };
});
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

provideRequestGlobals();
const AdaApi = require('../api/api').default;
const HardwareWalletsStore = require('./HardwareWalletsStore').default;
const { toLedgerCertificate } = require('../utils/shelleyLedger');
const { toTrezorCertificate } = require('../utils/shelleyTrezor');

// The mainnet build asks the wallet for a vote-only construct and hands its
// coin selection to the device flow.
const construct = loadRecordedExchange('ledger-vote', 'vote-construct');
const registering = loadRecordedExchange(
  'ledger-vote',
  'vote-construct-registering-key'
);
const walletOf = (exchange: typeof construct) =>
  exchange.request.path.split('/')[3];
const voteOf = (exchange: typeof construct) =>
  (exchange.request.body as { vote: string }).vote;

describe('HardwareWalletsStore vote preparation', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });

  const build = async (exchange: typeof construct) => {
    const walletId = walletOf(exchange);
    replay = await startWalletReplay([
      {
        method: 'POST',
        path: new RegExp(`^/v2/wallets/${walletId}/transactions-construct$`),
        reply: exchange,
      },
      {
        method: 'POST',
        path: /\/coin-selections\/random$/,
        reply: 'hold',
      },
    ]);
    const release = jest.fn();
    const acquireWalletSendLock = jest.fn().mockResolvedValue({ release });
    const store = new HardwareWalletsStore(
      { ada: new AdaApi(false, replay.config), localStorage: {} } as any,
      {} as ActionsMap,
      { sendEvent: jest.fn() } as unknown as AnalyticsTracker
    );
    store.stores = {
      wallets: { getWalletById: () => ({ id: walletId }) },
      transactions: { acquireWalletSendLock },
    } as any;
    return { store, walletId, release, acquireWalletSendLock };
  };

  it('holds the send lease and sets the recorded construct as the device request', async () => {
    const { store, walletId, acquireWalletSendLock } = await build(construct);
    const vote = voteOf(construct);

    const { fee } = await store.selectVoteCoins({ walletId, vote });

    expect(acquireWalletSendLock).toHaveBeenCalledWith(walletId);
    expect(fee.toString()).toBe('0.173245');
    const { coinSelection } = store.txSignRequest;
    expect(coinSelection.withdrawals).toEqual([]);
    expect(coinSelection.certificates).toEqual([
      expect.objectContaining({
        certificateType: 'cast_vote',
        rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
        vote,
      }),
    ]);
    expect(replay!.received).toEqual([
      expect.objectContaining({
        path: `/v2/wallets/${walletId}/transactions-construct`,
        body: { encoding: 'base16', vote },
      }),
    ]);
  });

  it('puts the vote for the chosen credential on both devices', async () => {
    const { store, walletId } = await build(construct);
    const vote = voteOf(construct);
    await store.selectVoteCoins({ walletId, vote });
    const [certificate] = store.txSignRequest.coinSelection.certificates;
    const { hash } = Cardano.DRepID.toCredential(Cardano.DRepID(vote));

    const ledger = toLedgerCertificate(certificate);
    const trezor = toTrezorCertificate(certificate);

    expect(ledger.params.dRep).toMatchObject({ keyHashHex: hash });
    expect(trezor.dRep).toMatchObject({ keyHash: hash });
  });

  it('keeps the stake key registration the wallet adds when the key is not registered', async () => {
    const { store, walletId } = await build(registering);

    await store.selectVoteCoins({ walletId, vote: voteOf(registering) });

    const { certificates, withdrawals } = store.txSignRequest.coinSelection;
    expect(certificates.map((c: any) => c.certificateType)).toEqual([
      'register_reward_account',
      'cast_vote',
    ]);
    expect(withdrawals).toEqual([]);
  });

  it('does not select a payment again when the device flow starts', async () => {
    const { store, walletId } = await build(construct);
    await store.selectVoteCoins({ walletId, vote: voteOf(construct) });
    const selectCoins = jest.spyOn(store, 'selectCoins');
    const device = jest
      .spyOn(store as any, 'initiateDeviceTransaction')
      .mockResolvedValue(undefined);

    await store.initiateTransaction({ walletId });

    expect(selectCoins).not.toHaveBeenCalled();
    expect(device).toHaveBeenCalledTimes(1);
    expect(
      replay!.received.filter(({ path }) => path.includes('coin-selections'))
    ).toEqual([]);
  });

  it('releases the lease and reports the failure when the wallet refuses the construct', async () => {
    const refused = loadRecordedExchange(
      'ledger-vote',
      'vote-construct-retired'
    );
    const { store, walletId, release } = await build(refused);

    await expect(
      store.selectVoteCoins({ walletId, vote: voteOf(refused) })
    ).rejects.toMatchObject({ values: { code: 'not_found' } });

    expect(release).toHaveBeenCalledTimes(1);
    expect(store.hwDeviceStatus).toBe(
      HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED
    );
  });
});
