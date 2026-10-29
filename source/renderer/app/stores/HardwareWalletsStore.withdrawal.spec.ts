import { runInAction } from 'mobx';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../../../../tests/jest/support/walletReplay';
import type {
  RecordedExchange,
  WalletReplay,
} from '../../../../tests/jest/support/walletReplay';
import {
  signTransactionLedgerChannel,
  signTransactionTrezorChannel,
} from '../ipc/getHardwareWalletChannel';
import { HwDeviceStatuses } from '../domains/Wallet';
import { messages } from '../api/errors';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';

jest.mock('@trezor/connect', () => {
  const names = new Proxy({}, { get: (_target, name) => String(name) });
  return {
    PROTO: {
      CardanoCertificateType: names,
      CardanoDRepType: names,
      CardanoTxSigningMode: names,
    },
  };
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

const walletOf = (exchange: RecordedExchange) =>
  exchange.request.path.split('/')[3];

describe.each(['ledger-vote', 'upstream', 'cip30'] as const)(
  'HardwareWalletsStore withdrawal preparation on the %s recording',
  (set) => {
    const construct = loadRecordedExchange(set, 'withdrawal-only-construct');
    const none = loadRecordedExchange(set, 'withdrawal-only-none');
    let replay: WalletReplay | null = null;
    afterEach(async () => {
      jest.restoreAllMocks();
      await replay?.close();
      replay = null;
    });

    const build = async (exchange: RecordedExchange) => {
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
        addresses: {
          _inspectAddress: jest
            .fn()
            .mockResolvedValue({ address_style: 'Shelley' }),
        },
        networkStatus: { absoluteSlotNumber: 100 },
      } as any;
      runInAction(() => {
        store.hardwareWalletsLocalDataRequest.result = {
          [walletId]: {
            id: walletId,
            extendedPublicKey: { publicKeyHex: '00', chainCodeHex: '00' },
          },
        } as any;
      });
      return { store, walletId, release, acquireWalletSendLock };
    };

    it('holds the send lease and sets the recorded construct as the device request', async () => {
      const { store, walletId, acquireWalletSendLock } = await build(construct);

      const { fee, coinSelection } = await store.selectWithdrawalCoins({
        walletId,
      });

      expect(acquireWalletSendLock).toHaveBeenCalledWith(walletId);
      expect(fee.gt(0)).toBe(true);
      expect(store.txSignRequest.coinSelection).toEqual(coinSelection);
      expect(coinSelection.withdrawals).toHaveLength(1);
      expect(coinSelection.withdrawals[0].derivationPath).toEqual([
        '1852H',
        '1815H',
        '0H',
        '2',
        '0',
      ]);
      expect(coinSelection.certificates).toEqual([]);
      expect(replay!.received).toEqual([
        expect.objectContaining({
          path: `/v2/wallets/${walletId}/transactions-construct`,
          body: { encoding: 'base16', withdrawal: 'self' },
        }),
      ]);
    });

    it('does not select a payment again when the device flow starts', async () => {
      const { store, walletId } = await build(construct);
      await store.selectWithdrawalCoins({ walletId });
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

    it('gives the Ledger one withdrawal with its path and an output for each in the selection', async () => {
      const { store, walletId } = await build(construct);
      const { coinSelection } = await store.selectWithdrawalCoins({ walletId });
      const request = jest
        .spyOn(signTransactionLedgerChannel, 'request')
        .mockRejectedValue(new Error('device stopped'));

      await expect(
        store._signTransactionLedger(walletId, 'device')
      ).rejects.toThrow('device stopped');

      const sent = request.mock.calls[0][0];
      expect(sent.withdrawals).toHaveLength(1);
      expect(sent.withdrawals[0]).toMatchObject({
        amount: String(coinSelection.withdrawals[0].amount.quantity),
        stakeCredential: {
          keyPath: [2147485500, 2147485463, 2147483648, 2, 0],
        },
      });
      expect(sent.outputs).toHaveLength(coinSelection.outputs.length);
      expect(sent.outputs.every((output: any) => output.destination)).toBe(
        true
      );
    });

    it('gives the Trezor one withdrawal with its path', async () => {
      const { store, walletId } = await build(construct);
      const { coinSelection } = await store.selectWithdrawalCoins({ walletId });
      const request = jest
        .spyOn(signTransactionTrezorChannel, 'request')
        .mockRejectedValue(new Error('device stopped'));

      await expect(
        store._signTransactionTrezor(walletId, 'device')
      ).rejects.toThrow('device stopped');

      const sent = request.mock.calls[0][0];
      expect(sent.withdrawals).toHaveLength(1);
      expect(sent.withdrawals[0]).toMatchObject({
        path: "m/1852'/1815'/0'/2/0",
        amount: String(coinSelection.withdrawals[0].amount.quantity),
      });
      expect(sent.certificates).toEqual([]);
    });

    it('releases the lease and refuses a selection the wallet built without the rewards', async () => {
      const { store, walletId, release } = await build(none);

      await expect(
        store.selectWithdrawalCoins({ walletId })
      ).rejects.toMatchObject({
        id: messages.rewardWithdrawalNotBuilt.id,
      });

      expect(release).toHaveBeenCalledTimes(1);
      expect(store.txSignRequest.coinSelection).toBeUndefined();
      expect(store.hwDeviceStatus).not.toBe(
        HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED
      );
    });
  }
);
