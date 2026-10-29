import BigNumber from 'bignumber.js';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../../../../tests/jest/support/walletReplay';
import type {
  RecordedExchange,
  ReplayRoute,
  WalletReplay,
} from '../../../../tests/jest/support/walletReplay';
import { messages } from '../api/errors';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';

jest.mock('@trezor/connect', () => ({}));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));
// The recorded construct is not the transaction a signature would cover, so
// the check that a signed transaction matches the unsigned one is replaced.
jest.mock('../../../common/transactions/nativeSigning', () => ({
  verifySignedNativeTransaction: jest.fn(
    (_unsigned: string, signed: string) => signed
  ),
}));

provideRequestGlobals();
const AdaApi = require('../api/api').default;
const WalletsStore = require('./WalletsStore').default;

const walletOf = (exchange: RecordedExchange) =>
  exchange.request.path.split('/')[3];
const answer = (exchange: RecordedExchange) =>
  JSON.parse(exchange.response.body);

describe.each(['ledger-vote', 'upstream', 'cip30'] as const)(
  'WalletsStore withdraw rewards on the %s recording',
  (set) => {
    const construct = loadRecordedExchange(set, 'withdrawal-only-construct');
    const none = loadRecordedExchange(set, 'withdrawal-only-none');
    const walletId = walletOf(construct);
    let replay: WalletReplay | null = null;
    afterEach(async () => {
      await replay?.close();
      replay = null;
    });

    const build = async (
      exchange: RecordedExchange,
      extraRoutes: Array<ReplayRoute> = [],
      isHardwareWallet = false
    ) => {
      replay = await startWalletReplay([
        { method: 'POST', path: /\/transactions-construct$/, reply: exchange },
        ...extraRoutes,
      ]);
      const events: Array<string> = [];
      const ada = new AdaApi(false, replay.config);
      const signTransaction = jest.spyOn(ada, 'signTransaction');
      const submitTransaction = jest.spyOn(ada, 'submitTransaction');
      const store = new WalletsStore(
        { ada, localStorage: {} } as any,
        {
          dialogs: {
            closeActiveDialog: { trigger: jest.fn(() => events.push('close')) },
          },
        } as unknown as ActionsMap,
        { sendEvent: jest.fn() } as unknown as AnalyticsTracker
      );
      const lock = jest.fn(
        async (_id: string, work: () => Promise<unknown>) => {
          events.push('lock');
          try {
            return await work();
          } finally {
            events.push('unlock');
          }
        }
      );
      store.configure({
        app: { currentRoute: '/wallets' },
        transactions: { withWalletSendLock: lock },
      } as any);
      store.walletsRequest.result = [
        { id: walletId, isHardwareWallet, name: 'W' },
      ] as any;
      store.refreshWalletsData = jest.fn(async () => events.push('refresh'));
      store.goToWalletRoute = jest.fn(() => events.push('route'));
      return { store, events, signTransaction, submitTransaction };
    };

    describe('on the mainnet build, software', () => {
      it('constructs with exactly the withdrawal-only body and shows the amount and fee the wallet answered', async () => {
        const { store } = await build(construct);
        await store.prepareRewardWithdrawal(walletId);
        expect(replay!.received).toHaveLength(1);
        expect(replay!.received[0].body).toEqual({
          encoding: 'base16',
          withdrawal: 'self',
        });
        const recorded = answer(construct);
        const step = store.rewardWithdrawalFor(walletId);
        expect(step.status).toBe('ready');
        expect(step.amount.times(1000000).toNumber()).toBe(
          recorded.coin_selection.withdrawals[0].amount.quantity
        );
        expect(step.fee.times(1000000).toNumber()).toBe(recorded.fee.quantity);
        expect(store._preparedWithdrawals.get(walletId)).toBe(
          recorded.transaction
        );
      });

      it('sends nothing when the wallet built the transaction without the rewards', async () => {
        const { store, signTransaction, submitTransaction } = await build(none);
        await store.prepareRewardWithdrawal(walletId);
        expect(store.rewardWithdrawalFor(walletId)).toMatchObject({
          status: 'failed',
          error: { id: messages.rewardWithdrawalNotBuilt.id },
        });
        expect(store._preparedWithdrawals.has(walletId)).toBe(false);
        await store.submitRewardWithdrawal(walletId, 'passphrase');
        expect(signTransaction).not.toHaveBeenCalled();
        expect(submitTransaction).not.toHaveBeenCalled();
      });

      it('signs the transaction the dialog showed and submits it inside the send lock', async () => {
        const submit = loadRecordedExchange(set, 'withdrawal-only-construct');
        const { store, events, signTransaction, submitTransaction } =
          await build(construct);
        await store.prepareRewardWithdrawal(walletId);
        signTransaction.mockResolvedValue({ transaction: 'signed-cbor' });
        submitTransaction.mockImplementation(async () => {
          events.push('submit');
          return { id: 'tx' };
        });
        await store.submitRewardWithdrawal(walletId, 'passphrase');
        expect(signTransaction).toHaveBeenCalledWith({
          walletId,
          transaction: answer(submit).transaction,
          passphrase: 'passphrase',
        });
        expect(submitTransaction).toHaveBeenCalledWith({
          walletId,
          transaction: 'signed-cbor',
        });
        expect(events).toEqual([
          'lock',
          'submit',
          'unlock',
          'refresh',
          'close',
          'route',
        ]);
        expect(store.rewardWithdrawalFor(walletId)).toBeNull();
      });

      it('keeps the transaction signable after a wrong spending password', async () => {
        const { store, signTransaction, submitTransaction } =
          await build(construct);
        await store.prepareRewardWithdrawal(walletId);
        const wrong = { id: 'api.errors.IncorrectPasswordError' };
        signTransaction.mockRejectedValue(wrong);
        await store.submitRewardWithdrawal(walletId, 'wrong');
        expect(store.rewardWithdrawalFor(walletId)).toMatchObject({
          status: 'ready',
          error: wrong,
        });
        expect(submitTransaction).not.toHaveBeenCalled();
        signTransaction.mockResolvedValue({ transaction: 'signed-cbor' });
        submitTransaction.mockResolvedValue({ id: 'tx' });
        await store.submitRewardWithdrawal(walletId, 'right');
        expect(store.rewardWithdrawalFor(walletId)).toBeNull();
      });

      it('stores an error without a message id as an internal error', async () => {
        const { store, signTransaction, submitTransaction } =
          await build(construct);
        await store.prepareRewardWithdrawal(walletId);
        signTransaction.mockResolvedValue({ transaction: 'signed-cbor' });
        require('../../../common/transactions/nativeSigning').verifySignedNativeTransaction.mockImplementationOnce(
          () => {
            throw new Error('Signed transaction does not match');
          }
        );
        await store.submitRewardWithdrawal(walletId, 'passphrase');
        expect(store.rewardWithdrawalFor(walletId)).toMatchObject({
          status: 'ready',
          error: { id: messages.walletInternalError.id },
        });
        expect(submitTransaction).not.toHaveBeenCalled();
      });

      it('cancels without a trace', async () => {
        const { store } = await build(construct);
        await store.prepareRewardWithdrawal(walletId);
        store.cancelRewardWithdrawal(walletId);
        expect(store.rewardWithdrawalFor(walletId)).toBeNull();
        expect(store._preparedWithdrawals.has(walletId)).toBe(false);
      });
    });
  }
);

describe('WalletsStore withdraw rewards refusals', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    await replay?.close();
    replay = null;
  });
  const construct = loadRecordedExchange(
    'upstream',
    'withdrawal-only-construct'
  );
  const walletId = walletOf(construct);

  const build = async (routes: Array<ReplayRoute>) => {
    replay = await startWalletReplay(routes);
    const ada = new AdaApi(false, replay.config);
    const store = new WalletsStore(
      { ada, localStorage: {} } as any,
      { dialogs: { closeActiveDialog: { trigger: jest.fn() } } } as any,
      { sendEvent: jest.fn() } as any
    );
    store.configure({
      app: { currentRoute: '/wallets' },
      transactions: {
        withWalletSendLock: async (_id: string, work: () => Promise<unknown>) =>
          work(),
      },
    } as any);
    store.walletsRequest.result = [{ id: walletId, name: 'W' }] as any;
    store.refreshWalletsData = jest.fn();
    store.goToWalletRoute = jest.fn();
    return { store, ada };
  };

  it('reads the wallet refusal at construct as the ledger rule', async () => {
    const refused = loadRecordedExchange(
      'ledger-vote',
      'withdrawal-only-refused'
    );
    const { store } = await build([
      { method: 'POST', path: /\/transactions-construct$/, reply: refused },
    ]);
    await store.prepareRewardWithdrawal(walletId);
    expect(store.rewardWithdrawalFor(walletId)).toMatchObject({
      status: 'failed',
      error: { id: messages.withdrawalNotPossibleWithoutVote.id },
    });
  });

  it.each(['upstream', 'cip30'] as const)(
    'reads the node refusal recorded on the %s pin as the ledger rule',
    async (set) => {
      const sent = loadRecordedExchange(set, 'withdrawal-only-construct');
      const refusal = loadRecordedExchange(
        set,
        'withdrawal-only-submit-refused'
      );
      const { store, ada } = await build([
        { method: 'POST', path: /\/transactions-construct$/, reply: sent },
        { method: 'POST', path: /\/transactions-submit$/, reply: refusal },
      ]);
      jest
        .spyOn(ada, 'signTransaction')
        .mockResolvedValue({ transaction: 'signed-cbor' });
      await store.prepareRewardWithdrawal(walletId);
      await store.submitRewardWithdrawal(walletId, 'passphrase');
      expect(store.rewardWithdrawalFor(walletId)).toMatchObject({
        status: 'failed',
        error: { id: messages.withdrawalNotPossibleWithoutVote.id },
      });
      expect(store.refreshWalletsData).not.toHaveBeenCalled();
    }
  );
});

describe('WalletsStore withdraw rewards on the cip30 builds', () => {
  const walletId = 'wallet';
  const setup = (isHardwareWallet: boolean) => {
    const store = new WalletsStore(
      { ada: {} } as any,
      {} as any,
      {
        sendEvent: jest.fn(),
      } as any
    );
    const hardwareSubmit = jest.fn();
    store.configure({
      app: { currentRoute: '/wallets' },
      hardwareWallets: { submitConstructedNativeTransaction: hardwareSubmit },
    } as any);
    store.walletsRequest.result = [{ id: walletId, isHardwareWallet }] as any;
    store.refreshWalletsData = jest.fn();
    const softwareSubmit = jest.fn();
    store.submitConstructedNativeTransaction = softwareSubmit;
    return { store, hardwareSubmit, softwareSubmit };
  };
  const expected = {
    walletId,
    data: { encoding: 'base16', withdrawal: 'self' },
    action: 'reward-withdrawal',
    requireWithdrawal: true,
  };

  it('submits through the software approval with the same body', async () => {
    const { store, softwareSubmit, hardwareSubmit } = setup(false);
    softwareSubmit.mockResolvedValue({ id: 'tx' });
    await store.submitRewardWithdrawalNative(walletId);
    expect(softwareSubmit).toHaveBeenCalledWith(expected);
    expect(hardwareSubmit).not.toHaveBeenCalled();
    expect(store.rewardWithdrawalFor(walletId)).toBeNull();
    expect(store.refreshWalletsData).toHaveBeenCalled();
  });

  it('submits through the hardware approval with the same body', async () => {
    const { store, softwareSubmit, hardwareSubmit } = setup(true);
    hardwareSubmit.mockResolvedValue({ id: 'tx' });
    await store.submitRewardWithdrawalNative(walletId);
    expect(hardwareSubmit).toHaveBeenCalledWith(expected);
    expect(softwareSubmit).not.toHaveBeenCalled();
  });

  it('shows a message that carries an id under the button', async () => {
    const { store, softwareSubmit } = setup(false);
    const error = { id: messages.rewardWithdrawalNotBuilt.id };
    softwareSubmit.mockRejectedValue(error);
    await store.submitRewardWithdrawalNative(walletId);
    expect(store.rewardWithdrawalFor(walletId)).toEqual({
      status: 'failed',
      error,
    });
  });

  it('leaves an approval outcome without a message to the approval dialog', async () => {
    const { store, softwareSubmit } = setup(false);
    softwareSubmit.mockRejectedValue(new Error('cancelled'));
    await store.submitRewardWithdrawalNative(walletId);
    expect(store.rewardWithdrawalFor(walletId)).toBeNull();
  });
});

describe('WalletsStore.submitConstructedNativeTransaction requireWithdrawal', () => {
  const none = loadRecordedExchange('cip30', 'withdrawal-only-none');
  const walletId = walletOf(none);
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    global.cip30Enabled = undefined as never;
    await replay?.close();
    replay = null;
  });

  it('throws the not-built message in prepare, before any dialog or context request', async () => {
    replay = await startWalletReplay([
      { method: 'POST', path: /\/transactions-construct$/, reply: none },
    ]);
    const ada = new AdaApi(false, replay.config);
    // The cip30 build constructs through the main process, so the recorded
    // answer is parsed here and handed to the store.
    const constructed = await ada.constructTransaction({
      walletId,
      data: { encoding: 'base16', withdrawal: 'self' },
    });
    global.cip30Enabled = true;
    global.environment = { network: 'preprod' } as never;
    jest.spyOn(ada, 'constructTransaction').mockResolvedValue(constructed);
    const context = jest.spyOn(ada, 'getDappTransactionContext');
    const store = new WalletsStore(
      { ada, localStorage: {} } as any,
      {} as any,
      { sendEvent: jest.fn() } as any
    );
    let prepareError: unknown;
    store.configure({
      app: { currentRoute: '/wallets' },
      networkStatus: { genesisBlockHash: 'a'.repeat(64) },
      walletApproval: {
        nativeTransactions: {
          run: async ({ prepare }: { prepare: () => Promise<unknown> }) => {
            try {
              await prepare();
            } catch (error) {
              prepareError = error;
              throw error;
            }
          },
        },
      },
    } as any);
    store.walletsRequest.result = [{ id: walletId, name: 'W' }] as any;
    await expect(
      store.submitConstructedNativeTransaction({
        walletId,
        data: { encoding: 'base16', withdrawal: 'self' },
        action: 'reward-withdrawal',
        requireWithdrawal: true,
      })
    ).rejects.toMatchObject({ id: messages.rewardWithdrawalNotBuilt.id });
    expect(prepareError).toMatchObject({
      id: messages.rewardWithdrawalNotBuilt.id,
    });
    expect(context).not.toHaveBeenCalled();
  });
});
