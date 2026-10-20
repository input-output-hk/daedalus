import CollateralStore from './CollateralStore';
import { dappCollateralChannel } from '../ipc/collateral';
import type {
  CollateralSnapshot,
  CollateralState,
} from '../../../common/types/collateral.types';

jest.mock('../ipc/collateral', () => ({
  dappCollateralChannel: { request: jest.fn() },
}));

const request = dappCollateralChannel.request as jest.MockedFunction<
  typeof dappCollateralChannel.request
>;
const preparation = Object.freeze({
  operationId: '11111111-1111-4111-8111-111111111111',
  walletId: 'ab'.repeat(20),
  networkGenesis: 'cd'.repeat(32),
});
const transactionId = '12'.repeat(32);
const snapshot = (state: CollateralState): CollateralSnapshot => ({
  corrupt: false,
  preference: {
    schemaVersion: 1,
    walletId: preparation.walletId,
    networkGenesis: preparation.networkGenesis,
    targetLovelace: '5000000',
    preferredInputs: [],
    generation: 1,
    state,
  },
  ...(state === 'preparing' ? { preparation } : {}),
});
const createStore = () => {
  const actions = { router: { goToRoute: { trigger: jest.fn() } } };
  const wallets = {
    activeDappWallet: { id: 'a-later-active-wallet' },
    getWalletRoute: (id: string, page: string) => `/wallets/${id}/${page}`,
  };
  const store = new CollateralStore(
    null as never,
    actions as never,
    null as never
  );
  store.configure({ wallets } as never);
  return { actions, wallets, store };
};

// The project compiler library predates Promise.withResolvers.
const deferred = <T>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((complete) => {
    resolve = complete;
  });
  return { promise, resolve };
};

describe('CollateralStore', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    request.mockReset();
    global.cip30Enabled = true;
  });
  afterEach(() => jest.useRealTimers());

  it('does not issue collateral IPC or navigate when disabled', async () => {
    global.cip30Enabled = false;
    const { store, actions } = createStore();
    await store.refresh();
    await store.prepare();
    await store.trackPreparation(preparation.walletId, transactionId);
    await store.cancelPreparation();
    await store.clear();
    await store.repair();
    store.teardown();
    expect(request).not.toHaveBeenCalled();
    expect(actions.router.goToRoute.trigger).not.toHaveBeenCalled();
    expect(store.isLoading).toBe(false);
  });

  it('stops checking and reports failure when collateral IPC does not settle', async () => {
    jest.useFakeTimers();
    request.mockReturnValue(deferred<CollateralSnapshot>().promise);
    const { store } = createStore();
    const refresh = store.refresh();
    jest.advanceTimersByTime(30_000);
    await refresh;
    expect(store.isLoading).toBe(false);
    expect(store.actionFailed).toBe(true);
  });

  it('detects only the exact preferred input in an ordinary selection', async () => {
    const projected = snapshot('ready');
    request.mockResolvedValue({
      ...projected,
      preference: {
        ...projected.preference,
        preferredInputs: [{ transactionId, index: 1 }],
      },
    });
    const { store } = createStore();
    await store.refresh();
    expect(store.spendsPreference([{ id: transactionId, index: 1 }])).toBe(
      true
    );
    expect(store.spendsPreference([{ id: transactionId, index: 0 }])).toBe(
      false
    );
  });

  it('captures operation identity before navigating and tracks its originating wallet after route loss', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { actions, wallets, store } = createStore();
    actions.router.goToRoute.trigger.mockImplementation(({ route }) => {
      expect(store.preparation).toEqual(preparation);
      expect(route).toBe(`/wallets/${preparation.walletId}/send`);
      wallets.activeDappWallet = null;
    });
    await store.prepare();
    expect(store.preparationFormActive).toBe(true);
    await store.trackPreparation(preparation.walletId, transactionId);
    expect(request).toHaveBeenLastCalledWith({
      type: 'track-preparation',
      preparation,
      transactionId,
    });
    expect(store.preparation).toEqual(preparation);
    expect(store.preparationFormActive).toBe(false);
    expect(store.state).toBe('preparing');
  });

  it('does not open Send without main-owned preparation authority', async () => {
    request.mockResolvedValue({
      ...snapshot('preparing'),
      preparation: undefined,
    });
    const { actions, store } = createStore();
    await store.prepare();
    expect(actions.router.goToRoute.trigger).not.toHaveBeenCalled();
    expect(store.preparationFormActive).toBe(false);
    expect(store.actionFailed).toBe(true);
  });

  it('rejects missing and wrong originating wallet locally without sending a completion', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { store } = createStore();
    await expect(
      store.trackPreparation(preparation.walletId, transactionId)
    ).rejects.toThrow('does not match wallet');
    expect(request).not.toHaveBeenCalled();
    await store.prepare();
    request.mockClear();
    await expect(
      store.trackPreparation('another-wallet', transactionId)
    ).rejects.toThrow('does not match wallet');
    expect(request).not.toHaveBeenCalled();
    expect(store.actionFailed).toBe(true);
    expect(store.preparation).toEqual(preparation);
  });

  it.each(['rejection', 'timeout'] as const)(
    'retains operation and reports %s so tracking can be retried',
    async (failure) => {
      request.mockResolvedValue(snapshot('preparing'));
      const { store } = createStore();
      await store.prepare();
      if (failure === 'timeout') {
        jest.useFakeTimers();
        request.mockReturnValueOnce(deferred<CollateralSnapshot>().promise);
      } else request.mockRejectedValueOnce(new Error('transport unavailable'));
      const tracking = store.trackPreparation(
        preparation.walletId,
        transactionId
      );
      if (failure === 'timeout') jest.advanceTimersByTime(30_000);
      await tracking;
      expect(store.actionFailed).toBe(true);
      expect(store.preparation).toEqual(preparation);
      expect(store.state).toBe('preparing');
      expect(store.isLoading).toBe(false);
      await store.trackPreparation(preparation.walletId, transactionId);
      expect(request).toHaveBeenLastCalledWith({
        type: 'track-preparation',
        preparation,
        transactionId,
      });
      expect(store.actionFailed).toBe(false);
      expect(store.preparation).toEqual(preparation);
    }
  );

  it('retains failed cancellation authority and clears it only when cancellation succeeds', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { store, wallets } = createStore();
    await store.prepare();
    wallets.activeDappWallet = null;
    request.mockRejectedValueOnce(new Error('transport unavailable'));
    await store.cancelPreparation();
    expect(store.actionFailed).toBe(true);
    expect(store.preparationFormActive).toBe(true);
    expect(store.preparation).toEqual(preparation);
    request.mockResolvedValueOnce(snapshot('checking'));
    await store.cancelPreparation();
    expect(store.preparation).toBeUndefined();
    expect(store.preparationFormActive).toBe(false);
    expect(store.actionFailed).toBe(false);
    expect(store.state).toBe('checking');
  });

  it('only a ready projection for the captured wallet and network clears local preparation', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { store } = createStore();
    await store.prepare();
    const ready = snapshot('ready');
    for (const changed of [
      { walletId: 'ef'.repeat(20) },
      { networkGenesis: 'ef'.repeat(32) },
    ]) {
      request.mockResolvedValueOnce({
        ...ready,
        preference: { ...ready.preference, ...changed },
      });
      await store.refresh();
      expect(store.preparation).toEqual(preparation);
    }
    request.mockResolvedValueOnce(ready);
    await store.refresh();
    expect(store.preparation).toBeUndefined();
    expect(store.preparationFormActive).toBe(false);
  });

  it('teardown cancels the captured operation and late responses cannot restore local authority', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { store } = createStore();
    await store.prepare();
    const late = deferred<CollateralSnapshot>();
    request.mockReturnValueOnce(late.promise);
    const refresh = store.refresh();
    request.mockResolvedValueOnce(snapshot('checking'));
    store.teardown();
    expect(request).toHaveBeenLastCalledWith({
      type: 'cancel-preparation',
      preparation,
    });
    late.resolve(snapshot('preparing'));
    await refresh;
    expect(store.preparation).toBeUndefined();
    expect(store.snapshot).toBeUndefined();
    expect(store.preparationFormActive).toBe(false);
  });
});
