import CollateralStore from './CollateralStore';
import { dappCollateralChannel } from '../ipc/collateral';

jest.mock('../ipc/collateral', () => ({
  dappCollateralChannel: { request: jest.fn() },
}));

const request = dappCollateralChannel.request as jest.Mock;
const snapshot = (state: 'ready' | 'preparing' | 'not-ready') => ({
  corrupt: false,
  preference: {
    schemaVersion: 1 as const,
    walletId: 'ab'.repeat(20),
    networkGenesis: 'cd'.repeat(32),
    targetLovelace: '5000000',
    preferredInputs: [],
    generation: 1,
    state,
  },
});

const createStore = () => {
  const actions = { router: { goToRoute: { trigger: jest.fn() } } };
  const store = new CollateralStore(
    null as never,
    actions as never,
    null as never
  );
  store.configure({
    wallets: {
      activeDappWallet: { id: 'wallet-a' },
      getWalletRoute: (id: string, page: string) => `/wallets/${id}/${page}`,
    },
  } as never);
  return { actions, store };
};

describe('CollateralStore', () => {
  beforeEach(() => jest.clearAllMocks());

  it('retains the exact main-owned projection without a writable copy', async () => {
    const projected = snapshot('ready');
    request.mockResolvedValue(projected);
    const { store } = createStore();

    await store.refresh();

    expect(request).toHaveBeenCalledWith({ type: 'snapshot' });
    expect(store.snapshot).toEqual(projected);
    expect(store.state).toBe('ready');
  });

  it('detects an exact preferred input in an ordinary selection', async () => {
    const projected = snapshot('ready');
    (projected.preference.preferredInputs as Array<{
      transactionId: string;
      index: number;
    }>).push({ transactionId: '12'.repeat(32), index: 1 });
    request.mockResolvedValue(projected);
    const { store } = createStore();
    await store.refresh();

    expect(store.spendsPreference([{ id: '12'.repeat(32), index: 1 }])).toBe(
      true
    );
    expect(store.spendsPreference([{ id: '12'.repeat(32), index: 0 }])).toBe(
      false
    );
  });

  it('opens the normal send route only after main starts preparation', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { actions, store } = createStore();

    await store.prepare();

    expect(request).toHaveBeenCalledWith({ type: 'prepare' });
    expect(actions.router.goToRoute.trigger).toHaveBeenCalledWith({
      route: '/wallets/wallet-a/send',
    });
    expect(store.preparationFormActive).toBe(true);
  });

  it('tracks the submitted preparation and leaves preference authority in main', async () => {
    request.mockResolvedValue(snapshot('preparing'));
    const { store } = createStore();
    store.preparationFormActive = true;

    await store.trackPreparation('12'.repeat(32));

    expect(request).toHaveBeenCalledWith({
      type: 'track-preparation',
      transactionId: '12'.repeat(32),
    });
    expect(store.preparationFormActive).toBe(false);
  });

  it('clears only through authenticated main IPC', async () => {
    request.mockResolvedValue(snapshot('not-ready'));
    const { store } = createStore();

    await store.clear();

    expect(request).toHaveBeenCalledWith({ type: 'clear' });
    expect(store.state).toBe('not-ready');
  });
});
