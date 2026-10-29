import WalletSettingsStore from './WalletSettingsStore';

const deferred = <T>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((onResolve) => {
    resolve = onResolve;
  });
  return { promise, resolve };
};

const create = () => {
  const walletA = {
    id: 'wallet-a',
    isLegacy: false,
    isHardwareWallet: true,
    singleAddressMode: false,
  };
  const walletB = {
    id: 'wallet-b',
    isLegacy: false,
    isHardwareWallet: false,
    singleAddressMode: false,
  };
  const wallets = [walletA, walletB];
  const updateWallet = jest.fn();
  const refreshWalletsData = jest.fn();
  const withWalletSendLock = jest.fn(
    async (_walletId: string, work: () => Promise<unknown>) => work()
  );
  const walletStore = {
    active: walletA,
    getWalletById: (id: string) => wallets.find((wallet) => wallet.id === id),
    walletsRequest: {
      patch: async (change: (result: typeof wallets) => void) =>
        change(wallets),
    },
    refreshWalletsData,
  };
  const store = new WalletSettingsStore(
    { ada: { updateWallet } } as never,
    null as never,
    null as never
  );
  store.configure({
    wallets: walletStore,
    transactions: { withWalletSendLock },
  } as never);
  return {
    store,
    updateWallet,
    walletA,
    walletB,
    walletStore,
    refreshWalletsData,
    withWalletSendLock,
  };
};

describe('WalletSettingsStore single-address mode', () => {
  beforeEach(() => {
    global.cip30Enabled = true;
  });

  it('rejects a disabled-package mutation before locking or HTTP', async () => {
    global.cip30Enabled = false;
    const fixture = create();
    await expect(
      fixture.store._setSingleAddressMode({
        walletId: fixture.walletA.id,
        enabled: true,
      })
    ).rejects.toThrow('Single-address mode is unavailable');
    expect(fixture.updateWallet).not.toHaveBeenCalled();
    expect(fixture.withWalletSendLock).not.toHaveBeenCalled();
    expect(fixture.walletA.singleAddressMode).toBe(false);
  });

  it('updates only the dispatched wallet when the active wallet changes', async () => {
    const fixture = create();
    const response = deferred<typeof fixture.walletA>();
    fixture.updateWallet.mockReturnValue(response.promise);
    const originalWallet = fixture.walletA;

    const pending = fixture.store._setSingleAddressMode({
      walletId: fixture.walletA.id,
      enabled: true,
    });
    fixture.walletStore.active = fixture.walletB;
    response.resolve({ ...fixture.walletA, singleAddressMode: true });
    await pending;

    expect(fixture.walletA).toBe(originalWallet);
    expect(fixture.walletA.singleAddressMode).toBe(true);
    expect(fixture.walletA.isHardwareWallet).toBe(true);
    expect(fixture.walletB.singleAddressMode).toBe(false);
    expect(fixture.refreshWalletsData).toHaveBeenCalledTimes(1);
  });

  it('retains the displayed choice on rejection and clears the error after success', async () => {
    const fixture = create();
    fixture.updateWallet.mockRejectedValueOnce(new Error('backend rejected'));

    await expect(
      fixture.store._setSingleAddressMode({
        walletId: fixture.walletA.id,
        enabled: true,
      })
    ).rejects.toThrow('backend rejected');
    expect(fixture.walletA.singleAddressMode).toBe(false);
    expect(fixture.store.updateWalletRequest.error).toEqual(
      new Error('backend rejected')
    );

    fixture.updateWallet.mockResolvedValueOnce({
      ...fixture.walletA,
      singleAddressMode: false,
    });
    await fixture.store._setSingleAddressMode({
      walletId: fixture.walletA.id,
      enabled: false,
    });
    expect(fixture.store.updateWalletRequest.error).toBeNull();
    expect(fixture.walletA.singleAddressMode).toBe(false);
  });

  it('rejects same-tick duplicates and preserves false in the request', async () => {
    const fixture = create();
    const response = deferred<typeof fixture.walletA>();
    fixture.updateWallet.mockReturnValue(response.promise);

    const first = fixture.store._setSingleAddressMode({
      walletId: fixture.walletA.id,
      enabled: false,
    });
    await fixture.store._setSingleAddressMode({
      walletId: fixture.walletA.id,
      enabled: true,
    });
    expect(fixture.updateWallet).toHaveBeenCalledTimes(1);
    expect(fixture.updateWallet).toHaveBeenCalledWith({
      walletId: fixture.walletA.id,
      isLegacy: false,
      singleAddressMode: false,
    });
    expect(fixture.withWalletSendLock).toHaveBeenCalledWith(
      fixture.walletA.id,
      expect.any(Function)
    );

    response.resolve({ ...fixture.walletA, singleAddressMode: false });
    await first;
    expect(fixture.walletA.singleAddressMode).toBe(false);
  });
});

describe('WalletSettingsStore withdraw rewards', () => {
  const createWithLocal = (
    stored: boolean | undefined = undefined,
    all: Record<string, { withdrawRewards?: boolean | null }> = {}
  ) => {
    const localStorage = {
      getWithdrawRewardsDefault: jest.fn(async () => stored ?? true),
      setWithdrawRewardsDefault: jest.fn(async () => undefined),
    };
    const trigger = jest.fn(async () => undefined);
    const store = new WalletSettingsStore(
      { ada: {}, localStorage } as never,
      { walletsLocal: { setWalletLocalData: { trigger } } } as never,
      null as never
    );
    store.configure({ walletsLocal: { all } } as never);
    return { store, localStorage, trigger };
  };

  it('is on when nothing is stored', async () => {
    const { store } = createWithLocal();
    expect(store.withdrawRewardsDefault).toBe(true);
    await store._loadWithdrawRewardsDefault();
    expect(store.withdrawRewardsDefault).toBe(true);
  });

  it('loads a stored off value', async () => {
    const { store } = createWithLocal(false);
    await store._loadWithdrawRewardsDefault();
    expect(store.withdrawRewardsDefault).toBe(false);
  });

  it('persists a change to the global setting', async () => {
    const { store, localStorage } = createWithLocal();
    await store._setWithdrawRewardsDefault({ value: false });
    expect(localStorage.setWithdrawRewardsDefault).toHaveBeenCalledWith(false);
    expect(store.withdrawRewardsDefault).toBe(false);
  });

  it('follows the global setting for a wallet with no choice of its own', async () => {
    const { store } = createWithLocal(undefined, {
      none: {},
      cleared: { withdrawRewards: null },
    });
    expect(store.withdrawRewardsFor('none')).toBe(true);
    expect(store.withdrawRewardsFor('cleared')).toBe(true);
    expect(store.withdrawRewardsFor('unknown')).toBe(true);
    await store._setWithdrawRewardsDefault({ value: false });
    expect(store.withdrawRewardsFor('none')).toBe(false);
    expect(store.withdrawRewardsFor('cleared')).toBe(false);
  });

  it('uses the wallet choice over the global setting', async () => {
    const { store } = createWithLocal(undefined, {
      on: { withdrawRewards: true },
      off: { withdrawRewards: false },
    });
    expect(store.withdrawRewardsFor('off')).toBe(false);
    await store._setWithdrawRewardsDefault({ value: false });
    expect(store.withdrawRewardsFor('on')).toBe(true);
  });

  it('follows the global setting again once the wallet local data is gone', () => {
    const all: Record<string, { withdrawRewards?: boolean | null }> = {
      w: { withdrawRewards: false },
    };
    const { store } = createWithLocal(undefined, all);
    expect(store.withdrawRewardsFor('w')).toBe(false);
    delete all.w;
    expect(store.withdrawRewardsFor('w')).toBe(true);
  });

  it('stores the wallet choice in its local data and clears it with null', async () => {
    const { store, trigger } = createWithLocal();
    await store._setWalletWithdrawRewards({ walletId: 'w', value: false });
    expect(trigger).toHaveBeenLastCalledWith({
      walletId: 'w',
      updatedWalletData: { withdrawRewards: false },
    });
    await store._setWalletWithdrawRewards({ walletId: 'w', value: null });
    expect(trigger).toHaveBeenLastCalledWith({
      walletId: 'w',
      updatedWalletData: { withdrawRewards: null },
    });
  });
});
