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
