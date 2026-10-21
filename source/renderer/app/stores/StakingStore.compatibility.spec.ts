import StakingStore from './StakingStore';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { AnalyticsTracker } from '../analytics';

const buildStore = () => {
  const joinStakePool = jest.fn().mockResolvedValue({ id: 'join-id' });
  const quitStakePool = jest.fn().mockResolvedValue({ id: 'quit-id' });
  const store = new StakingStore(
    { ada: { joinStakePool, quitStakePool } } as unknown as Api,
    {} as ActionsMap,
    { sendEvent: jest.fn() } as unknown as AnalyticsTracker
  );
  let release: (() => void) | undefined;
  const acquired = new Promise<void>((resolve) => {
    release = resolve;
  });
  const withWalletSendLock = jest.fn(
    async (_walletId: string, work: () => Promise<unknown>) => {
      await acquired;
      return work();
    }
  );
  const sendHardware = jest.fn().mockResolvedValue({ id: 'hardware-id' });
  const exactSoftware = jest.fn().mockResolvedValue({ id: 'exact-id' });
  const exactHardware = jest
    .fn()
    .mockResolvedValue({ id: 'exact-hardware-id' });
  store.configure({
    transactions: { withWalletSendLock },
    wallets: {
      getWalletById: () => ({ isDelegating: false }),
      submitConstructedNativeTransaction: exactSoftware,
      refreshWalletsData: jest.fn(),
    },
    hardwareWallets: {
      _sendMoney: sendHardware,
      submitConstructedNativeTransaction: exactHardware,
    },
  } as unknown as StoresMap);
  return {
    store,
    joinStakePool,
    quitStakePool,
    release: () => release?.(),
    sendHardware,
    exactSoftware,
    exactHardware,
  };
};

const flushRequestTimers = async () => {
  // Request settles through a promise continuation followed by its 1ms timer.
  for (let phase = 0; phase < 5; phase += 1) {
    await Promise.resolve();
    jest.advanceTimersByTime(2);
  }
};

describe('upstream staking transaction dispatch', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
    jest.useFakeTimers();
  });
  afterEach(() => {
    jest.clearAllTimers();
    jest.useRealTimers();
  });

  it.each(['join', 'quit'] as const)(
    'holds %s mutation until the wallet lock and preserves the password',
    async (operation) => {
      const fixture = buildStore();
      const request = {
        walletId: 'wallet',
        stakePoolId: 'pool',
        passphrase: 'collected-password',
        isHardwareWallet: false,
      };
      const pending =
        operation === 'join'
          ? fixture.store._joinStakePool(request)
          : fixture.store._quitStakePool(request);
      expect(fixture.joinStakePool).not.toHaveBeenCalled();
      expect(fixture.quitStakePool).not.toHaveBeenCalled();
      fixture.release();
      await flushRequestTimers();
      await pending;
      const mutation =
        operation === 'join' ? fixture.joinStakePool : fixture.quitStakePool;
      expect(mutation).toHaveBeenCalledWith(request);
      expect(fixture.exactSoftware).not.toHaveBeenCalled();
      expect(fixture.exactHardware).not.toHaveBeenCalled();
      expect(fixture.store.isDelegationTransactionPending).toBe(true);
    }
  );

  it('keeps a wrong-password failure recoverable and never retries through an exact endpoint', async () => {
    const fixture = buildStore();
    fixture.release();
    fixture.joinStakePool.mockRejectedValueOnce(new Error('wrong password'));
    const request = {
      walletId: 'wallet',
      stakePoolId: 'pool',
      passphrase: 'wrong',
      isHardwareWallet: false,
    };
    await Promise.all([
      expect(fixture.store._joinStakePool(request)).rejects.toThrow(
        'wrong password'
      ),
      flushRequestTimers(),
    ]);
    expect(fixture.store.isDelegationTransactionPending).toBe(false);
    const retried = fixture.store._joinStakePool({
      ...request,
      passphrase: 'correct',
    });
    await flushRequestTimers();
    await retried;
    expect(fixture.joinStakePool).toHaveBeenLastCalledWith({
      ...request,
      passphrase: 'correct',
    });
    expect(fixture.exactSoftware).not.toHaveBeenCalled();
  });
  it.each(['join', 'quit'] as const)(
    'submits prepared hardware %s without software signing',
    async (operation) => {
      const fixture = buildStore();
      const request = {
        walletId: 'wallet',
        stakePoolId: 'pool',
        passphrase: '',
        isHardwareWallet: true,
      };
      await (operation === 'join'
        ? fixture.store._joinStakePool(request)
        : fixture.store._quitStakePool(request));
      expect(fixture.sendHardware).toHaveBeenCalledWith({
        isDelegationTransaction: true,
        selectedWalletId: 'wallet',
      });
      expect(fixture.joinStakePool).not.toHaveBeenCalled();
      expect(fixture.quitStakePool).not.toHaveBeenCalled();
      expect(fixture.exactHardware).not.toHaveBeenCalled();
    }
  );

  it('retains pilot approval and does not fall back when exact construction fails', async () => {
    global.cip30Enabled = true;
    const fixture = buildStore();
    fixture.exactSoftware.mockRejectedValueOnce(
      new Error('context unavailable')
    );
    await expect(
      fixture.store._joinStakePool({
        walletId: 'wallet',
        stakePoolId: 'pool',
        passphrase: '',
        isHardwareWallet: false,
      })
    ).rejects.toThrow('context unavailable');
    expect(fixture.joinStakePool).not.toHaveBeenCalled();
    expect(fixture.quitStakePool).not.toHaveBeenCalled();
  });
});
