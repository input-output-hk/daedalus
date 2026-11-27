import VotingStore from './VotingStore';
import type { Api } from '../api';
import { noopAnalyticsTracker } from '../analytics';

const buildStore = (cip30Enabled: boolean) => {
  global.cip30Enabled = cip30Enabled;
  let locked = false;
  const lockedDuringSubmit: Array<boolean> = [];
  const createVotingRegistrationTransaction = jest.fn(async () => {
    lockedDuringSubmit.push(locked);
    return { id: 'registration' };
  });
  const createWalletSignature = jest.fn(async () => Buffer.from('abcd', 'hex'));
  const store = new VotingStore(
    {
      ada: { createVotingRegistrationTransaction, createWalletSignature },
    } as unknown as Api,
    {} as never,
    noopAnalyticsTracker
  );
  const withWalletSendLock = jest.fn(
    async (_walletId: string, work: () => Promise<unknown>) => {
      locked = true;
      try {
        return await work();
      } finally {
        locked = false;
      }
    }
  );
  store.configure({
    transactions: { withWalletSendLock },
    addresses: {
      getAddressesByWalletId: async () => [{ id: 'addr_test1' }],
      getAutomaticReceivingAddress: async () => ({ id: 'addr_test1' }),
    },
    wallets: { getWalletById: () => ({ isHardwareWallet: false }) },
    networkStatus: { absoluteSlotNumber: 100 },
  } as unknown as Parameters<VotingStore['configure']>[0]);
  store.selectedWalletId = 'wallet';
  store.prepareVotingData = jest.fn(async () => ({
    addressHex: 'aa',
    votingKey: 'bb',
    stakeKey: 'cc',
    role: 'mutable_account',
    index: '0',
  })) as unknown as VotingStore['prepareVotingData'];
  store._startTransactionPolling = jest.fn();
  store._nextRegistrationStep = jest.fn();
  return {
    store,
    withWalletSendLock,
    lockedDuringSubmit,
    createVotingRegistrationTransaction,
  };
};

describe('VotingStore registration transaction', () => {
  const originalCapability = global.cip30Enabled;
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  it('sends the registration while holding the wallet send lock', async () => {
    const fixture = buildStore(false);
    await fixture.store._sendTransaction({
      amount: 1000000,
      passphrase: 'password',
    });
    expect(fixture.withWalletSendLock).toHaveBeenCalledWith(
      'wallet',
      expect.any(Function)
    );
    expect(fixture.createVotingRegistrationTransaction).toHaveBeenCalledTimes(
      1
    );
    expect(fixture.lockedDuringSubmit).toEqual([true]);
    expect(fixture.store.transactionId).toBe('registration');
  });

  it('keeps the CIP-30 build outside the store-level lock', async () => {
    const fixture = buildStore(true);
    await fixture.store._sendTransaction({
      amount: 1000000,
      passphrase: 'password',
    });
    expect(fixture.withWalletSendLock).not.toHaveBeenCalled();
    expect(fixture.createVotingRegistrationTransaction).toHaveBeenCalledTimes(
      1
    );
  });
});
