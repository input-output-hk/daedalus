import AddressesStore from './AddressesStore';
import StakingStore from './StakingStore';
import VotingStore from './VotingStore';
import WalletAddress from '../domains/WalletAddress';
import { noopAnalyticsTracker } from '../analytics';

const address = (id: string, used: boolean, index: number) =>
  new WalletAddress({ id, used, spendingPath: `1852'/1815'/0'/0/${index}` });

const receiving = () => {
  const first = address('first', false, 0);
  const unused = address('last-unused', false, 1);
  const used = address('last-used', true, 2);
  const list = [first, unused, used];
  const wallet = { id: 'wallet', isLegacy: false, singleAddressMode: true };
  const getAddresses = jest.fn().mockResolvedValue(list);
  const store = new AddressesStore(
    { ada: { getAddresses } } as never,
    null as never,
    noopAnalyticsTracker
  );
  store.configure({
    wallets: { active: wallet, getWalletById: () => wallet },
  } as never);
  jest
    .spyOn(store, '_getAddressesAllRequest')
    .mockReturnValue({ result: list } as never);
  return { store, first, unused, used, list, getAddresses };
};

describe('upstream receiving compatibility', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });

  it('ignores stale single-address state and retains generated, unused, used ordering', () => {
    const fixture = receiving();
    expect(fixture.store.active?.id).toBe(fixture.unused.id);
    fixture.store.lastGeneratedAddress = address('generated', false, 3);
    expect(fixture.store.active?.id).toBe('generated');
    fixture.store.lastGeneratedAddress.used = true;
    fixture.first.used = true;
    fixture.unused.used = true;
    expect(fixture.store.active?.id).toBe(fixture.used.id);
  });

  it('keeps pilot first-address selection without enabling it for upstream', async () => {
    const fixture = receiving();
    global.cip30Enabled = true;
    expect(fixture.store.active?.id).toBe(fixture.first.id);
    expect(
      (await fixture.store.getAutomaticReceivingAddress('wallet')).id
    ).toBe(fixture.first.id);
  });

  it('uses cached baseline reward addresses for both fee calculation and redemption', async () => {
    const getAddressesByWalletId = jest
      .fn()
      .mockResolvedValue([{ id: 'baseline' }]);
    const getAutomaticReceivingAddress = jest
      .fn()
      .mockRejectedValue(new Error('pilot only'));
    const getRedeemItnRewardsFee = jest.fn().mockResolvedValue(1);
    const requestRedeemItnRewards = jest.fn().mockResolvedValue(5);
    const store = new StakingStore(
      { ada: { getRedeemItnRewardsFee, requestRedeemItnRewards } } as never,
      null as never,
      noopAnalyticsTracker
    );
    store.configure({
      wallets: { getWalletById: () => ({ id: 'wallet' }) },
      addresses: { getAddressesByWalletId, getAutomaticReceivingAddress },
    } as never);
    await store._onCalculateRedeemWalletFees({
      walletId: 'wallet',
      recoveryPhrase: ['word'],
    });
    await store._onConfirmationContinue({ spendingPassword: 'password' });
    expect(getRedeemItnRewardsFee).toHaveBeenCalledWith(
      expect.objectContaining({ address: 'baseline' })
    );
    expect(requestRedeemItnRewards).toHaveBeenCalledWith(
      expect.objectContaining({
        address: 'baseline',
        spendingPassword: 'password',
      })
    );
    expect(store.redeemSuccess).toBe(true);
    expect(getAutomaticReceivingAddress).not.toHaveBeenCalled();
  });

  it.each([false, true])(
    'preserves Catalyst metadata and address source for pilot=%s',
    async (pilot) => {
      global.cip30Enabled = pilot;
      const getAddressesByWalletId = jest
        .fn()
        .mockResolvedValue([{ id: 'baseline' }]);
      const getAutomaticReceivingAddress = jest
        .fn()
        .mockResolvedValue({ id: 'pilot' });
      const getWalletPublicKey = jest.fn().mockResolvedValue('stake-key');
      const store = new VotingStore(
        { ada: { getWalletPublicKey } } as never,
        null as never,
        noopAnalyticsTracker
      );
      store.configure({
        addresses: { getAddressesByWalletId, getAutomaticReceivingAddress },
        networkStatus: { absoluteSlotNumber: 123 },
      } as never);
      store._generateVotingRegistrationKey = async () => {
        store._setVotingRegistrationKey({
          bytes: () => new Uint8Array([1]),
          public: () => ({ bytes: () => new Uint8Array([2]) }),
        });
      };
      store._getHexFromBech32 = jest.fn(
        async (value: string) => `${value}-hex`
      );
      const result = await store.prepareVotingData({ walletId: 'wallet' });
      const expected = pilot ? 'pilot-hex' : 'baseline-hex';
      expect(result.addressHex).toBe(expected);
      expect(result.metadata[61284].map).toContainEqual({
        k: { int: 3 },
        v: { bytes: expected },
      });
      expect(result.metadata[61284].map).toContainEqual({
        k: { int: 4 },
        v: { int: 123 },
      });
      expect(
        pilot ? getAddressesByWalletId : getAutomaticReceivingAddress
      ).not.toHaveBeenCalled();
    }
  );
});
