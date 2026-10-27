import BigNumber from 'bignumber.js';
import StakingStore from './StakingStore';
import { decideWithdrawal } from '../utils/governance/rewardWithdrawal';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { AnalyticsTracker } from '../analytics';
import type { LedgerVoteDelegation } from '../api/wallets/types';

const QUIT = { quit: { stake_key_index: '0H' } };

const buildStore = (ledgerVote: LedgerVoteDelegation, reward: number) => {
  const store = new StakingStore(
    { ada: {} } as unknown as Api,
    {} as ActionsMap,
    { sendEvent: jest.fn() } as unknown as AnalyticsTracker
  );
  const software = jest.fn().mockResolvedValue({ id: 'software-id' });
  const hardware = jest.fn().mockResolvedValue({ id: 'hardware-id' });
  store.configure({
    wallets: {
      withdrawalDecisionFor: (_walletId: string, purpose: 'undelegation') =>
        decideWithdrawal({
          purpose,
          isLegacy: false,
          ledgerVote,
          reward: new BigNumber(reward),
        }),
      submitConstructedNativeTransaction: software,
    },
    hardwareWallets: { submitConstructedNativeTransaction: hardware },
  } as unknown as StoresMap);
  return { store, software, hardware };
};

const request = (isHardwareWallet: boolean) => ({
  walletId: 'wallet-a',
  stakePoolId: '',
  passphrase: '',
  isHardwareWallet,
});

describe('StakingStore undelegation on the cip30 build', () => {
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
    jest.useFakeTimers();
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
    jest.clearAllTimers();
    jest.useRealTimers();
  });

  it.each([
    ['unknown ledger value', { status: 'unknown' } as const],
    [
      'delegated ledger value',
      { status: 'delegated', voting: { kind: 'abstain' } } as const,
    ],
  ])('sends the withdrawal for a %s with a reward', async (_name, vote) => {
    const software = buildStore(vote, 5);
    await software.store._quitStakePool(request(false));
    expect(software.software).toHaveBeenCalledWith({
      walletId: 'wallet-a',
      action: 'undelegate',
      data: { encoding: 'base16', delegations: [QUIT], withdrawal: 'self' },
    });
    const hardware = buildStore(vote, 5);
    await hardware.store._quitStakePool(request(true));
    expect(hardware.hardware).toHaveBeenCalledWith({
      walletId: 'wallet-a',
      action: 'undelegate',
      data: { encoding: 'base16', delegations: [QUIT], withdrawal: 'self' },
    });
  });

  it('leaves the withdrawal out at a zero reward', async () => {
    const software = buildStore({ status: 'unknown' }, 0);
    await software.store._quitStakePool(request(false));
    expect(software.software).toHaveBeenCalledWith({
      walletId: 'wallet-a',
      action: 'undelegate',
      data: { encoding: 'base16', delegations: [QUIT] },
    });
    const hardware = buildStore({ status: 'not_delegated' }, 0);
    await hardware.store._quitStakePool(request(true));
    expect(hardware.hardware).toHaveBeenCalledWith({
      walletId: 'wallet-a',
      action: 'undelegate',
      data: { encoding: 'base16', delegations: [QUIT] },
    });
  });

  it.each([false, true])(
    'sends nothing when a vote delegation must come first (hardware %s)',
    async (isHardwareWallet) => {
      const fixture = buildStore({ status: 'not_delegated' }, 5);
      await expect(
        fixture.store._quitStakePool(request(isHardwareWallet))
      ).rejects.toThrow('requires a vote delegation first');
      expect(fixture.software).not.toHaveBeenCalled();
      expect(fixture.hardware).not.toHaveBeenCalled();
      expect(fixture.store.isDelegationTransactionPending).toBe(false);
    }
  );
});
