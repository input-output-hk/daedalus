import BigNumber from 'bignumber.js';
import { toJS } from 'mobx';
import Wallet from '../../../source/renderer/app/domains/Wallet';
import type { WalletProps } from '../../../source/renderer/app/domains/Wallet';
import type { DRepDelegation } from '../../../source/renderer/app/api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';

const DREP_TARGET: DRepDelegation = {
  kind: 'drep',
  drep: { raw: KEY_CIP129, credentialType: 'key' },
  source: 'onchain',
};

const makeWallet = (votingTarget?: DRepDelegation | null): Wallet =>
  new Wallet({
    id: 'computeds-wallet',
    addressPoolGap: 20,
    name: 'computeds wallet',
    amount: new BigNumber(0),
    availableAmount: new BigNumber(0),
    reward: new BigNumber(0),
    assets: { available: [], total: [] },
    passwordUpdateDate: null,
    syncState: { status: 'ready' },
    isLegacy: false,
    delegatedStakePoolId: null,
    delegationStakePoolStatus: null,
    lastDelegatedStakePoolId: null,
    lastDelegationStakePoolStatus: null,
    pendingDelegations: [],
    discovery: 'sequential',
    hasPassword: false,
    votingTarget,
  } as WalletProps);

describe('Wallet.currentDRep / Wallet.isVoting', () => {
  it('returns the drep target and isVoting true', () => {
    const wallet = makeWallet(DREP_TARGET);
    expect(toJS(wallet.currentDRep)).toEqual(DREP_TARGET);
    expect(wallet.isVoting).toBe(true);
  });

  it('returns the abstain target and isVoting true', () => {
    const wallet = makeWallet({ kind: 'abstain' });
    expect(toJS(wallet.currentDRep)).toEqual({ kind: 'abstain' });
    expect(wallet.isVoting).toBe(true);
  });

  it('returns the no_confidence target and isVoting true', () => {
    const wallet = makeWallet({ kind: 'no_confidence' });
    expect(toJS(wallet.currentDRep)).toEqual({ kind: 'no_confidence' });
    expect(wallet.isVoting).toBe(true);
  });

  it('returns null and isVoting false for a null target', () => {
    const wallet = makeWallet(null);
    expect(wallet.currentDRep).toBeNull();
    expect(wallet.isVoting).toBe(false);
  });

  it('returns null and isVoting false when votingTarget was never set', () => {
    const wallet = makeWallet();
    expect(wallet.currentDRep).toBeNull();
    expect(wallet.isVoting).toBe(false);
  });

  it('update() propagates a fresh votingTarget onto a stale instance', () => {
    const stale = makeWallet(null);
    stale.update(makeWallet({ kind: 'abstain' }));
    expect(toJS(stale.currentDRep)).toEqual({ kind: 'abstain' });
    expect(stale.isVoting).toBe(true);
  });

  it('update() clears a removed votingTarget instead of sticking stale', () => {
    const stale = makeWallet({ kind: 'no_confidence' });
    stale.update(makeWallet(null));
    expect(stale.currentDRep).toBeNull();
    expect(stale.isVoting).toBe(false);
  });
});
