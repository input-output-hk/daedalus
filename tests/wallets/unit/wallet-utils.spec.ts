import BigNumber from 'bignumber.js';
import {
  isWalletRewardsWithdrawalPossible,
  shouldShowEmptyWalletWarning,
} from '../../../source/renderer/app/utils/walletUtils';

// As a user using a shelly wallet when sending ADA and the balance
// after the transactions remains < 10 ADA, the following Warning message
// is displayed in the send confirmation dialog

const walletTokens = {
  available: [],
  total: [],
};
const walletSyncState = {
  status: 'ready',
} as const;
const walletPendingDelegations = [
  {
    status: 'delegating' as const,
    changes_at: null,
  },
];
const discovery = 'random' as const;

const testWallet = {
  id: '',
  addressPoolGap: 123,
  name: '',
  amount: new BigNumber(100),
  availableAmount: new BigNumber(100),
  reward: new BigNumber(100),
  assets: walletTokens,
  passwordUpdateDate: null,
  syncState: walletSyncState,
  isLegacy: false,
  delegatedStakePoolId: null,
  delegationStakePoolStatus: null,
  lastDelegatedStakePoolId: null,
  lastDelegationStakePoolStatus: null,
  pendingDelegations: walletPendingDelegations,
  discovery: discovery,
  hasPassword: false,
  walletNotConnected: false,
  isHardwareWallet: false,

  update: () => {},
  hasFunds: true,
  hasAssets: true,
  isRestoring: true,
  isSyncing: true,
  isNotResponding: true,
  isRandom: true,
  isDelegating: true,
  isSequential: true,
  restorationProgress: 0,
} as const;

describe('Function shouldShowEmptyWalletWarning returns:', () => {
  it(`<false> in case the balance after transaction is lower than
    MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS`, () => {
    const transactionAmount = new BigNumber(100);
    const walletBalance = new BigNumber(101);
    expect(
      isWalletRewardsWithdrawalPossible(transactionAmount, walletBalance)
    ).toBe(false);
  });

  it(`<true> in case the balance after transaction is higher than
    MINIMUM_MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS`, () => {
    const transactionAmount = new BigNumber(100);
    const walletBalance = new BigNumber(110);
    expect(
      isWalletRewardsWithdrawalPossible(transactionAmount, walletBalance)
    ).toBe(true);
  });

  // This does not apply if
  //
  // wallet is NOT delegating
  // has no tokens and
  // remain balance after transaction is 0 ADA.

  it(`<true> in case of:
    - remain balance less than MINIMUM_MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS
    - is not Legacy
    - is delegating
    - has no assets left`, () => {
    const totalAmountToSpend = new BigNumber(95);
    const walletBalance = new BigNumber(100);
    const isLegacy = false;
    const isDelegating = true;
    const wallet = {
      amount: walletBalance,
      isLegacy,
      isDelegating,
      ...testWallet,
    } as const;
    const hasAssets = false;
    expect(
      shouldShowEmptyWalletWarning(totalAmountToSpend, wallet, hasAssets)
    ).toBe(true);
  });

  it(`<false> in case of:
    - remain balance less than MINIMUM_MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS
    - is legacy
    - is delegating
    - has no assets left`, () => {
    const totalAmountToSpend = new BigNumber(95);
    const walletBalance = new BigNumber(100);
    const isLegacy = true;
    const isDelegating = true;
    const wallet = {
      amount: walletBalance,
      isLegacy,
      isDelegating,
      ...testWallet,
    };
    const hasAssets = false;
    expect(
      shouldShowEmptyWalletWarning(totalAmountToSpend, wallet, hasAssets)
    ).toBe(false);
  });

  it(`<true> in case of:
    - remain balance less than MINIMUM_MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS
    - is not legacy
    - is not delegating
    - has no assets left`, () => {
    const totalAmountToSpend = new BigNumber(95);
    const walletBalance = new BigNumber(100);
    const isLegacy = false;
    const isDelegating = false;
    const wallet = { amount: walletBalance, isLegacy, isDelegating };
    const hasAssets = false;
    expect(
      shouldShowEmptyWalletWarning(totalAmountToSpend, testWallet, hasAssets)
    ).toBe(true);
  });

  it(`<false> in case of:
    - remain balance far more than MINIMUM_MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS
    - is not Legacy
    - is delegating
    'has no assets left`, () => {
    const totalAmountToSpend = new BigNumber(95);
    const walletBalance = new BigNumber(200);
    const isLegacy = false;
    const isDelegating = true;
    const wallet = {
      amount: walletBalance,
      isLegacy,
      isDelegating,
      ...testWallet,
    };
    const hasAssets = false;
    expect(
      shouldShowEmptyWalletWarning(totalAmountToSpend, wallet, hasAssets)
    ).toBe(false);
  });

  it(`<true> in case of:
    - remain balance less than MINIMUM_MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS
    - is not Legacy
    - is delegating
    - has assets left`, () => {
    const totalAmountToSpend = new BigNumber(95);
    const walletBalance = new BigNumber(100);
    const isLegacy = false;
    const isDelegating = true;
    const wallet = {
      amount: walletBalance,
      isLegacy,
      isDelegating,
      ...testWallet,
    };
    const hasAssets = true;
    expect(
      shouldShowEmptyWalletWarning(totalAmountToSpend, wallet, hasAssets)
    ).toBe(true);
  });
});
