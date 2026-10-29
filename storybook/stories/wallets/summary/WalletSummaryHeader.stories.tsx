import React from 'react';
import { withKnobs, text, number, boolean } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import WalletSummaryHeader from '../../../../source/renderer/app/components/wallet/summary/WalletSummaryHeader';
import WithdrawRewardsDialog from '../../../../source/renderer/app/components/wallet/withdraw-rewards/WithdrawRewardsDialog';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';
import { messages as apiErrorMessages } from '../../../../source/renderer/app/api/errors';
import type { RewardsAction } from '../../../../source/renderer/app/components/wallet/summary/WalletSummaryHeader';
import type { RewardWithdrawalStep } from '../../../../source/renderer/app/stores/WalletsStore';
import StoryDecorator from '../../_support/StoryDecorator';
import StoryProvider from '../../_support/StoryProvider';
import { generateRewardForWallet, generateWallet } from '../../_support/utils';

function WalletSummaryHeaderDecorator(story: any, context: any) {
  const storyWithKnobs = withKnobs(story, context);
  return (
    <StoryDecorator>
      <StoryProvider>{storyWithKnobs}</StoryProvider>
    </StoryDecorator>
  );
}

const readyStep = {
  amount: new BigNumber('36.5'),
  fee: new BigNumber('0.171793'),
};

const withdrawRewardsHeader = (overrides: Partial<RewardsAction>) => {
  const wallet = generateWallet(
    'Wallet name',
    '4564321263',
    undefined,
    '4141123'
  );
  const reward = generateRewardForWallet(wallet, '36500000');
  return (
    <WalletSummaryHeader
      wallet={wallet}
      reward={reward}
      numberOfRecentTransactions={0}
      numberOfPendingTransactions={0}
      isLoadingTransactions={false}
      rewardsAction={{
        disabled: false,
        isSubmitting: false,
        error: null,
        onWithdraw: action('onWithdraw'),
        onDelegate: action('onDelegate'),
        ...overrides,
      }}
    />
  );
};

const withdrawRewardsDialog = (
  step: RewardWithdrawalStep,
  isHardwareWallet: boolean
) => (
  <WithdrawRewardsDialog
    walletName="Wallet name"
    isHardwareWallet={isHardwareWallet}
    step={step}
    hwDeviceStatus={HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED}
    isTrezor={false}
    onSubmit={action('onSubmit')}
    onCancel={action('onCancel')}
    onExternalLinkClick={action('onExternalLinkClick')}
  />
);

storiesOf('Wallets / Summary', module)
  .addDecorator(WalletSummaryHeaderDecorator)
  .add('Wallet Summary Header', () => {
    const rewardsGroup = 'Rewards';
    const wallet = generateWallet(
      'Wallet name',
      text('wallet total', '4564321263', rewardsGroup),
      undefined,
      text('rewards total', '4141123', rewardsGroup)
    );
    const reward = generateRewardForWallet(
      wallet,
      text('unspent rewards', '0', rewardsGroup)
    );
    return (
      <WalletSummaryHeader
        wallet={wallet}
        reward={reward}
        numberOfRecentTransactions={number(
          'numberOfRecentTransactions',
          0,
          {},
          rewardsGroup
        )}
        numberOfPendingTransactions={number(
          'numberOfPendingTransactions',
          0,
          {},
          rewardsGroup
        )}
        isLoadingTransactions={boolean(
          'isLoadingTransactions',
          false,
          rewardsGroup
        )}
      />
    );
  })
  .add('Withdraw rewards', () => withdrawRewardsHeader({}))
  .add('Withdraw rewards - voting power not delegated', () =>
    withdrawRewardsHeader({
      disabled: true,
      reason: 'ledger_not_delegated',
    })
  )
  .add('Withdraw rewards dialog - ready, software', () =>
    withdrawRewardsDialog({ status: 'ready', ...readyStep }, false)
  )
  .add('Withdraw rewards dialog - ready, hardware', () =>
    withdrawRewardsDialog({ status: 'ready', ...readyStep }, true)
  )
  .add('Withdraw rewards dialog - rewards not included', () =>
    withdrawRewardsDialog(
      {
        status: 'failed',
        error: apiErrorMessages.rewardWithdrawalNotBuilt as never,
      },
      false
    )
  )
  .add('Withdraw rewards dialog - refused', () =>
    withdrawRewardsDialog(
      {
        status: 'failed',
        error: {
          ...apiErrorMessages.withdrawalNotPossibleWithoutVote,
          values: {},
        } as never,
      },
      false
    )
  );
