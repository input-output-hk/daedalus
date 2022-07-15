import React from 'react';
import { withKnobs, text, number, boolean } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import WalletSummaryHeader from '../../../../source/renderer/app/components/wallet/summary/WalletSummaryHeader';
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
  });
