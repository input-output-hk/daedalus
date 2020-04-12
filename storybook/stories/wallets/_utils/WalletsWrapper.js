// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import startCase from 'lodash/startCase';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';
import { generateWallet } from '../../_support/utils';
import STAKE_POOLS from '../../../../source/renderer/app/config/stakingStakePools.dummy';
import Wallet from '../../../../source/renderer/app/domains/Wallet';

const WALLETS = [
  generateWallet('First Wallet', '1000000000', 0, STAKE_POOLS[0], false),
  generateWallet('Second Wallet', '500000000', 0, STAKE_POOLS[100], true),
  generateWallet('Third Wallet', '100000000', 0, STAKE_POOLS[150], true),
  generateWallet('Fourth Wallet', '50000000', 0, STAKE_POOLS[290], true),
  generateWallet('Fifth Wallet', '7000000', 0, STAKE_POOLS[0], true),
];

export default (story: any, context: any) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () =>
    context.story
      .replace('Wallet UTXO distribution', 'utxo')
      .replace('Wallet Summary', 'summary')
      .toLocaleLowerCase();

  const getActiveWalletFromContext = () => {
    return WALLETS[1];
  };

  const activeWallet: Wallet = getActiveWalletFromContext();
  const { hasPassword, isLegacy, isNotResponding } = activeWallet;

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/wallets" {...context}>
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigation
              activeItem={getItemFromContext()}
              hasPassword={hasPassword}
              isActiveScreen={item => item === getItemFromContext()}
              isLegacy={isLegacy}
              isNotResponding={isNotResponding}
              isSetWalletPasswordDialogOpen={false}
              onWalletNavItemClick={linkTo(context.kind, item => {
                if (item === 'utxo') return 'Wallet UTXO distribution';
                return startCase(item);
              })}
              onSetWalletPassword={() => {}}
              onOpenExternalLink={() => {}}
              onRestartNode={() => {}}
            >
              {storyWithKnobs}
            </WalletWithNavigation>
          ) : (
            storyWithKnobs
          )}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
