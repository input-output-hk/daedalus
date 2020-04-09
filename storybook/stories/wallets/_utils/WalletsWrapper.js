// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import startCase from 'lodash/startCase';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';
import DialogsActions from "../../../../source/renderer/app/actions/dialogs-actions";
import { generateWallet } from "../../_support/utils";
import STAKE_POOLS from "../../../../source/renderer/app/config/stakingStakePools.dummy";
import Wallet from "../../../../source/renderer/app/domains/Wallet";

const WALLETS = [
  generateWallet('First Wallet', '1000000000', 0, STAKE_POOLS[0]),
  generateWallet('Second Wallet', '500000000', 0, STAKE_POOLS[100]),
  generateWallet('Third Wallet', '100000000', 0, STAKE_POOLS[150]),
  generateWallet('Fourth Wallet', '50000000', 0, STAKE_POOLS[290]),
  generateWallet('Fifth Wallet', '7000000'),
];

export default (story: any, context: any) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () =>
    context.story
      .replace('Wallet UTXO distribution', 'utxo')
      .replace('Wallet Summary', 'summary')
      .toLocaleLowerCase();

  const activeWallet: Wallet = WALLETS[0];

  const dialogs = new DialogsActions();

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/wallets" {...context}>
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigation
              dialogs={dialogs}
              onOpenExternalLink={() => {}}
              isDialogOpen={() => {}}
              onRestartNode={() => {}}
              isActiveScreen={item => item === getItemFromContext()}
              activeWallet={activeWallet}
              onWalletNavItemClick={linkTo(context.kind, item => {
                if (item === 'utxo') return 'Wallet UTXO distribution';
                return startCase(item);
              })}
              activeItem={getItemFromContext()}
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
