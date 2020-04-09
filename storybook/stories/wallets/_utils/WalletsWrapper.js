// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import startCase from 'lodash/startCase';
import { observable, runInAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';
import Wallet from '../../../../source/renderer/app/domains/Wallet';
import { WalletRecoveryPhraseVerificationStatuses } from '../../../../source/renderer/app/stores/WalletsStore';

const sidebarMenus = observable({
  wallets: {
    items: [
      {
        id: '1',
        title: 'First',
        info: '100 ADA',
        isNotResponding: false,
        hasPassword: true,
        isConnected: true,
        isLegacy: false,
        recoveryPhraseVerificationStatus:
          WalletRecoveryPhraseVerificationStatuses.OK,
      },
      {
        id: '2',
        title: 'Second',
        info: '200 ADA',
        isNotResponding: false,
        hasPassword: true,
        isConnected: true,
        isLegacy: false,
        recoveryPhraseVerificationStatus:
          WalletRecoveryPhraseVerificationStatuses.OK,
      },
      {
        id: '3',
        title: 'Third',
        info: '300 ADA',
        isNotResponding: false,
        hasPassword: true,
        isConnected: true,
        isLegacy: false,
        recoveryPhraseVerificationStatus:
          WalletRecoveryPhraseVerificationStatuses.OK,
      },
    ],
    activeWalletId: '1',
    actions: {
      onAddWallet: action('toggleAddWallet'),
      onWalletItemClick: (walletId: string) => {
        runInAction(() => {
          sidebarMenus.wallets.activeWalletId = walletId;
        });
      },
    },
  },
});

export default (story, context) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () =>
    context.story
      .replace('Wallet UTXO distribution', 'utxo')
      .replace('Wallet Summary', 'summary')
      .toLocaleLowerCase();

  const activeWallet: Wallet =
    sidebarMenus.wallets.items[
      parseInt(sidebarMenus.wallets.activeWalletId, 10)
    ];

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/wallets" {...context}>
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigation
              isDialogOpen={() => {}}
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
