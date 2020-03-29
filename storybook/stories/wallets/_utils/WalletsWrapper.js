// @flow
import React from 'react';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import startCase from 'lodash/startCase';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

export default (story, context) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () =>
    context.story
      .replace('UTXO Distribution', 'settings')
      .replace('Transactions - Grouped by days', 'transactions')
      .toLocaleLowerCase();

  const getStoryName = (item: string) => {
    if (item === 'transactions') return 'Transactions - Grouped by days';
    if (item === 'utxo') return 'UTXO distribution';
    return startCase(item);
  };

  const getStoryKind = (item: string) => {
    const name =
      item === 'transactions' || item === 'utxo'
        ? 'Transactions'
        : startCase(item);
    return `Wallets|${name}`;
  };

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/wallets" {...context}>
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigation
              isActiveScreen={item => item === getItemFromContext()}
              onWalletNavItemClick={linkTo(getStoryKind, getStoryName)}
              activeItem={getItemFromContext()}
              isNotResponding={boolean('isNotResponding')}
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
