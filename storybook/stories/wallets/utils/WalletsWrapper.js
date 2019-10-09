import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
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
      .replace('Wallet UTXO distribution', 'utxo')
      .replace('Wallet Summary', 'summary')
      .toLocaleLowerCase();

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/wallets" storyName={context.story}>
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigation
              isActiveScreen={item => item === getItemFromContext()}
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
