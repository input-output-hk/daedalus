// @flow
import React from 'react';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import WalletWithNavigationLayout from './WalletWithNavigationLayout';

export default (story: any, context: any) => {
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/wallets" {...context}>
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigationLayout context={context}>
              {story()}
            </WalletWithNavigationLayout>
          ) : (
            story()
          )}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
