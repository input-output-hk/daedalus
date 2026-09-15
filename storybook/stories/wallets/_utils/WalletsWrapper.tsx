import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import WalletWithNavigationLayout from './WalletWithNavigationLayout';
import { currentThemeOf } from '../../_support/globals';

export default function (story: any, context: any) {
  const storyWithKnobs = withKnobs(story, context);
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout
          activeSidebarCategory="/wallets"
          {...context}
          currentTheme={currentThemeOf(context)}
        >
          {context.story !== 'Empty' && context.story !== 'Wallet Add' ? (
            <WalletWithNavigationLayout context={context}>
              {storyWithKnobs}
            </WalletWithNavigationLayout>
          ) : (
            storyWithKnobs
          )}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
}
