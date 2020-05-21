// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import HardwareWalletWithNavigationLayout from './HardwareWalletWithNavigationLayout';

export default (story: any, context: any) => {
  const storyWithKnobs = withKnobs(story, context);
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/hardware-wallets" {...context}>
          {context.story !== 'Empty' ? (
            <HardwareWalletWithNavigationLayout context={context}>
              {storyWithKnobs}
            </HardwareWalletWithNavigationLayout>
          ) : (
            storyWithKnobs
          )}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
