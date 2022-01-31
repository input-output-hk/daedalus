import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';

export default (story: any, context: any) => {
  const storyWithKnobs = withKnobs(story, context);
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/hardware-wallets" {...context}>
          {storyWithKnobs}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
