import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import { currentThemeOf } from '../../_support/globals';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';

export default function (story: any, context: any) {
  const storyWithKnobs = withKnobs(story, context);
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout
          activeSidebarCategory="/hardware-wallets"
          {...context}
          currentTheme={currentThemeOf(context)}
        >
          {storyWithKnobs}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
}
