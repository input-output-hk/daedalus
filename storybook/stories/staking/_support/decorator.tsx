import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import { CATEGORIES_BY_NAME } from '../../../../source/renderer/app/config/sidebarConfig';
import StakingWithNavigation from '../../../../source/renderer/app/components/staking/layouts/StakingWithNavigation';

export const stakingDecorator = (story, context) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () => context.parameters.id;

  const activeSidebarCategory = CATEGORIES_BY_NAME.STAKING.route;

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory={activeSidebarCategory} {...context}>
          {context.parameters.id === 'wizard' ? (
            storyWithKnobs
          ) : (
            <StakingWithNavigation
              key="stakingWithNavigation"
              isActiveNavItem={(item) => item === getItemFromContext()}
              showInfoTab
              activeItem={getItemFromContext()}
              onNavItemClick={() => {}}
            >
              {storyWithKnobs}
            </StakingWithNavigation>
          )}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
