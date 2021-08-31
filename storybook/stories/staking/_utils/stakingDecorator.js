// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import StakingWithNavigation from '../../../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import { CATEGORIES_BY_NAME } from '../../../../source/renderer/app/config/sidebarConfig';

export default (activeItem: string) => (story: any, context: any) => {
  const storyWithKnobs = withKnobs(story, context);
  const activeSidebarCategory = CATEGORIES_BY_NAME.STAKING.route;
  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory={activeSidebarCategory} {...context}>
          {
            <StakingWithNavigation
              key="stakingWithNavigation"
              isActiveNavItem={(item) => item === activeItem}
              activeItem={activeItem}
              onNavItemClick={() => {}}
              showInfoTab
            >
              {storyWithKnobs}
            </StakingWithNavigation>
          }
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
