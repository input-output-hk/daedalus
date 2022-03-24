import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
// import startCase from 'lodash/startCase';
import StoryLayout from '../_support/StoryLayout';
import StoryProvider from '../_support/StoryProvider';
import StoryDecorator from '../_support/StoryDecorator';
import StakingWithNavigation from '../../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';

const pageNames = {
  countdown: 'Staking Countdown',
  'delegation-center': 'Delegation Center',
  'stake-pools': 'Stake Pools',
  'stake-pools-tooltip': 'Tooltip',
  rewards: 'Rewards',
  epochs: 'Epochs',
  info: 'Info',
};
export default (story, context) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () => context.parameters.id;

  let activeSidebarCategory = null;

  if (
    context.parameters.id === 'countdown' ||
    context.parameters.id === 'stake-pools-tooltip'
  ) {
    activeSidebarCategory =
      CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN.route;
  } else {
    activeSidebarCategory = CATEGORIES_BY_NAME.STAKING.route;
  }

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory={activeSidebarCategory} {...context}>
          {context.parameters.id === 'countdown' ? (
            storyWithKnobs
          ) : (
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            <StakingWithNavigation
              isActiveNavItem={(item) => item === getItemFromContext()}
              activeItem={getItemFromContext()}
              onNavItemClick={linkTo(context.kind, (item) => {
                return pageNames[item];
              })}
            >
              {storyWithKnobs}
            </StakingWithNavigation>
          )}
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
