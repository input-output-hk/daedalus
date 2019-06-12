// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, date, number } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';

import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';

import StakingWithNavigation from '../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import StakingDelegationCountdown from '../../source/renderer/app/components/staking/delegation-countdown/StakingDelegationCountdown';
import StakingDelegationCenter from '../../source/renderer/app/components/staking/delegation-center/StakingDelegationCenter';
import StakingInfo from '../../source/renderer/app/components/staking/info/StakingInfo';

import { StakingStakePoolsStory } from './Staking-StakePools.js';
import { StakingRewardsStory } from './Staking-Rewards.stories';
import { StakingEpochsStory } from './Staking-Epochs.stories';

const defaultPercentage = 10;
const defaultStartDateTime = new Date('2019-09-26');
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);

  return new Date(stringTimestamp).toISOString();
};

const pageNames = {
  'delegation-center': 'Delegation Center',
  'stake-pools': 'Stake Pools',
  rewards: 'Rewards',
  epochs: 'Epochs',
  info: 'Info',
};

storiesOf('Staking', module)
  .addDecorator((story, context) => {
    const storyWithKnobs = withKnobs(story, context);
    const getItemFromContext = () => context.parameters.id;
    let activeSidebarCategory = null;

    if (context.parameters.id === 'countdown') {
      activeSidebarCategory =
        CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN.route;
    } else {
      activeSidebarCategory = CATEGORIES_BY_NAME.STAKING.route;
    }

    return (
      <StoryDecorator>
        <StoryProvider>
          <StoryLayout
            activeSidebarCategory={activeSidebarCategory}
            storyName={context.story}
          >
            {context.parameters.id === 'countdown' ? (
              storyWithKnobs
            ) : (
              <StakingWithNavigation
                activeItem={getItemFromContext()}
                onNavItemClick={linkTo('Staking', item => pageNames[item])}
              >
                {storyWithKnobs}
              </StakingWithNavigation>
            )}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })
  // ====== Stories ======

  .add(
    'Decentralization Start Info',
    () => (
      <div>
        <StakingDelegationCountdown
          currentLocale="en-US"
          startDateTime={startDateTimeKnob(
            'Decentralization Start DateTime',
            defaultStartDateTime
          )}
        />
      </div>
    ),
    { id: 'countdown' }
  )

  .add(
    pageNames['delegation-center'],
    () => <StakingDelegationCenter name={pageNames['delegation-center']} />,
    { id: 'delegation-center' }
  )

  .add(pageNames['stake-pools'], StakingStakePoolsStory, { id: 'stake-pools' })

  .add(pageNames.rewards, StakingRewardsStory, { id: 'rewards' })

  .add(pageNames.epochs, StakingEpochsStory, { id: 'epochs' })

  .add(
    pageNames.info,
    () => (
      <StakingInfo
        percentage={number('Percentage', defaultPercentage, {
          min: 0,
          max: 100,
          step: 1,
          range: true,
        })}
      />
    ),
    {
      id: 'info',
    }
  );
