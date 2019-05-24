// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, date } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';

import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';

import StakingWithNavigation from '../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import StakingDelegationCountdown from '../../source/renderer/app/components/staking/delegation-countdown/StakingDelegationCountdown';
import StakingDelegationCenter from '../../source/renderer/app/components/staking/delegation-center/StakingDelegationCenter';
import StakingStakePools from '../../source/renderer/app/components/staking/stake-pools/StakingStakePools';
import StakingEpochs from '../../source/renderer/app/components/staking/epochs/StakingEpochs';
import StakingInfo from '../../source/renderer/app/components/staking/info/StakingInfo';

import StakingRewards from './Staking-Rewards.stories';

const defaultStartDateTime = new Date('Jun 01 2019');
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

    return (
      <StoryDecorator>
        <StoryProvider>
          <StoryLayout
            activeSidebarCategory={CATEGORIES_BY_NAME.STAKING.route}
            storyName={context.story}
          >
            {context.parameters.id !== 'countdown' ? (
              <StakingWithNavigation
                activeItem={getItemFromContext()}
                onNavItemClick={linkTo('Staking', item => pageNames[item])}
              >
                {storyWithKnobs}
              </StakingWithNavigation>
            ) : (
              storyWithKnobs
            )}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })
  // ====== Stories ======

  .add(
    'Start of decentralisation notification',
    () => (
      <div>
        <StakingDelegationCountdown
          currentLocale="en-US"
          startDateTime={startDateTimeKnob(
            'Delegation Start DateTime',
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

  .add(
    pageNames['stake-pools'],
    () => <StakingStakePools name={pageNames['stake-pools']} />,
    { id: 'stake-pools' }
  )

  .add(pageNames.rewards, StakingRewards, { id: 'rewards' })

  .add(pageNames.epochs, () => <StakingEpochs name={pageNames.epochs} />, {
    id: 'epochs',
  })

  .add(pageNames.info, () => <StakingInfo name={pageNames.info} />, {
    id: 'info',
  });
