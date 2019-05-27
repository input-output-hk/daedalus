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
import StakingStakePools from '../../source/renderer/app/components/staking/stake-pools/StakingStakePools';
import StakingEpochs from '../../source/renderer/app/components/staking/epochs/StakingEpochs';
import StakingInfo from '../../source/renderer/app/components/staking/info/StakingInfo';

import STAKE_POOLS from '../../source/renderer/app/config/stakingStakePools.dummy.json';
import StakingRewards from './Staking-Rewards.stories';

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
        CATEGORIES_BY_NAME.STAKING_WITH_DELEGATION_COUNTDOWN.route;
    } else {
      activeSidebarCategory =
        CATEGORIES_BY_NAME.STAKING_WITHOUT_DELEGATION_COUNTDOWN.route;
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
          redirectToStakingInfo={linkTo('Staking', () => 'Info')}
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

  .add(
    pageNames['stake-pools'],
    () => (
      <StakingStakePools
        stakePoolsList={STAKE_POOLS.slice(
          0,
          number('Pools', 100, {
            range: true,
            min: 37,
            max: 300,
            step: 1,
          })
        )}
        stakePoolsDelegatingList={[
          STAKE_POOLS[1],
          STAKE_POOLS[3],
          STAKE_POOLS[20],
          STAKE_POOLS[36],
        ]}
        onOpenExternalLink={() => {}}
      />
    ),
    { id: 'stake-pools' }
  )

  .add(pageNames.rewards, StakingRewards, { id: 'rewards' })

  .add(pageNames.epochs, () => <StakingEpochs name={pageNames.epochs} />, {
    id: 'epochs',
  })

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
