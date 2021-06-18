// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, date, number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryLayout from '../_support/StoryLayout';
import StoryProvider from '../_support/StoryProvider';
import StoryDecorator from '../_support/StoryDecorator';

import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';

import StakingWithNavigation from '../../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import StakingCountdown from '../../../source/renderer/app/components/staking/countdown/StakingCountdown';
import StakingInfo from '../../../source/renderer/app/components/staking/info/StakingInfo';
import StakingInfoCountdown from '../../../source/renderer/app/components/staking/info/StakingInfoCountdown';
import DelegationCenterNoWallets from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenterNoWallets';

import { StakePoolsStory } from './StakePools.stories';
import { StakingRewardsStory } from './Rewards.stories';
import { StakingDelegationCenterStory } from './DelegationCenter.stories';
import { StakingEpochsStory } from './Epochs.stories';
import { StakingDelegationSteps } from './DelegationSteps.stories';
import {
  Step1ConfigurationDialogStory,
  Step2ConfirmationDialogStory,
  Step3SuccessDialogStory,
  Step3FailureDialogStory,
  NoWalletsDialogDialogStory,
  RedemptionUnavailableDialogDialogStory,
} from './RedeemItnWallets.stories';
import {
  StakingUndelegateConfirmationStory,
  StakingUndelegateConfirmationResultStory,
} from './Undelegate.stories';
import { StakePoolsTableStory } from './StakePoolsTable.stories';

const defaultPercentage = 10;
const defaultStartDateTime = new Date();
defaultStartDateTime.setDate(defaultStartDateTime.getDate() + 2);
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);

  return new Date(stringTimestamp).toISOString();
};

const pageNames = {
  countdown: 'Decentralization Countdown',
  'delegation-center': 'Delegation Center',
  'stake-pools': 'Pools Index',
  'stake-pools-table': 'Stake Pools List',
  'stake-pools-tooltip': 'Tooltip',
  rewards: 'Rewards',
  epochs: 'Epochs',
  info: 'Info',
  'info-countdown': 'Info Countdown',
};

const decorator = (story, context) => {
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
        <StoryLayout activeSidebarCategory={activeSidebarCategory} {...context}>
          {context.parameters.id === 'countdown' ||
          context.parameters.id === 'wizard' ? (
            storyWithKnobs
          ) : (
            <StakingWithNavigation
              key="stakingWithNavigation"
              isActiveNavItem={(item) => item === getItemFromContext()}
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

storiesOf('Decentralization | Countdown', module)
  .addDecorator(decorator)
  .add(
    pageNames.countdown,
    () => (
      <div>
        <StakingCountdown
          startDateTime={startDateTimeKnob(
            'Decentralization Start DateTime',
            defaultStartDateTime
          )}
          onLearnMoreClick={action('onLearnMoreClick')}
        />
      </div>
    ),
    { id: 'countdown' }
  );

storiesOf('Decentralization | Staking', module)
  .addDecorator(decorator)
  // ====== Stories ======

  .add(
    pageNames['delegation-center'],
    (props) => (
      <StakingDelegationCenterStory {...props} isEpochsInfoAvailable />
    ),
    {
      id: 'delegation-center',
    }
  )

  .add(
    'Delegation Center - Loading',
    (props) => (
      <StakingDelegationCenterStory
        {...props}
        isLoading
        isEpochsInfoAvailable
      />
    ),
    {
      id: 'delegation-center-loading',
    }
  )

  .add(
    'Delegation Center - Not an Shelley era',
    (props) => (
      <StakingDelegationCenterStory {...props} isEpochsInfoAvailable={false} />
    ),
    {
      id: 'delegation-center-loading',
    }
  )

  .add('Delegation Center - No Wallets', () => (
    <DelegationCenterNoWallets
      onGoToCreateWalletClick={action('onGoToCreateWalletClick')}
      minDelegationFunds={number('minDelegationFunds', 10)}
    />
  ))

  .add(pageNames['stake-pools'], StakePoolsStory, { id: 'stake-pools' })

  .add(
    `${pageNames['stake-pools']} - Loading`,
    (props) => <StakePoolsStory {...props} isLoading />,
    {
      id: 'stake-pools-loading',
    }
  )

  .add(pageNames['stake-pools-table'], StakePoolsTableStory, {
    id: 'stake-pools-table',
  })

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
        onLearnMoreClick={action('onLearnMoreClick')}
      />
    ),
    {
      id: 'info',
    }
  )
  .add(
    pageNames['info-countdown'],
    () => {
      const percentage = number('percentage', 98, {
        range: true,
        min: 0,
        max: 100,
        step: 1,
      });
      const epochNumber = number('epochNumber', 257);
      const isFullyDecentralized = percentage === 100;
      const epochDate = isFullyDecentralized
        ? new Date().getTime() - 100000000
        : new Date().getTime() + 100000000;
      const epochStart = new Date(epochDate).toISOString();
      return (
        <StakingInfoCountdown
          percentage={percentage}
          onLearnMoreClick={action('onLearnMoreClick')}
          epoch={{
            epochNumber,
            epochStart,
          }}
          onSetStakingInfoWasOpen={action('onSetStakingInfoWasOpen')}
          isAnimating={boolean('isAnimating', false)}
          isFullyDecentralized={boolean('isFullyDecentralized', false)}
          stakingInfoWasOpen={boolean('stakingInfoWasOpen', false)}
          onStartStakingInfoAnimation={action('onStartStakingInfoAnimation')}
          onStopStakingInfoAnimation={action('onStopStakingInfoAnimation')}
        />
      );
    },
    {
      id: 'info-countdown',
    }
  )

  .add('Delegation Wizard', (props) => <StakingDelegationSteps {...props} />, {
    id: 'wizard',
  })
  .add(
    'Delegation Wizard - Delegation Not Available',
    (props) => <StakingDelegationSteps {...props} isDisabled />,
    {
      id: 'wizard',
    }
  )
  .add(
    'Undelegate Confirmation',
    (props) => (
      <StakingUndelegateConfirmationStory
        {...props}
        isHardwareWallet={boolean('isHardwareWallet', false)}
      />
    ),
    {
      id: 'undelegate-confirmation',
    }
  )

  .add(
    'Undelegate Confirmation - unknownn stake pool',
    (props) => (
      <StakingUndelegateConfirmationStory {...props} unknownStakePool />
    ),
    {
      id: 'undelegate-confirmation-unknown-pool',
    }
  )

  .add(
    'Undelegate Confirmation Result',
    StakingUndelegateConfirmationResultStory,
    {
      id: 'undelegate-confirmation-result',
    }
  );

storiesOf('Decentralization | Redeem ITN Rewards', module)
  .addDecorator(decorator)
  // ====== Stories ======

  .add('Step 1', Step1ConfigurationDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Step 2', Step2ConfirmationDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Step 3 - Success', Step3SuccessDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Step 3 - Failure', Step3FailureDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('No Wallets', NoWalletsDialogDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Redemption Unavailable', RedemptionUnavailableDialogDialogStory, {
    id: 'redeem-itn-wallets-story',
  });
