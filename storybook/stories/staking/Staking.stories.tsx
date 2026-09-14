import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryLayout from '../_support/StoryLayout';
import StoryProvider from '../_support/StoryProvider';
import StoryDecorator from '../_support/StoryDecorator';
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';
import StakingWithNavigation from '../../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import DelegationCenterNoWallets from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenterNoWallets';
import { StakePoolsStory } from './_support/StakePools';
import { StakingRewardsStory } from './_support/Rewards';
import { StakingDelegationCenterStory } from './_support/DelegationCenter';
import { StakingEpochsStory } from './_support/Epochs';
import { StakingDelegationSteps } from './_support/DelegationSteps';
import {
  Step1ConfigurationDialogStory,
  Step2ConfirmationDialogStory,
  Step3SuccessDialogStory,
  Step3FailureDialogStory,
  NoWalletsDialogDialogStory,
  RedemptionUnavailableDialogDialogStory,
} from './_support/RedeemItnWallets';
import {
  StakingUndelegateConfirmationStory,
  StakingUndelegateConfirmationResultStory,
} from './_support/Undelegate';
import { StakePoolsTableStory } from './_support/StakePoolsTable';

const pageNames = {
  'delegation-center': 'Delegation Center',
  'stake-pools': 'Pools Index',
  'stake-pools-table': 'Stake Pools List',
  'stake-pools-tooltip': 'Tooltip',
  rewards: 'Rewards',
  epochs: 'Epochs',
};

const decorator = (story, context) => {
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

storiesOf('Decentralization / Staking', module)
  .addDecorator(decorator) // ====== Stories ======
  .add(
    pageNames['delegation-center'],
    (_, props) => (
      // @ts-ignore ts-migrate(2739) FIXME: Type '{ isEpochsInfoAvailable: true; id: string; n... Remove this comment to see the full error message
      <StakingDelegationCenterStory {...props} isEpochsInfoAvailable />
    ),
    {
      id: 'delegation-center',
    }
  )
  .add(
    'Delegation Center - Loading',
    (_, props) => (
      // @ts-ignore ts-migrate(2739) FIXME: Type '{ isLoading: true; isEpochsInfoAvailable: tr... Remove this comment to see the full error message
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
    (_, props) => (
      // @ts-ignore ts-migrate(2739) FIXME: Type '{ isEpochsInfoAvailable: false; id: string; ... Remove this comment to see the full error message
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
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: Props) => JSX.Element' i... Remove this comment to see the full error message
  .add(pageNames['stake-pools'], StakePoolsStory, {
    id: 'stake-pools',
  })
  .add(
    `${pageNames['stake-pools']} - Loading`,
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ isLoading: true; id: string; name: string;... Remove this comment to see the full error message
    (props) => <StakePoolsStory {...props} isLoading />,
    {
      id: 'stake-pools-loading',
    }
  )
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: Props) => JSX.Element' i... Remove this comment to see the full error message
  .add(pageNames['stake-pools-table'], StakePoolsTableStory, {
    id: 'stake-pools-table',
  })
  .add(pageNames.rewards, StakingRewardsStory, {
    id: 'rewards',
  })
  .add(pageNames.epochs, StakingEpochsStory, {
    id: 'epochs',
  })
  .add(
    'Delegation Wizard',
    (_, props) => {
      const oversaturationPercentage = number('Oversaturation Percentage', 0, {
        min: 0,
        max: 1000,
        step: 1,
        range: true,
      });
      return (
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        <StakingDelegationSteps
          {...props}
          oversaturationPercentage={oversaturationPercentage}
        />
      );
    },
    {
      id: 'wizard',
    }
  )
  .add(
    'Delegation Wizard - Delegation Not Available',
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    (_, props) => <StakingDelegationSteps {...props} isDisabled />,
    {
      id: 'wizard',
    }
  )
  .add(
    'Undelegate Confirmation',
    (_, props) => (
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
    (_, props) => (
      <StakingUndelegateConfirmationStory {...props} unknownStakePool />
    ),
    {
      id: 'undelegate-confirmation-unknown-pool',
    }
  )
  .add(
    'Undelegate Confirmation Result',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale, }: { locale: string; ... Remove this comment to see the full error message
    (_, props) => <StakingUndelegateConfirmationResultStory {...props} />,
    {
      id: 'undelegate-confirmation-result',
    }
  );
storiesOf('Decentralization / Redeem ITN Rewards', module)
  .addDecorator(decorator) // ====== Stories ======
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
