// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, date, number } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';

import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';

import StakingWithNavigation from '../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import StakingCountdown from '../../source/renderer/app/components/staking/countdown/StakingCountdown';
import DelegationCenter from '../../source/renderer/app/components/staking/delegation-center/DelegationCenter';
import StakingEpochs from '../../source/renderer/app/components/staking/epochs/StakingEpochs';
import StakingInfo from '../../source/renderer/app/components/staking/info/StakingInfo';
import DelegationStepsIntroDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsNotAvailableDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';

import { StakePoolsListStory } from './StakePoolsListStory.js';
import { StakingRewardsStory } from './Staking-Rewards.stories';

import translations from '../../source/renderer/app/i18n/translations';

const defaultPercentage = 10;
const defaultStartDateTime = new Date('2019-09-26');
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);

  return new Date(stringTimestamp).toISOString();
};

const pageNames = {
  countdown: 'Staking Countdown',
  'delegation-center': 'Delegation Center',
  'stake-pools': 'Stake Pools',
  rewards: 'Rewards',
  epochs: 'Epochs',
  info: 'Info',
};

const WALLETS = [
  {
    value: '1.0001 ADA',
    label: 'First Wallet',
    isAcceptableSetupWallet: true,
  },
  {
    value: '2 ADA',
    label: 'Second Wallet',
    isAcceptableSetupWallet: true,
  },
  {
    value: '0.0001 ADA',
    label: 'Third Wallet',
    isAcceptableSetupWallet: false,
  },
];

const locales = {
  English: 'en-US',
  Japanese: 'ja-JP',
};
// Delegation steps labels are translated outside components and we need to determine correct translations
const locale = localStorage.getItem('currentLocale') || 'English';
const translationIndex = locales[locale];
const DELEGATION_WIZARD_STEPS_LIST = [
  translations[translationIndex]["delegation.setup.steps.step.1.label"],
  translations[translationIndex]["delegation.setup.steps.step.2.label"],
  translations[translationIndex]["delegation.setup.steps.step.3.label"],
  translations[translationIndex]["delegation.setup.steps.step.4.label"],
];

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
    pageNames.countdown,
    () => (
      <div>
        <StakingCountdown
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
    () => <DelegationCenter name={pageNames['delegation-center']} />,
    { id: 'delegation-center' }
  )

  .add(pageNames['stake-pools'], StakePoolsListStory, { id: 'stake-pools' })

  .add(pageNames.rewards, StakingRewardsStory, { id: 'rewards' })

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
  )

  .add('DelegationStepsIntroDialog', () => (
    <DelegationStepsIntroDialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      onLearnMoreClick={action('onLearnMoreClick')}
    />
  ))

  .add('DelegationStepsChooseWalletDialog', () => (
    <DelegationStepsChooseWalletDialog
      stepsList={DELEGATION_WIZARD_STEPS_LIST}
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      onBack={action('onBack')}
      wallets={WALLETS}
    />
  ))

  .add('DelegationStepsNotAvailableDialog', () => (
    <DelegationStepsNotAvailableDialog onClose={action('onClose')} />
  ));
