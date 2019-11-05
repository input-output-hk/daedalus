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
import StakingInfo from '../../source/renderer/app/components/staking/info/StakingInfo';
import DelegationStepsIntroDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsChooseStakePoolDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseStakePoolDialog';
import DelegationStepsNotAvailableDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';
import DelegationStepsConfirmationDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog';
import DelegationStepsActivationDialog from '../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsActivationDialog';
import DelegationCenterNoWallets from '../../source/renderer/app/components/staking/delegation-center/DelegationCenterNoWallets';

import { StakePoolsStory } from './Staking-StakePools.stories';
import { StakingRewardsStory } from './Staking-Rewards.stories';
import { StakingDelegationCenterStory } from './Staking-DelegationCenter.stories';
import { StakingEpochsStory } from './Staking-Epochs.stories';

import translations from '../../source/renderer/app/i18n/translations';
import STAKE_POOLS from '../../source/renderer/app/config/stakingStakePools.dummy.json';

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
  'stake-pools-tooltip': 'Tooltip',
  rewards: 'Rewards',
  epochs: 'Epochs',
  info: 'Info',
};

const WALLETS = [
  {
    id: '1',
    amount: 1,
    name: 'First Wallet',
  },
  {
    id: '2',
    amount: 2,
    name: 'Second Wallet',
  },
  {
    id: '3',
    amount: 0.0001,
    name: 'Third Wallet',
  },
];

const locales = {
  English: 'en-US',
  Japanese: 'ja-JP',
};

// Delegation steps labels are translated outside components and we need to determine correct translations
const locale = sessionStorage.getItem('localeName') || 'English';
const currentTheme = sessionStorage.getItem('themeName') || 'light-blue';
const translationIndex = locales[locale];

// @TODO - improve locales GET once [DDW-711](https://github.com/input-output-hk/daedalus/pull/1426) is merged
const DELEGATION_WIZARD_STEPS_LIST = [
  translations[translationIndex]['staking.delegationSetup.steps.step.1.label'],
  translations[translationIndex]['staking.delegationSetup.steps.step.2.label'],
  translations[translationIndex]['staking.delegationSetup.steps.step.3.label'],
  translations[translationIndex]['staking.delegationSetup.steps.step.4.label'],
];

storiesOf('Staking', module)
  .addDecorator((story, context) => {
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
          <StoryLayout
            activeSidebarCategory={activeSidebarCategory}
            storyName={context.story}
          >
            {context.parameters.id === 'countdown' ? (
              storyWithKnobs
            ) : (
              <StakingWithNavigation
                isActiveNavItem={item => item === getItemFromContext()}
                activeItem={getItemFromContext()}
                onNavItemClick={linkTo('Staking', item => pageNames[item])}
                isIncentivizedTestnet={false}
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
          startDateTime={startDateTimeKnob(
            'Decentralization Start DateTime',
            defaultStartDateTime
          )}
          onLearnMoreClick={action('onLearnMoreClick')}
        />
      </div>
    ),
    { id: 'countdown' }
  )

  .add(pageNames['delegation-center'], StakingDelegationCenterStory, {
    id: 'delegation-center',
  })

  .add('Delegation Center - No Wallets', () => (
    <DelegationCenterNoWallets
      onGoToCreateWalletClick={action('onGoToCreateWalletClick')}
    />
  ))

  .add(pageNames['stake-pools'], StakePoolsStory, { id: 'stake-pools' })

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
      onSelectWallet={action('onSelectWallet')}
      onBack={action('onBack')}
      wallets={WALLETS}
      minDelegationFunds={1}
      selectedWalletId={WALLETS[0].id}
      isWalletAcceptable={amount => amount >= 1}
    />
  ))

  .add('DelegationStepsConfirmationDialog', () => (
    <DelegationStepsConfirmationDialog
      stepsList={DELEGATION_WIZARD_STEPS_LIST}
      onClose={action('onClose')}
      onConfirm={action('onConfirm')}
      onBack={action('onBack')}
    />
  ))

  .add('DelegationStepsActivationDialog', () => (
    <DelegationStepsActivationDialog
      stepsList={DELEGATION_WIZARD_STEPS_LIST}
      onClose={action('onClose')}
      onActivate={action('onActivate')}
      onBack={action('onBack')}
    />
  ))

  .add('DelegationStepsChooseStakePoolDialog', () => (
    <DelegationStepsChooseStakePoolDialog
      stepsList={DELEGATION_WIZARD_STEPS_LIST}
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
        STAKE_POOLS[0],
        STAKE_POOLS[13],
        STAKE_POOLS[36],
      ]}
      onOpenExternalLink={() => {}}
      currentTheme={currentTheme}
      onClose={action('onClose')}
      onBack={action('onBack')}
      onSelectPool={action('onSelectPool')}
      selectedPool={null}
    />
  ))

  .add('DelegationStepsNotAvailableDialog', () => (
    <DelegationStepsNotAvailableDialog
      minDelegationFunds={1}
      onClose={action('onClose')}
    />
  ));
