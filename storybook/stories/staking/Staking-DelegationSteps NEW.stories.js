// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import DelegationStepsIntroDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsChooseStakePoolDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseStakePoolDialog';
import DelegationStepsNotAvailableDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';
import DelegationStepsConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog';
import DelegationStepsActivationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsActivationDialog';

import translations from '../../../source/renderer/app/i18n/translations';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';

import StakingWrapper from './StakingWrapper';

const WALLETS = [
  {
    id: '1',
    value: '1.0001 ADA',
    label: 'First Wallet',
    isAcceptableSetupWallet: true,
    hasPassword: true,
  },
  {
    id: '2',
    value: '2 ADA',
    label: 'Second Wallet',
    isAcceptableSetupWallet: true,
    hasPassword: true,
  },
  {
    id: '3',
    value: '0.0001 ADA',
    label: 'Third Wallet',
    isAcceptableSetupWallet: false,
    hasPassword: true,
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
