// @flow
import React from 'react';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import stakingDecorator from './_utils/stakingDecorator';

import DelegationCenterNoWallets from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenterNoWallets';
import { DelegationCenterStory } from './Delegation-DelegationCenter.stories';
import { DelegationStepsStory } from './Delegation-DelegationSteps.stories';

const activeItem = 'delegation-center';

storiesOf('Decentralization | Delegation', module)
  .addDecorator(stakingDecorator(activeItem))
  // ====== Stories ======

  .add('Delegation Center', (props) => (
    <DelegationCenterStory {...props} isEpochsInfoAvailable />
  ))
  .add('Delegation Center - Loading', (props) => (
    <DelegationCenterStory {...props} isLoading isEpochsInfoAvailable />
  ))
  .add('Delegation Center - Not an Shelley era', (props) => (
    <DelegationCenterStory {...props} isEpochsInfoAvailable={false} />
  ))
  .add('Delegation Center - No Wallets', () => (
    <DelegationCenterNoWallets
      onGoToCreateWalletClick={action('onGoToCreateWalletClick')}
      minDelegationFunds={number('minDelegationFunds', 10)}
    />
  ))
  .add('Delegation steps', (props) => <DelegationStepsStory {...props} />);
