// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import Staking from '../../source/renderer/app/components/staking/Staking';
import StakingSwitch from '../../source/renderer/app/components/staking/StakingSwitch';

storiesOf('Staking', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Switches', () => (
    <div>
      <StakingSwitch active={false} />
      <StakingSwitch active />
    </div>
  ))

  .add('StakingPage', () => (
    <div>
      <Staking />
    </div>
  ));
