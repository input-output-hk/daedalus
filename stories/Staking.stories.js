import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import Staking from '../app/components/staking/Staking';
import StakingSwitch from '../app/components/staking/StakingSwitch';

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
