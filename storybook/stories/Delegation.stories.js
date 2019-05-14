// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import Staking from '../../source/renderer/app/components/staking/Staking';

storiesOf('Delegation', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  // ====== Stories ======

  .add('Start', () => (
    <div>
      <Staking />
    </div>
  ));
