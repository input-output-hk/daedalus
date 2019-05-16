// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import Delegation from '../../source/renderer/app/components/staking/Delegation';

const timeLeft = (78 * 60 + 10) * 60 * 1000;

storiesOf('Delegation', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  // ====== Stories ======

  .add('Start', () => (
    <div>
      <Delegation currentLocale="en-US" timeLeft={timeLeft} />
    </div>
  ));
