// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number } from '@storybook/addon-knobs';
import StoryDecorator from './support/StoryDecorator';
import Delegation from '../../source/renderer/app/components/staking/Delegation';

const timeLeft = (78 * 60 + 10) * 60 * 1000;

storiesOf('Delegation', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======

  .add('Start', () => (
    <div>
      <Delegation
        currentLocale="en-US"
        timeLeft={number('Time Left (miliseconds)', timeLeft, {
          min: 0,
          step: 1000,
        })}
      />
    </div>
  ));
