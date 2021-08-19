// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';

import { ThumbPoolStory } from './Widgets-ThumbPool.stories';
import { PoolPopOverStory } from './Widgets-PoolPopOver.stories';

const defaultStartDateTime = new Date();
defaultStartDateTime.setDate(defaultStartDateTime.getDate() + 2);

storiesOf('Decentralization | Widgets', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  // ====== Stories ======

  .add('Thumb Pool', ThumbPoolStory)
  .add('Pool PopOver', PoolPopOverStory);
