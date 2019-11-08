// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import { SplashNetworkStory } from './Splash-Network.stories';

storiesOf('Splash', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Network', SplashNetworkStory);
