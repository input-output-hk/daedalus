// @flow
import React from 'react';
import { configure, addDecorator } from '@storybook/react';
import StoryWrapper from './stories/support/StoryWrapper';

addDecorator(story => {
  return <StoryWrapper>{story}</StoryWrapper>;
});

function loadStories() {
  require('./stories');
}

configure(loadStories, module);
