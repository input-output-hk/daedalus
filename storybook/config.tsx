import React from 'react';
import { configure, addDecorator } from '@storybook/react';
import StoryWrapper from './stories/_support/StoryWrapper';
import '!style-loader!css-loader!sass-loader!../source/renderer/app/themes/index.global.scss'; // eslint-disable-line

addDecorator((story) => {
  return <StoryWrapper>{story}</StoryWrapper>;
});

function loadStories() {
  require('./stories');
}

configure(loadStories, module);
