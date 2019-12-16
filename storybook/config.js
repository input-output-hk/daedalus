// @flow
import React from 'react';
import { configure, addDecorator, addParameters } from '@storybook/react';
import { themes } from '@storybook/theming';
import StoryWrapper from './stories/_support/StoryWrapper';
import '!style-loader!css-loader!sass-loader!../source/renderer/app/themes/index.global.scss'; // eslint-disable-line

addDecorator(story => {
  return <StoryWrapper>{story}</StoryWrapper>;
});

// Option defaults:
addParameters({
  options: {
    name: 'Daedalus',
    theme: themes.dark,
  },
});

function loadStories() {
  require('./stories');
}

configure(loadStories, module);
