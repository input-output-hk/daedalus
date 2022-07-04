import React from 'react';
import timemachine from 'timemachine';
import StoryWrapper from './stories/_support/StoryWrapper';
import '!style-loader!css-loader!sass-loader!../source/renderer/app/themes/index.global.scss'; // eslint-disable-line

import './stories/_support/environment';

export const decorators = [(story) => <StoryWrapper>{story}</StoryWrapper>];
timemachine.config({
  dateString: 'Sat, 01 Jan 2022 10:00:00 GMT',
});
