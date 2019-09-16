// @flow

import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../support/StoryDecorator';

storiesOf('NewsFeed', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Incident Overlay', () => <div>hello</div>);
