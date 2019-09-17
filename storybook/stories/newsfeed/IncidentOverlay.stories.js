// @flow

import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../support/StoryDecorator';
import IncidentOverlay from '../../../source/renderer/app/components/news/IncidentOverlay';

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>
      {story({
        content: {
          h1: 'Lazarus Incident',
          paragraph: 'The network has failed, please go home',
        },
      })}
    </StoryDecorator>
  ))
  .add('Incident Overlay', props => <IncidentOverlay {...props} />);
