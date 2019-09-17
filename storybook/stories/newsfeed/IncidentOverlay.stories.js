// @flow

import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../support/StoryDecorator';
import IncidentOverlay from '../../../source/renderer/app/components/newsfeed/IncidentOverlay';

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>
      {story({
        content: {
          h1: 'Network failure',
          h2:
            'The network has failed, please do not try accessing it at this time',
          paragraph:
            'You will not recieve a response until it is running again',
        },
        title: 'Lazarus Incident',
      })}
    </StoryDecorator>
  ))
  .add('Incident Overlay', props => <IncidentOverlay {...props} />);
