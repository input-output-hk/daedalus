// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../support/StoryDecorator';
import IncidentOverlay from '../../../source/renderer/app/components/news/IncidentOverlay';

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>
      {story({
        incident: {
          action: {
            label: 'Dismiss',
          },
          content: 'Network failure',
          date: Date.now(),
          target: {
            daedalus: 'v0.13',
            platform: 'macOS',
            platformVersion: '10.14.6',
          },
          title: 'Lazarus Incident',
        },
      })}
    </StoryDecorator>
  ))
  .add('Incident Overlay', props => <IncidentOverlay {...props} />);
