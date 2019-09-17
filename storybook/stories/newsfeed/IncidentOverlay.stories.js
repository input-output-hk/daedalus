// @flow

import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../support/StoryDecorator';
import IncidentOverlay from '../../../source/renderer/app/components/news/IncidentOverlay';

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>
      {story({
        action: {
          label: {
            'en-US': 'Dismiss',
            'ja-JP': 'Dismiss',
          },
        },
        content: {
          'en-US': 'Network failure',
          'ja-JP': 'Network failure',
        },
        date: Date.now(),
        target: {
          daedalus: 'v0.13',
          platform: 'macOS',
          platformVersion: '10.14.6',
        },
        title: {
          'en-US': 'Lazarus Incident',
          'ja-JP': 'Lazarus Incident',
        },
      })}
    </StoryDecorator>
  ))
  .add('Incident Overlay', props => <IncidentOverlay {...props} />);
