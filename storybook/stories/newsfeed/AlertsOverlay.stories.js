// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../support/StoryDecorator';
import AlertsOverlay from '../../../source/renderer/app/components/news/AlertsOverlay';

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>
      {story({
        alerts: [
          {
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
        ],
      })}
    </StoryDecorator>
  ))
  .add('Alerts Overlay', props => <AlertsOverlay {...props} />);
