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
            content: 'Network failures have been reported.',
            date: Date.now(),
            target: {
              daedalus: 'v0.13',
              platform: 'macOS',
              platformVersion: '10.14.6',
            },
            title: 'Failure Alert',
          },
        ],
      })}
    </StoryDecorator>
  ))
  .add('Alerts Overlay', props => <AlertsOverlay {...props} />);
