// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import AutomaticUpdateNotification from '../../source/renderer/app/components/notifications/AutomaticUpdateNotification';

storiesOf('Notifications', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('AutomaticUpdateNotification - with newer version number', () => (
    <AutomaticUpdateNotification
      currentAppVersion="0.12.0"
      nextUpdateVersion="0.14.0"
      onAccept={action('onAccept')}
      onPostpone={action('onPostpone')}
    />
  ))

  .add('AutomaticUpdateNotification - without newer version number', () => (
    <AutomaticUpdateNotification
      currentAppVersion="0.12.0"
      nextUpdateVersion={null}
      onAccept={action('onAccept')}
      onPostpone={action('onPostpone')}
    />
  ));
