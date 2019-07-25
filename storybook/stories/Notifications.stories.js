// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import AutomaticUpdate from '../../source/renderer/app/components/notifications/AutomaticUpdate';

storiesOf('Notifications', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('AutomaticUpdate', () => (
    <AutomaticUpdate
      currentAppVersion="0.12.0"
      availableAppVersion="0.14.0"
      onAccept={action('onAccept')}
      onPostpone={action('onPostpone')}
    />
  ));
