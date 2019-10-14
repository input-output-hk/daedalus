// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import InternetConnectionOfflineStatus from '../../../../source/renderer/app/components/status/InternetConnectionOfflineStatus';

storiesOf('Nodes|Status', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('Internet Connection Status', () => (
    <InternetConnectionOfflineStatus
      isCheckingInternetConnectionStatus={boolean(
        'Is checking Internet connection status',
        false
      )}
      checkInternetConnectionStatus={() => {}}
    />
  ));
