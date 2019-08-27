// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import InternetConnectionOfflineStatus from '../../source/renderer/app/components/status/InternetConnectionOfflineStatus';

storiesOf('Internet Connection Status', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add(
    'Offline',
    () => <InternetConnectionOfflineStatus checkAgain={() => null} />,
    { id: 'offline' }
  );
