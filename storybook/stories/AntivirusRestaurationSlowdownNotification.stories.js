// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import AntivirusRestaurationSlowdownNotification
  from '../../source/renderer/app/components/notifications/AntivirusRestaurationSlowdownNotification';

storiesOf('AntivirusRestaurationSlodownNotification', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('default', () => (
    <div style={{ width: '100%', position: 'absolute', bottom: '0px' }}>
      <AntivirusRestaurationSlowdownNotification />
    </div>
  ));
