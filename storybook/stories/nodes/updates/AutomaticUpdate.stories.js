// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import AutomaticUpdateNotification from '../../../../source/renderer/app/components/notifications/AutomaticUpdateNotification';

export const AutoUpdateWithVerison = () => (
  <AutomaticUpdateNotification
    currentAppVersion="0.12.0"
    nextUpdateVersion="0.14.0"
    onAccept={action('onAccept')}
    onPostpone={action('onPostpone')}
  />
);

export const AutoUpdateWithoutVersion = () => (
  <AutomaticUpdateNotification
    currentAppVersion="0.12.0"
    nextUpdateVersion={null}
    onAccept={action('onAccept')}
    onPostpone={action('onPostpone')}
  />
);
