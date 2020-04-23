// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import ManualUpdate from '../../../../source/renderer/app/components/loading/manual-update/ManualUpdate';

export const ManualUpdateStory = () => (
  <ManualUpdate
    currentAppVersion="0.12.0"
    availableAppVersion="0.14.0"
    onExternalLinkClick={action('openExternalLink')}
  />
);
