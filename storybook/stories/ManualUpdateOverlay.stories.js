// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import ManualUpdateOverlay from '../../source/renderer/app/components/loading/ManualUpdateOverlay';

storiesOf('ManualUpdateOverlay', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Default', () => (
    <ManualUpdateOverlay
      currentAppVersion="0.12.0"
      availableAppVersion="0.13.1"
      onExternalLinkClick={action('openExternalLink')}
    />
  ));
