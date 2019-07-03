// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import { SyncingConnectingStory } from './Loading-SyncingConnecting.stories';
import { NoDiskSpaceErrorStory } from './Loading-NoDiskSpaceError.stories';
import { SystemTimeErrorStory } from './Loading-SystemTimeError.stories';
import { ManualUpdateStory } from './Loading-ManualUpdate.stories';

storiesOf('Loading', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('SyncingConnecting', SyncingConnectingStory)
  .add('NoDiskSpaceError', NoDiskSpaceErrorStory)
  .add('SystemTimeError', SystemTimeErrorStory)
  .add('ManualUpdate', ManualUpdateStory);
