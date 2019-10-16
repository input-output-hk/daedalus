// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import {
  DefaultSyncingConnectingStory,
  ConnectivityIssuesSyncingConnectingStory,
  SyncIssuesSyncingConnectingStory,
} from './Loading-SyncingConnecting.stories';
import { NoDiskSpaceErrorStory } from './Loading-NoDiskSpaceError.stories';
import { SystemTimeErrorStory } from './Loading-SystemTimeError.stories';
import { DataLayerMigrationStory } from './Loading-DataLayerMigration.stories';

storiesOf('Loading', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('SyncingConnecting - Default', DefaultSyncingConnectingStory)
  .add(
    'SyncingConnecting - Connectivity Issues',
    ConnectivityIssuesSyncingConnectingStory
  )
  .add('SyncingConnecting - Sync Issues', SyncIssuesSyncingConnectingStory)
  .add('NoDiskSpaceError', NoDiskSpaceErrorStory)
  .add('SystemTimeError', SystemTimeErrorStory)
  .add('DataLayerMigrationStory', DataLayerMigrationStory);
