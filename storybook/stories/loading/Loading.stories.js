// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from '../_support/StoryDecorator';

// Screens
import {
  DefaultSyncingConnectingStory,
  ConnectivityIssuesSyncingConnectingStory,
  SyncIssuesSyncingConnectingStory,
} from './Loading-SyncingConnecting.stories';
import { NoDiskSpaceErrorStory } from './Loading-NoDiskSpaceError.stories';
import { SystemTimeErrorStory } from './Loading-SystemTimeError.stories';
import { ManualUpdateStory } from './Loading-ManualUpdate.stories';
import { DataLayerMigrationStory } from './Loading-DataLayerMigration.stories';

storiesOf('Loading|Syncing and Connecting', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======
  .add('SyncingConnecting - Default', DefaultSyncingConnectingStory)
  .add(
    'SyncingConnecting - Connectivity Issues',
    ConnectivityIssuesSyncingConnectingStory
  )
  .add('SyncingConnecting - Sync Issues', SyncIssuesSyncingConnectingStory);

storiesOf('Loading|Overlays', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======
  .add('NoDiskSpaceError', NoDiskSpaceErrorStory)
  .add('SystemTimeError', SystemTimeErrorStory)
  .add('ManualUpdate', ManualUpdateStory)
  .add('DataLayerMigrationStory', DataLayerMigrationStory);
