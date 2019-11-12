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
  ConnectivityIssuesSyncingConnectingStoryForIncentivizedTestnet,
  SyncIssuesSyncingConnectingStory,
  SyncIssuesSyncingConnectingStoryForIncentivizedTestnet,
} from './Loading-SyncingConnecting.stories';
import { NoDiskSpaceErrorStory } from './Loading-NoDiskSpaceError.stories';
import { SystemTimeErrorStory } from './Loading-SystemTimeError.stories';
import { ManualUpdateStory } from './Loading-ManualUpdate.stories';
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
  .add(
    'SyncingConnecting - Connectivity Issues For Incentivized Testnet',
    ConnectivityIssuesSyncingConnectingStoryForIncentivizedTestnet
  )
  .add('SyncingConnecting - Sync Issues', SyncIssuesSyncingConnectingStory)
  .add(
    'SyncingConnecting - Sync Issues For Incentivized Testnet',
    SyncIssuesSyncingConnectingStoryForIncentivizedTestnet
  )
  .add('NoDiskSpaceError', NoDiskSpaceErrorStory)
  .add('SystemTimeError', SystemTimeErrorStory)
  .add('ManualUpdate', ManualUpdateStory)
  .add('DataLayerMigrationStory', DataLayerMigrationStory);
