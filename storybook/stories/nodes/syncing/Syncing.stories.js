// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Stories
import {
  DefaultSyncingConnectingStory,
  ConnectivityIssuesSyncingConnectingStory,
  SyncIssuesSyncingConnectingStory,
} from './SyncingConnecting.stories';

storiesOf('Nodes|Syncing and Connecting', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======
  .add('Default', DefaultSyncingConnectingStory)
  .add('Connectivity Issues', ConnectivityIssuesSyncingConnectingStory)
  .add('Sync Issues', SyncIssuesSyncingConnectingStory);
