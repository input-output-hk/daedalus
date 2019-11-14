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
  ConnectivityIssuesSyncingConnectingStoryForIncentivizedTestnet,
  SyncIssuesSyncingConnectingStoryForIncentivizedTestnet,
} from './SyncingConnecting.stories';

storiesOf('Nodes|Syncing and Connecting', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======
  .add('Default', DefaultSyncingConnectingStory)
  .add('Trouble Connecting', ConnectivityIssuesSyncingConnectingStory)
  .add(
    ' Trouble Connecting For Incentivized Testnet',
    ConnectivityIssuesSyncingConnectingStoryForIncentivizedTestnet
  )
  .add('Trouble Syncing', SyncIssuesSyncingConnectingStory)
  .add(
    'Trouble Syncing For Incentivized Testnet',
    SyncIssuesSyncingConnectingStoryForIncentivizedTestnet
  );
