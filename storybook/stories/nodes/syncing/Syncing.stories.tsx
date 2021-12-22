import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';
// Stories
import {
  DefaultSyncingConnectingStory,
  LoadingWalletDataSyncingConnectingStory,
  ConnectivityIssuesSyncingConnectingStory,
} from './SyncingConnecting.stories';

storiesOf('Nodes|Connecting and Loading', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  )) // ====== Stories ======
  .add('Connecting', DefaultSyncingConnectingStory)
  .add('Trouble Connecting', ConnectivityIssuesSyncingConnectingStory)
  .add('Loading Wallet Data', LoadingWalletDataSyncingConnectingStory);
