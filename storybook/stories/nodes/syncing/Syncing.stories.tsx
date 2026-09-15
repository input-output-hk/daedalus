import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';
// Stories
import {
  DefaultSyncingConnectingStory,
  LoadingWalletDataSyncingConnectingStory,
  ConnectivityIssuesSyncingConnectingStory,
} from './_support/SyncingConnecting';

export default {
  title: 'Nodes / Connecting and Loading',

  decorators: [
    (story, context) => (
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    ),
  ],
};

export const Connecting = DefaultSyncingConnectingStory;
export const TroubleConnecting = ConnectivityIssuesSyncingConnectingStory;
export const LoadingWalletData = LoadingWalletDataSyncingConnectingStory;
