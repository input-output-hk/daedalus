import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../../_support/StoryDecorator';
import SyncingConnectingMithrilPrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';

// Proactive (syncing-screen) prompt fixtures. `onStart` MUST return a Promise so
// the confirm-view "Start now" await resolves like the real store call.
const proactivePromptBaseProps = {
  onStart: async () => {
    action('onStart')();
  },
  onDismiss: action('onDismiss'),
};

// Moved out of "Nodes / Status" (now "Nodes / Diagnostic") into the Mithril
// loading group so the proactive dialogue lives with the other Mithril loading
// views and renders the full styled dialogue (not a bare text block). The prompt
// pulls intl + its own .scss at render, so theme switching stays truthful.
storiesOf('Loading / Mithril / Mithril Partial Sync Dialogue', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Known Epochs Behind', () => (
    <SyncingConnectingMithrilPrompt
      {...proactivePromptBaseProps}
      behindByEpochs={120}
    />
  ))
  .add('Unknown Behind', () => (
    <SyncingConnectingMithrilPrompt {...proactivePromptBaseProps} />
  ));
