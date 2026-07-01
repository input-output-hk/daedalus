import React, { useEffect, useRef } from 'react';
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

function ConfirmViewPrompt({ behindByEpochs }: { behindByEpochs?: number }) {
  const containerRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    const actionButton = Array.from(
      containerRef.current?.querySelectorAll('button') || []
    ).find((button) => button.textContent === 'Mithril Sync (fast)');

    actionButton?.click();
  }, []);

  return (
    <div ref={containerRef}>
      <SyncingConnectingMithrilPrompt
        {...proactivePromptBaseProps}
        behindByEpochs={behindByEpochs}
      />
    </div>
  );
}

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
  .add('Known Epochs Behind / Confirm View', () => (
    <ConfirmViewPrompt behindByEpochs={120} />
  ))
  .add('Unknown Behind', () => (
    <SyncingConnectingMithrilPrompt {...proactivePromptBaseProps} />
  ))
  .add('Unknown Behind / Confirm View', () => <ConfirmViewPrompt />);
