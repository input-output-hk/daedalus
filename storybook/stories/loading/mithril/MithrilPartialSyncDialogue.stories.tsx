import React, { useEffect, useRef } from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../../_support/StoryDecorator';
import SyncingConnectingMithrilPrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
import styles from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss';

// onStart must return a Promise so the confirm-view "Start now" await resolves like the real store call.
const proactivePromptBaseProps = {
  onStart: async () => {
    action('onStart')();
  },
  onDismiss: action('onDismiss'),
};

function ConfirmViewPrompt({ behindByEpochs }: { behindByEpochs?: number }) {
  const containerRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    // Reach the confirm view through the real click path. Selecting by the
    // component's own .scss class stays truthful across copy edits and locale
    // switches; a missing button fails loudly instead of showing choice view.
    const actionButton = containerRef.current?.querySelector<HTMLButtonElement>(
      `button.${styles.primaryAction}`
    );
    if (!actionButton) {
      throw new Error('confirm-view story: primary action button not found');
    }
    actionButton.click();
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
