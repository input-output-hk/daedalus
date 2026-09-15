import React, { useEffect, useRef } from 'react';
import { action } from '@storybook/addon-actions';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import { applyEnvironmentOs } from '../../_support/environment';
import { osNameOf } from '../../_support/globals';
import SyncingConnectingMithrilPrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
import styles from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss';
import { computeBehindByEpochs } from '../../../../source/renderer/app/utils/mithrilBehindness';
import {
  loadingBooleanKnob,
  loadingNumberKnob,
} from '../_support/loadingKnobs';

// onStart must return a Promise so the confirm-view "Start now" await resolves
// like the real store call; a rejection surfaces the inline confirm-view error.
const makePromptProps = (startFails: boolean) => ({
  onStart: async () => {
    action('onStart')();
    if (startFails) {
      throw new Error('Simulated start rejection from the startFails knob');
    }
  },
  onDismiss: action('onDismiss'),
});

const behindByEpochsKnob = () => loadingNumberKnob('behindByEpochs', 120);
const startFailsKnob = () => loadingBooleanKnob('startFails', false);

// The OS selection reaches a story on the context, the second render argument,
// and this mirrors it onto global.environment so the prompt's platform-aware
// shortcut note ("Cmd + D" on macOS, "Ctrl + D" elsewhere) tracks the toolbar
// switch.
const applyStoryOs = (context: unknown) =>
  applyEnvironmentOs(osNameOf(context));

// Only the epoch is read by the behind-ness derivation.
const makeTip = (epoch: number) => ({
  epoch,
  slot: 0,
  absoluteSlotNumber: 0,
});

function ConfirmViewPrompt({
  behindByEpochs,
  startFails,
}: {
  behindByEpochs?: number;
  startFails: boolean;
}) {
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
        {...makePromptProps(startFails)}
        behindByEpochs={behindByEpochs}
      />
    </div>
  );
}

export default {
  title: 'Loading / Mithril / Mithril Partial Sync Dialogue',

  decorators: [
    (story, context) => (
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    ),
  ],
};

export const KnownEpochsBehind = {
  render: (_args, context) => {
    applyStoryOs(context);
    return (
      <SyncingConnectingMithrilPrompt
        {...makePromptProps(startFailsKnob())}
        behindByEpochs={behindByEpochsKnob()}
      />
    );
  },
};

export const KnownEpochsBehindConfirmView = {
  render: (_args, context) => {
    applyStoryOs(context);
    return (
      <ConfirmViewPrompt
        behindByEpochs={behindByEpochsKnob()}
        startFails={startFailsKnob()}
      />
    );
  },

  name: 'Known Epochs Behind / Confirm View',
};

export const SnapshotAheadOfLocalTipDerived = {
  render: (_args, context) => {
    applyStoryOs(context);
    const localTipEpoch = loadingNumberKnob('localTipEpoch', 412);
    const mithrilSnapshotEpoch = loadingNumberKnob('mithrilSnapshotEpoch', 512);
    const isNetworkTipKnown = loadingBooleanKnob('networkTipKnown', false);
    const networkTipEpoch = loadingNumberKnob('networkTipEpoch', 513);

    return (
      <SyncingConnectingMithrilPrompt
        {...makePromptProps(startFailsKnob())}
        behindByEpochs={computeBehindByEpochs(
          makeTip(localTipEpoch),
          isNetworkTipKnown ? makeTip(networkTipEpoch) : null,
          mithrilSnapshotEpoch
        )}
      />
    );
  },

  name: 'Snapshot Ahead Of Local Tip (Derived)',
};

export const UnknownBehind = {
  render: (_args, context) => {
    applyStoryOs(context);
    return (
      <SyncingConnectingMithrilPrompt {...makePromptProps(startFailsKnob())} />
    );
  },
};

export const UnknownBehindConfirmView = {
  render: (_args, context) => {
    applyStoryOs(context);
    return <ConfirmViewPrompt startFails={startFailsKnob()} />;
  },

  name: 'Unknown Behind / Confirm View',
};
