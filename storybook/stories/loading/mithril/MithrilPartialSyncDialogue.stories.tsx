import React, { useEffect, useRef } from 'react';
import { action } from '@storybook/addon-actions';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../../_support/StoryDecorator';
import { applyEnvironmentOs } from '../../_support/environment';
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

// StoryWrapper injects the DaedalusMenu OS selection onto the story *context*
// (the second render arg) — not the args object — so read it from there and
// mirror it onto global.environment. This is what lets the prompt's
// platform-aware shortcut note ("Cmd + D" on macOS, "Ctrl + D" elsewhere)
// track the toolbar OS switch.
const applyStoryOs = (context: unknown) =>
  applyEnvironmentOs((context as { osName?: string }).osName ?? '');

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

storiesOf('Loading / Mithril / Mithril Partial Sync Dialogue', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('Known Epochs Behind', (_args, context) => {
    applyStoryOs(context);
    return (
      <SyncingConnectingMithrilPrompt
        {...makePromptProps(startFailsKnob())}
        behindByEpochs={behindByEpochsKnob()}
      />
    );
  })
  .add('Known Epochs Behind / Confirm View', (_args, context) => {
    applyStoryOs(context);
    return (
      <ConfirmViewPrompt
        behindByEpochs={behindByEpochsKnob()}
        startFails={startFailsKnob()}
      />
    );
  })
  // Drives the container's real behind-ness derivation instead of a canned
  // figure: computeBehindByEpochs anchors on the network tip when known and
  // otherwise falls back to the Mithril certified snapshot epoch. With the
  // defaults the snapshot is later than the local tip while no network tip has
  // resolved yet (early sync / ledger replay), so the epochs figure comes from
  // the snapshot; raise localTipEpoch to or past mithrilSnapshotEpoch and the
  // prompt degrades to the "behind the blockchain tip" line (gap <= 0 never
  // renders as a number).
  .add('Snapshot Ahead Of Local Tip (Derived)', (_args, context) => {
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
  })
  .add('Unknown Behind', (_args, context) => {
    applyStoryOs(context);
    return (
      <SyncingConnectingMithrilPrompt {...makePromptProps(startFailsKnob())} />
    );
  })
  .add('Unknown Behind / Confirm View', (_args, context) => {
    applyStoryOs(context);
    return <ConfirmViewPrompt startFails={startFailsKnob()} />;
  });
