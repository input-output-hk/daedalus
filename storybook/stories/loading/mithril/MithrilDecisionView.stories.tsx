import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';
import { ManagedMithrilDecisionView } from '../_support/mithrilHarness';
import {
  loadingBooleanKnob,
  loadingRadiosKnob,
  loadingSelectKnob,
  loadingTextKnob,
} from '../_support/loadingKnobs';
import {
  defaultChainPath,
  explicitSnapshot,
  latestSnapshot,
  snapshots,
} from '../_support/mithrilFixtures';

const snapshotPresetOptions = {
  None: 'none',
  Single: 'single',
  Multiple: 'multiple',
};

const snapshotSelectionOptions = {
  Latest: 'latest',
  'Explicit Snapshot': explicitSnapshot.digest,
};

export default {
  title: 'Loading / Mithril / Snapshot Picker',

  decorators: [
    (story, context) => (
      <StoryDecorator>
        <LoadingOverlayStoryFrame>
          {withKnobs(story, context)}
        </LoadingOverlayStoryFrame>
      </StoryDecorator>
    ),
  ],
};

export const InteractiveDecisionView = () => {
  const snapshotPreset = loadingRadiosKnob(
    'snapshotPreset',
    snapshotPresetOptions,
    'multiple'
  );
  let availableSnapshots = snapshots;

  if (snapshotPreset === 'none') {
    availableSnapshots = [];
  } else if (snapshotPreset === 'single') {
    availableSnapshots = [latestSnapshot];
  }

  const selectedDigest = loadingSelectKnob(
    'selectedSnapshot',
    snapshotSelectionOptions,
    'latest'
  );

  return (
    <ManagedMithrilDecisionView
      snapshots={availableSnapshots}
      selectedDigest={selectedDigest === 'latest' ? null : selectedDigest}
      isFetchingSnapshots={loadingBooleanKnob('isFetchingSnapshots', false)}
      customChainPath={
        loadingBooleanKnob('showCustomChainPath', true)
          ? loadingTextKnob('customChainPath', '/mnt/fast-ssd/daedalus-chain')
          : null
      }
      defaultChainPath={defaultChainPath}
      includeReturnToStorageAction={loadingBooleanKnob(
        'includeReturnToStorageAction',
        true
      )}
    />
  );
};

export const LoadingSnapshots = () => (
  <ManagedMithrilDecisionView
    snapshots={[]}
    selectedDigest={null}
    isFetchingSnapshots
    customChainPath="/mnt/fast-ssd/daedalus-chain"
    defaultChainPath={defaultChainPath}
  />
);

export const NoSnapshotsAvailable = () => (
  <ManagedMithrilDecisionView
    snapshots={[]}
    selectedDigest={null}
    isFetchingSnapshots={false}
    customChainPath="/mnt/fast-ssd/daedalus-chain"
    defaultChainPath={defaultChainPath}
  />
);
