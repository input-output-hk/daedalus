import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import type { MithrilBootstrapStatus } from '../../../../source/common/types/mithril-bootstrap.types';
import StoryDecorator from '../../_support/StoryDecorator';
import { ManagedMithrilBootstrap } from '../_support/mithrilHarness';
import {
  loadingBooleanKnob,
  loadingNumberKnob,
  loadingRadiosKnob,
  loadingSelectKnob,
} from '../_support/loadingKnobs';
import {
  ancillaryBytesTotal,
  createBootstrapStartedAt,
  defaultChainPath,
  defaultChainStorageValidation,
  errorStageOptions,
  explicitSnapshot,
  getErrorPreset,
  getProgressItemsPreset,
  latestSnapshot,
  progressPresetOptions,
  snapshotSize,
  snapshots,
  validationPresetOptions,
} from '../_support/mithrilFixtures';

const statusOptions: Record<string, MithrilBootstrapStatus> = {
  Decision: 'decision',
  Preparing: 'preparing',
  Downloading: 'downloading',
  Verifying: 'verifying',
  Finalizing: 'finalizing',
  'Starting Node': 'starting-node',
  Failed: 'failed',
  Cancelled: 'cancelled',
};

const snapshotSelectionOptions = {
  Latest: 'latest',
  'Explicit Snapshot': explicitSnapshot.digest,
};

const makePercentKnob = (name: string, value: number) =>
  loadingNumberKnob(name, value, {
    range: true,
    min: 0,
    max: 100,
    step: 1,
  });

storiesOf('Loading / Mithril / Bootstrap', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('Interactive Shell', () => {
    const status = loadingRadiosKnob('status', statusOptions, 'decision');
    const validationPreset = loadingSelectKnob(
      'chainStorageValidationPreset',
      validationPresetOptions,
      'valid-custom'
    );
    const progressPreset = loadingSelectKnob(
      'progressPreset',
      progressPresetOptions,
      'download-mid'
    );
    const errorStage = loadingSelectKnob(
      'errorStage',
      errorStageOptions,
      'download'
    );
    const selectedDigest = loadingSelectKnob(
      'selectedSnapshot',
      snapshotSelectionOptions,
      'latest'
    );
    const snapshotDownloadPercent = makePercentKnob(
      'snapshotDownloadPercent',
      47
    );
    const ancillaryPercent = makePercentKnob('ancillaryPercent', 62);
    const elapsedMinutes = loadingNumberKnob('elapsedMinutes', 18, {
      range: true,
      min: 0,
      max: 180,
      step: 1,
    });

    return (
      <ManagedMithrilBootstrap
        status={status}
        snapshots={snapshots}
        selectedDigest={selectedDigest === 'latest' ? null : selectedDigest}
        initialStorageLocationConfirmed={loadingBooleanKnob(
          'storageLocationConfirmed',
          true
        )}
        customChainPath={textOrNull(
          'customChainPath',
          '/mnt/fast-ssd/daedalus-chain'
        )}
        defaultChainPath={defaultChainPath}
        defaultChainStorageValidation={defaultChainStorageValidation}
        latestSnapshotSize={snapshotSize}
        isFetchingSnapshots={loadingBooleanKnob('isFetchingSnapshots', false)}
        validationPreset={validationPreset}
        availableSpaceBytes={Math.round(
          loadingNumberKnob('availableSpaceGiB', 256) * 1024 * 1024 * 1024
        )}
        isChainStorageLoading={loadingBooleanKnob(
          'isChainStorageLoading',
          false
        )}
        bytesDownloaded={Math.round(
          snapshotSize * (snapshotDownloadPercent / 100)
        )}
        snapshotSize={snapshotSize}
        ancillaryBytesDownloaded={Math.round(
          ancillaryBytesTotal * (ancillaryPercent / 100)
        )}
        ancillaryBytesTotal={ancillaryBytesTotal}
        ancillaryProgress={ancillaryPercent}
        progressItems={getProgressItemsPreset(progressPreset)}
        bootstrapStartedAt={createBootstrapStartedAt(elapsedMinutes)}
        error={getErrorPreset(errorStage)}
      />
    );
  })
  .add('Storage To Decision Routing', () => (
    <ManagedMithrilBootstrap
      status="decision"
      snapshots={snapshots}
      selectedDigest={latestSnapshot.digest}
      initialStorageLocationConfirmed={false}
      customChainPath="/mnt/fast-ssd/daedalus-chain"
      defaultChainPath={defaultChainPath}
      defaultChainStorageValidation={defaultChainStorageValidation}
      latestSnapshotSize={snapshotSize}
      isFetchingSnapshots={false}
      validationPreset="valid-custom"
      availableSpaceBytes={256 * 1024 * 1024 * 1024}
    />
  ));

function textOrNull(name: string, value: string) {
  const nextValue = loadingSelectKnob(
    name,
    { Default: '', Custom: value },
    value
  );
  return nextValue || null;
}
