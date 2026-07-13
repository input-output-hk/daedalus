import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import type { MithrilBootstrapStatus } from '../../../../source/common/types/mithril-bootstrap.types';
import MithrilProgressView from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';
import { loadingNumberKnob, loadingRadiosKnob } from '../_support/loadingKnobs';
import {
  ancillaryBytesTotal,
  bootstrapActions,
  createBootstrapStartedAt,
  getBootstrapProgressItems,
  snapshotFilesTotal,
  snapshotSize,
} from '../_support/mithrilFixtures';

const statusOptions: Record<string, MithrilBootstrapStatus> = {
  Preparing: 'preparing',
  Downloading: 'downloading',
  Verifying: 'verifying',
  Converting: 'converting',
  Unpacking: 'unpacking',
  Finalizing: 'finalizing',
  'Starting Node': 'starting-node',
};

const makePercentKnob = (name: string, value: number) =>
  loadingNumberKnob(name, value, {
    range: true,
    min: 0,
    max: 100,
    step: 1,
  });

storiesOf('Loading / Mithril / Progress', module)
  .addDecorator((story, context) => (
    <StoryDecorator>
      <LoadingOverlayStoryFrame>
        {withKnobs(story, context)}
      </LoadingOverlayStoryFrame>
    </StoryDecorator>
  ))
  .add('Interactive Working State', () => {
    const status = loadingRadiosKnob('status', statusOptions, 'downloading');
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
      <MithrilProgressView
        status={status}
        progressItems={getBootstrapProgressItems(status)}
        filesDownloaded={Math.round(
          snapshotFilesTotal * (snapshotDownloadPercent / 100)
        )}
        filesTotal={snapshotFilesTotal}
        snapshotSizeBytes={snapshotSize}
        ancillaryBytesDownloaded={Math.round(
          ancillaryBytesTotal * (ancillaryPercent / 100)
        )}
        ancillaryBytesTotal={ancillaryBytesTotal}
        ancillaryProgress={ancillaryPercent}
        bootstrapStartedAt={createBootstrapStartedAt(elapsedMinutes)}
        onAction={() => bootstrapActions.onCancel()}
      />
    );
  })
  .add('Preparing', () => (
    <MithrilProgressView
      status="preparing"
      progressItems={getBootstrapProgressItems('preparing')}
      snapshotSizeBytes={snapshotSize}
      bootstrapStartedAt={createBootstrapStartedAt(4)}
      onAction={() => bootstrapActions.onCancel()}
    />
  ))
  .add('Starting Node Handoff', () => (
    <MithrilProgressView
      status="starting-node"
      progressItems={getBootstrapProgressItems('starting-node')}
      filesDownloaded={snapshotFilesTotal}
      filesTotal={snapshotFilesTotal}
      snapshotSizeBytes={snapshotSize}
      ancillaryBytesDownloaded={ancillaryBytesTotal}
      ancillaryBytesTotal={ancillaryBytesTotal}
      ancillaryProgress={100}
      bootstrapStartedAt={createBootstrapStartedAt(24)}
      onAction={() => bootstrapActions.onCancel()}
    />
  ));
