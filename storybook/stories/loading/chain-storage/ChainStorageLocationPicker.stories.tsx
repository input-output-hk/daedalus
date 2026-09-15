import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';
import { ManagedChainStorageLocationPicker } from '../_support/mithrilHarness';
import {
  loadingBooleanKnob,
  loadingNumberKnob,
  loadingSelectKnob,
  loadingTextKnob,
} from '../_support/loadingKnobs';
import {
  defaultChainStorageValidation,
  defaultChainPath,
  snapshotSize,
  validationPresetOptions,
} from '../_support/mithrilFixtures';

export default {
  title: 'Loading / Chain Storage',

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

export const InteractivePicker = () => {
  const validationPreset = loadingSelectKnob(
    'validationPreset',
    validationPresetOptions,
    'valid-custom'
  );

  return (
    <ManagedChainStorageLocationPicker
      customChainPath={
        loadingBooleanKnob('useCustomChainPath', true)
          ? loadingTextKnob('customChainPath', '/mnt/fast-ssd/daedalus-chain')
          : null
      }
      defaultChainPath={defaultChainPath}
      validationPreset={validationPreset}
      estimatedRequiredSpaceBytes={Math.round(
        loadingNumberKnob('estimatedRequiredSpaceGiB', 82) * 1024 * 1024 * 1024
      )}
      availableSpaceBytes={Math.round(
        loadingNumberKnob('availableSpaceGiB', 256) * 1024 * 1024 * 1024
      )}
      isChainStorageLoading={loadingBooleanKnob('isChainStorageLoading', false)}
    />
  );
};

export const InvalidCurrentPath = () => (
  <ManagedChainStorageLocationPicker
    customChainPath="/mnt/slow-disk/daedalus-chain"
    defaultChainPath={defaultChainPath}
    validationPreset="insufficient-space"
    estimatedRequiredSpaceBytes={snapshotSize}
    availableSpaceBytes={32 * 1024 * 1024 * 1024}
  />
);

export const BusyState = () => (
  <ManagedChainStorageLocationPicker
    customChainPath="/mnt/fast-ssd/daedalus-chain"
    defaultChainPath={defaultChainPath}
    validationPreset="valid-custom"
    estimatedRequiredSpaceBytes={snapshotSize}
    availableSpaceBytes={256 * 1024 * 1024 * 1024}
    isChainStorageLoading
  />
);

export const RecoveryFallback = () => (
  <ManagedChainStorageLocationPicker
    customChainPath={null}
    defaultChainPath={defaultChainPath}
    validationPreset="valid-default"
    estimatedRequiredSpaceBytes={snapshotSize}
    availableSpaceBytes={256 * 1024 * 1024 * 1024}
    isRecoveryFallback
  />
);

export const DataFound = () => (
  <ManagedChainStorageLocationPicker
    customChainPath="/mnt/fast-ssd/daedalus-chain"
    defaultChainPath={defaultChainPath}
    validationPreset="existing-directory"
    estimatedRequiredSpaceBytes={snapshotSize}
    availableSpaceBytes={256 * 1024 * 1024 * 1024}
  />
);

export const RecoveryDataFound = {
  render: () => (
    <ManagedChainStorageLocationPicker
      customChainPath={null}
      defaultChainPath={defaultChainPath}
      validationPreset="valid-default"
      defaultChainStorageValidation={{
        ...defaultChainStorageValidation,
        chainSubdirectoryStatus: 'existing-directory',
      }}
      estimatedRequiredSpaceBytes={snapshotSize}
      availableSpaceBytes={256 * 1024 * 1024 * 1024}
      isRecoveryFallback
    />
  ),

  name: 'Recovery + Data Found',
};
