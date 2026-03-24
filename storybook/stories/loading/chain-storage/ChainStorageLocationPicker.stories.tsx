import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
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
  defaultChainPath,
  snapshotSize,
  validationPresetOptions,
} from '../_support/mithrilFixtures';

storiesOf('Loading / Chain Storage', module)
  .addDecorator((story, context) => (
    <StoryDecorator>
      <LoadingOverlayStoryFrame>
        {withKnobs(story, context)}
      </LoadingOverlayStoryFrame>
    </StoryDecorator>
  ))
  .add('Interactive Picker', () => {
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
          loadingNumberKnob('estimatedRequiredSpaceGiB', 82) *
            1024 *
            1024 *
            1024
        )}
        availableSpaceBytes={Math.round(
          loadingNumberKnob('availableSpaceGiB', 256) * 1024 * 1024 * 1024
        )}
        isChainStorageLoading={loadingBooleanKnob(
          'isChainStorageLoading',
          false
        )}
      />
    );
  })
  .add('Invalid Current Path', () => (
    <ManagedChainStorageLocationPicker
      customChainPath="/mnt/slow-disk/daedalus-chain"
      defaultChainPath={defaultChainPath}
      validationPreset="insufficient-space"
      estimatedRequiredSpaceBytes={snapshotSize}
      availableSpaceBytes={32 * 1024 * 1024 * 1024}
    />
  ))
  .add('Busy State', () => (
    <ManagedChainStorageLocationPicker
      customChainPath="/mnt/fast-ssd/daedalus-chain"
      defaultChainPath={defaultChainPath}
      validationPreset="valid-custom"
      estimatedRequiredSpaceBytes={snapshotSize}
      availableSpaceBytes={256 * 1024 * 1024 * 1024}
      isChainStorageLoading
    />
  ));
