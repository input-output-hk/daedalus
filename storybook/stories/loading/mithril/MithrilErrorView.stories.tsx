import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import MithrilErrorView from '../../../../source/renderer/app/components/loading/mithril/MithrilErrorView';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';
import { loadingSelectKnob, loadingTextKnob } from '../_support/loadingKnobs';
import {
  bootstrapActions,
  errorStageOptions,
  getErrorPreset,
} from '../_support/mithrilFixtures';

const errorActions = [
  {
    label: 'Retry',
    onClick: () => bootstrapActions.onWipeRetry(),
    variant: 'primary' as const,
  },
  {
    label: 'Skip to blockchain sync',
    onClick: () => bootstrapActions.onDecline(),
    variant: 'secondary' as const,
  },
];

storiesOf('Loading / Mithril / Error', module)
  .addDecorator((story, context) => (
    <StoryDecorator>
      <LoadingOverlayStoryFrame>
        {withKnobs(story, context)}
      </LoadingOverlayStoryFrame>
    </StoryDecorator>
  ))
  .add('Interactive Error Stage', () => {
    const stage = loadingSelectKnob('stage', errorStageOptions, 'download');
    const preset = getErrorPreset(stage);

    return (
      <MithrilErrorView
        error={{
          ...preset,
          code: loadingTextKnob('code', preset.code || ''),
          message: loadingTextKnob('message', preset.message),
          logPath: loadingTextKnob('logPath', preset.logPath || ''),
        }}
        onOpenExternalLink={(value) =>
          bootstrapActions.onOpenExternalLink(value)
        }
        actions={errorActions}
      />
    );
  })
  .add('Generic Failure', () => (
    <MithrilErrorView
      error={{
        code: 'MITHRIL_UNKNOWN_FAILURE',
        message:
          'The bootstrap process failed before a stage-specific error could be derived.',
      }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Probe Failed', () => (
    <MithrilErrorView
      error={{ code: 'PROBE_FAILED', message: 'detail from the watchdog' }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Invalid Chain Path', () => (
    <MithrilErrorView
      error={{
        code: 'INVALID_CHAIN_PATH',
        message: 'detail from the watchdog',
      }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Chain Dir Not Replaceable', () => (
    <MithrilErrorView
      error={{
        code: 'CHAIN_DIR_NOT_REPLACEABLE',
        message: 'detail from the watchdog',
      }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Chain Dir Other Network', () => (
    <MithrilErrorView
      error={{
        code: 'CHAIN_DIR_OTHER_NETWORK',
        message: 'detail from the watchdog',
      }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Insufficient Space', () => (
    <MithrilErrorView
      error={{
        code: 'INSUFFICIENT_SPACE',
        message: 'detail from the watchdog',
      }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Marker Failed', () => (
    <MithrilErrorView
      error={{
        code: 'MARKER_FAILED',
        message: 'detail from the watchdog',
      }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ))
  .add('Rebuild Failed After Data Removed', () => (
    <MithrilErrorView
      error={{
        code: 'DOWNLOAD_FAILED',
        stage: 'download',
        message: 'detail from the watchdog',
      }}
      chainRemoved
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={[
        errorActions[0],
        {
          label: 'Choose how to sync',
          onClick: () => bootstrapActions.onDecline(),
          variant: 'secondary' as const,
        },
      ]}
    />
  ))
  .add('Wipe Failed', () => (
    <MithrilErrorView
      error={{ code: 'WIPE_FAILED', message: 'detail from the watchdog' }}
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={errorActions}
    />
  ));
