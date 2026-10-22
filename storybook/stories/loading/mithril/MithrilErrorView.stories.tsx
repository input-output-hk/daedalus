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

const chooseLocationAction = {
  label: 'Choose another location',
  onClick: () => bootstrapActions.onDecline(),
  variant: 'secondary' as const,
};

// A rebuild refused before it deleted anything: the node runs again on the
// existing blockchain data, and Not now continues with it
const notNowAction = {
  label: 'Not now',
  onClick: () => bootstrapActions.onDecline(),
  variant: 'secondary' as const,
};

const rebuildRefusedActions = [errorActions[0], notNowAction];

const rebuildLocationRefusedActions = [
  errorActions[0],
  chooseLocationAction,
  notNowAction,
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
  .add('Rebuild Probe Failed', () => (
    <MithrilErrorView
      error={{ code: 'PROBE_FAILED', message: 'detail from the watchdog' }}
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={rebuildRefusedActions}
    />
  ))
  .add('Invalid Chain Path', () => (
    <MithrilErrorView
      error={{
        code: 'INVALID_CHAIN_PATH',
        message: 'detail from the watchdog',
      }}
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={rebuildRefusedActions}
    />
  ))
  .add('Chain Dir Not Replaceable', () => (
    <MithrilErrorView
      error={{
        code: 'CHAIN_DIR_NOT_REPLACEABLE',
        message: 'detail from the watchdog',
      }}
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={rebuildLocationRefusedActions}
    />
  ))
  .add('Chain Dir Other Network', () => (
    <MithrilErrorView
      error={{
        code: 'CHAIN_DIR_OTHER_NETWORK',
        message: 'detail from the watchdog',
      }}
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={rebuildRefusedActions}
    />
  ))
  .add('Insufficient Space', () => (
    <MithrilErrorView
      error={{
        code: 'INSUFFICIENT_SPACE',
        message: 'detail from the watchdog',
      }}
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={rebuildLocationRefusedActions}
    />
  ))
  .add('Rebuild Marker Failed', () => (
    <MithrilErrorView
      error={{
        code: 'REBUILD_MARKER_FAILED',
        message: 'detail from the watchdog',
      }}
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={rebuildRefusedActions}
    />
  ))
  .add('Probe Failed After Data Removed', () => (
    <MithrilErrorView
      error={{ code: 'PROBE_FAILED', message: 'detail from the watchdog' }}
      chainRemoved
      rebuild
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
        code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
        message: 'detail from the watchdog',
      }}
      chainRemoved
      rebuild
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
  .add('Install Failed', () => (
    <MithrilErrorView
      error={{ code: 'INSTALL_FAILED', message: 'detail from the watchdog' }}
      installUnfinished
      retryOnly
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={[errorActions[0]]}
    />
  ))
  .add('Probe Failed After An Unfinished Install', () => (
    <MithrilErrorView
      error={{ code: 'PROBE_FAILED', message: 'detail from the watchdog' }}
      installUnfinished
      rebuild
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
  .add('Rebuild Install Failed After Data Removed', () => (
    <MithrilErrorView
      error={{ code: 'INSTALL_FAILED', message: 'detail from the watchdog' }}
      chainRemoved
      retryOnly
      rebuild
      onOpenExternalLink={(value) => bootstrapActions.onOpenExternalLink(value)}
      actions={[errorActions[0]]}
    />
  ))
  // The rebuild recorded itself before the delete failed, so the watchdog
  // reports the rebuild as pending and no chain to start the node on
  .add('Wipe Failed', () => (
    <MithrilErrorView
      error={{ code: 'WIPE_FAILED', message: 'detail from the watchdog' }}
      chainRemoved
      rebuild
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
  ));
