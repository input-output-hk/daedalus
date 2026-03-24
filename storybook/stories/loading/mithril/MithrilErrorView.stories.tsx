import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import MithrilErrorView from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';
import { loadingSelectKnob, loadingTextKnob } from '../_support/loadingKnobs';
import {
  bootstrapActions,
  errorStageOptions,
  getErrorPreset,
} from '../_support/mithrilFixtures';

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
        onWipeRetry={() => bootstrapActions.onWipeRetry()}
        onDecline={() => bootstrapActions.onDecline()}
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
      onWipeRetry={() => bootstrapActions.onWipeRetry()}
      onDecline={() => bootstrapActions.onDecline()}
    />
  ));
