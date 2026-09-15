import React from 'react';
import type { ComponentProps } from 'react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import MithrilPartialSyncConfirmation from '../../../../source/renderer/app/components/status/MithrilPartialSyncConfirmation';
import { currentThemeOf } from '../../_support/globals';

type ConfirmationProps = ComponentProps<typeof MithrilPartialSyncConfirmation>;

const confirmationBaseProps: ConfirmationProps = {
  isActionBlocked: false,
  startError: null,
  onCancel: action('onCancel'),
  onConfirm: action('onConfirm'),
};

// Keying the modal on the selected theme remounts it when the theme switches.
// The theme comes from the story context, the second argument a render function
// is given; the first is the story's args and carries nothing here.
const renderConfirmationStory = (storyProps: Partial<ConfirmationProps> = {}) =>
  function RenderConfirmationStory(_args: unknown, context: unknown) {
    return (
      <MithrilPartialSyncConfirmation
        key={currentThemeOf(context)}
        {...confirmationBaseProps}
        {...storyProps}
      />
    );
  };

export default {
  title: 'Nodes / Diagnostic / Mithril Partial Sync Confirmation',
  decorators: [(story) => <StoryDecorator>{story()}</StoryDecorator>],
};

export const KnownEpochsBehind = renderConfirmationStory({
  behindByEpochs: 42,
});
export const UnknownBehind = renderConfirmationStory();
export const AtOrPastSnapshot = renderConfirmationStory({
  isAtOrPastSnapshot: true,
});

export const StartError = renderConfirmationStory({
  behindByEpochs: 42,
  startError:
    'Unable to start Mithril sync. Cardano node did not stop in time.',
});
