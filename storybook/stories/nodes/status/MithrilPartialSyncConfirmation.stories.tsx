import React from 'react';
import type { ComponentProps } from 'react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import MithrilPartialSyncConfirmation from '../../../../source/renderer/app/components/status/MithrilPartialSyncConfirmation';

type ConfirmationProps = ComponentProps<typeof MithrilPartialSyncConfirmation>;

const confirmationBaseProps: ConfirmationProps = {
  isActionBlocked: false,
  startError: null,
  onCancel: action('onCancel'),
  onConfirm: action('onConfirm'),
};

// StoryWrapper hands currentTheme to the story as a prop (first parameter);
// keying the modal on it remounts per theme switch.
const renderConfirmationStory = (
  storyProps: Partial<ConfirmationProps> = {}
): ((props: { currentTheme: string }) => JSX.Element) =>
  function RenderConfirmationStory(props: { currentTheme: string }) {
    return (
      <MithrilPartialSyncConfirmation
        key={props.currentTheme}
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
