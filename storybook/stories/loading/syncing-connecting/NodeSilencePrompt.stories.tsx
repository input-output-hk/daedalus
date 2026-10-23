import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import NodeSilencePrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/NodeSilencePrompt';
import StoryDecorator from '../../_support/StoryDecorator';

storiesOf('Loading / Startup Silence', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Five Minutes', () => (
    <NodeSilencePrompt
      silentSecs={300}
      onRestart={action('onRestart')}
      onKeepWaiting={action('onKeepWaiting')}
    />
  ));
