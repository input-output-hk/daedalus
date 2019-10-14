// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number, boolean } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import BlockConsolidationStatus from '../../../../source/renderer/app/components/status/BlockConsolidationStatus';

const currentEpoch = 95;

storiesOf('Nodes|Status', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('Block Consolidation Status', () => (
    <BlockConsolidationStatus
      onExternalLinkClick={() => {}}
      epochsConsolidated={number('Epochs Consolidated', 57, {
        range: true,
        min: 0,
        max: currentEpoch - 2,
        step: 1,
      })}
      epochsSynced={number('Epochs Synced', 80, {
        range: true,
        min: 0,
        max: 100,
        step: 1,
      })}
      onClose={() => {}}
      currentEpoch={boolean('Has `currentEpoch`', true) ? currentEpoch : 0}
    />
  ));
