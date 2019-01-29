// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import BlockConsolidationStatus from '../../source/renderer/app/components/status/BlockConsolidationStatus';

const currentEpoch = 95;

storiesOf('BlockConsolidationStatus', module)

  .addDecorator((story, context) => (
    <StoryDecorator>
      {withKnobs(story, context)}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Default', () => (
    <BlockConsolidationStatus
      onExternalLinkClick={() => {}}
      epochsConsolidated={number('Epochs Consolidated', 57, { range: true, min: 0, max: currentEpoch - 2, step: 1, })}
      currentEpoch={currentEpoch}
      epochsSynced={number('Epochs Synced', 80, { range: true, min: 0, max: 100, step: 1, })}
      onClose={() => {}}
    />
  ));
