// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';

// Screens
import BlockConsolidationStatus from '../../source/renderer/app/components/status/BlockConsolidationStatus';

const epochsDownloaded = 93;
const totalEpochs = 95;

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
      epochsConsolidated={number('Epochs Consolidated', 57, { range: true, min: 0, max: epochsDownloaded, step: 1, })}
      epochsDownloaded={number('Epochs Downloaded', epochsDownloaded, { range: true, min: 0, max: totalEpochs - 2, step: 1, })}
      totalEpochs={totalEpochs}
      epochsSynced={number('Epochs Synced', 80, { range: true, min: 0, max: 100, step: 1, })}
    />
  ));

