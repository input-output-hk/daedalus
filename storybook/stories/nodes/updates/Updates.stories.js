// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Stories
import { ManualUpdateStory } from './ManualUpdate.stories';
import {
  AutoUpdateWithVerison,
  AutoUpdateWithoutVersion,
} from './AutomaticUpdate.stories';
import { DataLayerMigrationStory } from './DataLayerMigration.stories';

storiesOf('Nodes|Updates', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======
  .add('Manual Update', ManualUpdateStory)
  .add('Automatic Update With Version', AutoUpdateWithVerison)
  .add('Automatic Update Without Version', AutoUpdateWithoutVersion)
  .add('Data Layer Migration', DataLayerMigrationStory);
