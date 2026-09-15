import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';
// Stories
import { DataLayerMigrationStory } from './_support/DataLayerMigration';

export default {
  title: 'Nodes / Updates',

  decorators: [
    (story, context) => (
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    ),
  ],
};

export const DataLayerMigration = DataLayerMigrationStory;
