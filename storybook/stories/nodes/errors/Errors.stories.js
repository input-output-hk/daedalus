// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';
import { NoDiskSpaceErrorStory } from './NoDiskSpaceError.stories';
import { SystemTimeErrorStory } from './SystemTimeError.stories';

storiesOf('Nodes | Errors', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======
  .add('No Disk Space Error', NoDiskSpaceErrorStory)
  .add('System Time Error', SystemTimeErrorStory);
