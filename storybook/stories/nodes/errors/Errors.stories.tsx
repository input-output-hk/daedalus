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
  )) // ====== Stories ======
  .add('No Disk Space Error', NoDiskSpaceErrorStory)
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('System Time Error', SystemTimeErrorStory);
