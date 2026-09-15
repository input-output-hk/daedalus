import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';
import { NoDiskSpaceErrorStory } from './_support/NoDiskSpaceError';
import { SystemTimeErrorStory } from './_support/SystemTimeError';

export default {
  title: 'Nodes / Errors',

  decorators: [
    (story, context) => (
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    ),
  ],
};

export const NoDiskSpaceError = NoDiskSpaceErrorStory;

export const SystemTimeError = {
  render: (_, props) => <SystemTimeErrorStory locale={props.locale} />,
};
