import React, { useLayoutEffect } from 'react';
import { observer } from 'mobx-react';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import StoryProvider from '../../../../../../storybook/stories/_support/StoryProvider';
import {
  DiscreetModeFeatureProvider,
  useDiscreetModeFeature,
} from '../context';
import DiscreetValue from './DiscreetValue';

const Toggle = observer(({ knob }: { knob: boolean }) => {
  const feature = useDiscreetModeFeature();
  useLayoutEffect(() => {
    if (knob !== feature.isDiscreetMode) {
      feature.toggleDiscreetMode();
    }
  }, [knob, feature.isDiscreetMode]);
  return null;
});

export default {
  title: 'Discreet Mode / Discreet Asset Amount',

  decorators: [
    withKnobs,
    (story) => (
      <StoryDecorator>
        <StoryProvider>
          <DiscreetModeFeatureProvider>{story()}</DiscreetModeFeatureProvider>
        </StoryProvider>
      </StoryDecorator>
    ),
  ],
};

export const DiscreetModeDisabled = {
  render: () => (
    <>
      {/* @ts-ignore ts-migrate(2741) FIXME: Property 'replacer' is missing in type '{ children... Remove this comment to see the full error message */}
      <DiscreetValue>123</DiscreetValue>
      <Toggle knob={boolean('Toggle discreet mode', false)} />
    </>
  ),

  name: 'Discreet mode disabled',
};

export const DiscreetModeEnabled = {
  render: () => (
    <>
      {/* @ts-ignore ts-migrate(2741) FIXME: Property 'replacer' is missing in type '{ children... Remove this comment to see the full error message */}
      <DiscreetValue>123</DiscreetValue>
      <Toggle knob={boolean('Toggle discreet mode', true)} />
    </>
  ),

  name: 'Discreet mode enabled',
};
