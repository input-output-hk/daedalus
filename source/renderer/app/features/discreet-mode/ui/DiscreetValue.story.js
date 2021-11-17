// @flow
import React, { useLayoutEffect } from 'react';
import { storiesOf } from '@storybook/react';
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

storiesOf('Discreet Mode|Discreet Asset Amount', module)
  .addDecorator(withKnobs)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator((story) => <StoryProvider>{story()}</StoryProvider>)
  .addDecorator((story) => (
    <DiscreetModeFeatureProvider>{story()}</DiscreetModeFeatureProvider>
  ))
  .add('Discreet mode disabled', () => (
    <>
      <DiscreetValue>123</DiscreetValue>
      <Toggle knob={boolean('Toogle discreet mode', false)} />
    </>
  ))
  .add('Discreet mode enabled', () => (
    <>
      <DiscreetValue>123</DiscreetValue>
      <Toggle knob={boolean('Toogle discreet mode', true)} />
    </>
  ));
