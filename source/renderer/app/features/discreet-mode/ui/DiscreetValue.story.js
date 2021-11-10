// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import {
  DiscreetModeFeatureProvider,
  useDiscreetModeFeature,
} from '../context';
import DiscreetValue from './DiscreetValue';

function Toggle({ knob }: { knob: boolean }) {
  const feature = useDiscreetModeFeature();

  if (feature.isDiscreetMode !== knob) {
    feature.toggleDiscreetMode();
  }

  return null;
}

storiesOf('Discreet Mode|Discreet Value', module)
  .addDecorator(withKnobs)
  .add('Main', () => (
    <DiscreetModeFeatureProvider>
      <>
        <DiscreetValue>123</DiscreetValue>
        <Toggle knob={boolean('Toggle', false)} />
      </>
    </DiscreetModeFeatureProvider>
  ));
