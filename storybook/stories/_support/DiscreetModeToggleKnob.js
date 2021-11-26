// @flow

import { useLayoutEffect } from 'react';
import { observer } from 'mobx-react';
import { boolean } from '@storybook/addon-knobs';
import { useDiscreetModeFeature } from '../../../source/renderer/app/features';

export const DiscreetModeToggleKnob = observer(() => {
  const feature = useDiscreetModeFeature();
  const knob = boolean('Toogle discreet mode', false);

  useLayoutEffect(() => {
    if (knob !== feature.isDiscreetMode) {
      feature.toggleDiscreetMode();
    }
  }, [knob, feature.isDiscreetMode]);

  return null;
});
