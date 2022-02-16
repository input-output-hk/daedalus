import React from 'react';
import { DiscreetMode } from '../../feature';
import NormalSwitch from '../../../../components/widgets/forms/NormalSwitch';
import { isDiscreetMode } from '../../selectors';

export function DiscreetModeToggle() {
  const dispatch = DiscreetMode.useUpdate();
  return (
    <NormalSwitch
      checked={DiscreetMode.useSelector(isDiscreetMode)}
      onChange={() => dispatch({ type: 'TOGGLE_DISCREET_MODE' })}
    />
  );
}
