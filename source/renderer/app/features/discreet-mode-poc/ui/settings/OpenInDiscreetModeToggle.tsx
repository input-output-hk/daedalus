import React from 'react';
import { DiscreetMode } from '../../feature';
import NormalSwitch from '../../../../components/widgets/forms/NormalSwitch';
import { isOpeningInDiscreetMode } from '../../selectors';

export function OpenInDiscreetModeToggle() {
  const dispatch = DiscreetMode.useUpdate();
  return (
    <NormalSwitch
      checked={DiscreetMode.useSelector(isOpeningInDiscreetMode)}
      onChange={() => dispatch({ type: 'TOGGLE_OPEN_IN_DISCREET_MODE' })}
    />
  );
}
