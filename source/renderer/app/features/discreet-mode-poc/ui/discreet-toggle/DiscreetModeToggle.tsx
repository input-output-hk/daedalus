import React from 'react';
import { DiscreetMode } from '../../feature';
import { isDiscreetMode } from '../../selectors';
import { DiscreetToggle } from './DiscreetToggle';

export const DiscreetModeToggle = ({ className }: { className: string }) => {
  const dispatch = DiscreetMode.useUpdate();
  return (
    <DiscreetToggle
      className={className}
      isDiscreetMode={DiscreetMode.useSelector(isDiscreetMode)}
      onToggle={() => dispatch({ type: 'TOGGLE_DISCREET_MODE' })}
    />
  );
};
