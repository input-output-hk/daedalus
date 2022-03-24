import React from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'AbstractC... Remove this comment to see the full error message
import type { AbstractComponent } from 'react';
import { observer } from 'mobx-react';
import { useDiscreetModeFeature } from '../context';
import { DiscreetMode } from '../feature';

function getDisplayName(WrappedComponent) {
  return WrappedComponent.displayName || WrappedComponent.name || 'Component';
}

type InjectedProps = {
  discreetModeFeature: DiscreetMode;
};
export function withDiscreetMode<Config>(
  WrappedComponent: AbstractComponent<Config & InjectedProps>
): AbstractComponent<Config> {
  function WithDiscreetMode(props: Config) {
    const feature = useDiscreetModeFeature();
    return <WrappedComponent {...props} discreetModeFeature={feature} />;
  }

  WithDiscreetMode.displayName = `WithDiscreetMode(${getDisplayName(
    WrappedComponent
  )})`;
  return observer(WithDiscreetMode);
}
