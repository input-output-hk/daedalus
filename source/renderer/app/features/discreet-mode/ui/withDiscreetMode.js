// @flow
import React from 'react';
import type { AbstractComponent } from 'react';
import { observer } from 'mobx-react';
import { useDiscreetModeFeature } from '../context';
import { DiscreetMode } from '../feature';

function getDisplayName(WrappedComponent) {
  return WrappedComponent.displayName || WrappedComponent.name || 'Component';
}

type InjectedProps = {| discreetModeFeature: DiscreetMode |};

export function withDiscreetMode<Config>(
  WrappedComponent: AbstractComponent<{| ...Config, ...InjectedProps |}>
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
