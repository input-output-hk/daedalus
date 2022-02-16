import { State } from './state';

export function isDiscreetMode(feature: State) {
  return feature.state === 'LOADED' && feature.discreetMode.state === 'ON';
}

export function isOpeningInDiscreetMode(feature: State) {
  return feature.state === 'LOADED' && feature.isOpeningInDiscreetMode;
}
