// @flow
import { rangeMap } from './rangeMap';

// Ranking 001: hsla(142, 76%, 45%, 1)
// Ranking 100: hsla(15, 97%, 58%, 1)

const RANKING_COLORS_RANGE = {
  hue: [142, 15],
  saturation: [76, 97],
  lighness: [45, 58],
};

const getHSLParam = (ranking: number, param: string, offset?: number) =>
  rangeMap(
    ranking,
    1,
    100,
    RANKING_COLORS_RANGE[param][0],
    RANKING_COLORS_RANGE[param][1]
  ) + offset;

type Options = {
  hueOffset?: number,
  saturationOffset?: number,
  lighnessOffset?: number,
};

const defaultOptions = {
  hueOffset: 0,
  saturationOffset: 0,
  lighnessOffset: 0,
};

export const getHSLColor = (ranking: number, options?: Options = {}) => {
  const { hueOffset, saturationOffset, lighnessOffset } = {
    ...defaultOptions,
    ...options,
  };
  const hue = getHSLParam(ranking, 'hue', hueOffset);
  const saturation = getHSLParam(ranking, 'saturation', saturationOffset);
  const lighness = getHSLParam(ranking, 'lighness', lighnessOffset);
  return `hsl(${hue}, ${saturation}%, ${lighness}%)`;
};
