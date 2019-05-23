// @flow
import { rangeMap } from './rangeMap';

// Ranking 001: hsla(142, 76%, 45%, 1)
// Ranking 100: hsla(15, 97%, 58%, 1)

const RANKING_COLORS_RANGE = {
  hue: [142, 15],
  saturation: [76, 97],
  lighness: [45, 58],
};

const getHSLParam = (ranking: number, param: string) =>
  rangeMap(
    ranking,
    1,
    100,
    RANKING_COLORS_RANGE[param][0],
    RANKING_COLORS_RANGE[param][1]
  );

export const getHSLColor = (ranking: number) =>
  `hsl(${getHSLParam(ranking, 'hue')}, ${getHSLParam(
    ranking,
    'saturation'
  )}%, ${getHSLParam(ranking, 'lighness')}%)`;
