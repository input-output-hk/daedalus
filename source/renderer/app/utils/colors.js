// @flow
import chroma from 'chroma-js';

// Ranking 001: hsla(142, 76%, 45%, 1)
// Ranking 100: hsla(15, 97%, 58%, 1)

type RangeOptions = {
  colors?: Array<any>,
  domain?: Array<number>,
  darken?: number,
  brighten?: number,
  alpha?: number,
  reverse?: boolean,
};

const defaultRangeOptions = {
  colors: ['#1cca5b', '#fc602c'],
  domain: [0, 99],
  darken: 0,
  brighten: 0,
  alpha: 1,
  reverse: false,
};

export const getColorFromRange = (index: number, options?: RangeOptions) => {
  const { colors, domain: originalDomain, darken, brighten, alpha, reverse } = {
    ...defaultRangeOptions,
    ...options,
  };
  const domain = originalDomain.slice();
  if (reverse) domain.reverse();
  const scale = chroma.scale(colors).domain(domain);
  return scale(index)
    .darken(darken)
    .brighten(brighten)
    .alpha(alpha)
    .hex();
};
