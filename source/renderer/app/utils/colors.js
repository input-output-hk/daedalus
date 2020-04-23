// @flow
import chroma from 'chroma-js';
import isNil from 'lodash/isNil';

// Ranking 001: hsla(142, 76%, 45%, 1)
// Ranking 100: hsla(15, 97%, 58%, 1)

type RangeOptions = {
  colors?: Array<any>,
  numberOfItems?: number,
  darken?: number,
  brighten?: number,
  alpha?: number,
  reverse?: boolean,
};

const defaultRangeOptions = {
  colors: ['#1cca5b', '#fc602c'],
  numberOfItems: 99,
  darken: 0,
  brighten: 0,
  alpha: 1,
  reverse: false,
};

export const getColorFromRange = (
  index: ?number,
  optionsOrNumberOfItems?: RangeOptions | number
) => {
  let options = {};
  let { numberOfItems } = defaultRangeOptions;

  if (typeof optionsOrNumberOfItems === 'object') {
    options = optionsOrNumberOfItems;
    numberOfItems = options.numberOfItems || numberOfItems;
  } else if (typeof optionsOrNumberOfItems === 'number') {
    numberOfItems = optionsOrNumberOfItems;
  }

  const { colors, darken, brighten, alpha, reverse } = {
    ...defaultRangeOptions,
    ...options,
  };
  const domain = [0, numberOfItems];
  if (reverse) domain.reverse();
  const scale = chroma.scale(colors).domain(domain);

  if (isNil(index)) {
    return 'transparent';
  }

  return scale(index)
    .darken(darken)
    .brighten(brighten)
    .alpha(alpha)
    .hex();
};

export const getSaturationColor = (saturation: number): string => {
  let color;
  if (saturation >= 100) {
    color = 'red';
  } else if (saturation >= 90) {
    color = 'orange';
  } else if (saturation >= 80) {
    color = 'yellow';
  } else {
    color = 'green';
  }
  return color;
};
