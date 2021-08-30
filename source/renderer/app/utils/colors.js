// @flow
import chroma from 'chroma-js';
import isNil from 'lodash/isNil';

import { LIGHT_BLUE_THEME_OUTPUT } from '../themes/daedalus/light-blue';

type RangeOptions = {
  colors?: Array<any>,
  numberOfItems?: number,
  darken?: number,
  brighten?: number,
  alpha?: number,
  reverse?: boolean,
};

const defaultRangeOptions = {
  colors: ['#1ccc5d', '#aeafb2'],
  numberOfItems: 99,
  darken: 0,
  brighten: 0,
  alpha: 1,
  reverse: false,
};

export const getColorFromRange = (
  index: ?number,
  optionsOrNumberOfItems?: RangeOptions | number,
  pledgeNotMet?: boolean
) => {
  let options = {};
  let { numberOfItems } = defaultRangeOptions;

  if (pledgeNotMet)
    return LIGHT_BLUE_THEME_OUTPUT.errors['--theme-color-error'];

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

  return scale(index).darken(darken).brighten(brighten).alpha(alpha).hex();
};

export const getSaturationColor = (saturation: number): string => {
  let color;
  if (saturation > 110) {
    color = 'red';
  } else if (saturation > 105) {
    color = 'orange';
  } else if (saturation > 100) {
    color = 'yellow';
  } else {
    color = 'green';
  }
  return color;
};
