// @flow
import { get } from 'lodash';

export const with2Decimals = (value: number) => {
  const formattedValue = value.toString().match(/^-?\d+(?:\.\d{0,2})?/);
  const result = get(formattedValue, 0, 0);
  return result;
};
