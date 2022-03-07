// @flow
export type NumberFormat = {
  groupSeparator: '.' | ',' | ' ',
  decimalSeparator: '.' | ',' | ' ',
};

type NumbersFormat = {
  [key: string]: NumberFormat,
};

export const NUMBER_FORMATS: NumbersFormat = {
  'number-1': {
    groupSeparator: ',',
    decimalSeparator: '.',
  },
  'number-2': {
    groupSeparator: '.',
    decimalSeparator: ',',
  },
  'number-3': {
    groupSeparator: ' ',
    decimalSeparator: '.',
  },
};

export const DEFAULT_NUMBER_FORMAT: Object = {
  decimalSeparator: '.',
  groupSeparator: ',',
  groupSize: 3,
  secondaryGroupSize: 0,
  fractionGroupSeparator: ' ',
  fractionGroupSize: 0,
};
