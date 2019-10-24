// @flow
export type NumberFormat = {
  groupSize: number,
  groupSeparator: '.' | ',' | ' ',
  decimalSeparator: '.' | ',' | ' ',
};

type NumbersFormat = {
  [key: string]: NumberFormat,
};

export const NUMBER_FORMATS: NumbersFormat = {
  'number-1': {
    groupSize: 3,
    groupSeparator: ',',
    decimalSeparator: '.',
  },
  'number-2': {
    groupSize: 3,
    groupSeparator: '.',
    decimalSeparator: ',',
  },
  'number-3': {
    groupSize: 3,
    groupSeparator: ' ',
    decimalSeparator: '.',
  },
};
