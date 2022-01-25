// @flow
import BigNumber from 'bignumber.js';
import moment from 'moment';

export const bigNumberComparator = (
  numberA: BigNumber,
  numberB: BigNumber,
  isAscending: boolean = true
): number => {
  if (numberA.isLessThan(numberB)) {
    return isAscending ? -1 : 1;
  }

  if (numberA.isGreaterThan(numberB)) {
    return isAscending ? 1 : -1;
  }

  return 0;
};

export const stringComparator = (
  stringA: string,
  stringB: string,
  isAscending: boolean = true
): number => {
  if (`${stringA}`.toLowerCase() < `${stringB}`.toLowerCase()) {
    return isAscending ? -1 : 1;
  }

  if (`${stringA}`.toLowerCase() > `${stringB}`.toLowerCase()) {
    return isAscending ? 1 : -1;
  }

  return 0;
};

export const numberComparator = (
  numberA: number,
  numberB: number,
  isAscending: boolean = true
): number => {
  if (numberA < numberB) {
    return isAscending ? -1 : 1;
  }

  if (numberA > numberB) {
    return isAscending ? 1 : -1;
  }

  return 0;
};

export const dateComparator = (
  dateA: string,
  dateB: string,
  isAscending: boolean = true
): number => {
  if (moment(dateA).unix() < moment(dateB).unix()) {
    return isAscending ? -1 : 1;
  }

  if (moment(dateA).unix() > moment(dateB).unix()) {
    return isAscending ? 1 : -1;
  }

  return 0;
};
