import BigNumber from 'bignumber.js';
import moment from 'moment';

export const bigNumberComparator = (
  numberA: BigNumber,
  numberB: BigNumber,
  isAscending = true
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
  isAscending = true
): number => {
  if (stringA < stringB) {
    return isAscending ? -1 : 1;
  }

  if (stringA > stringB) {
    return isAscending ? 1 : -1;
  }

  return 0;
};
export const dateComparator = (
  dateA: string,
  dateB: string,
  isAscending = true
): number => {
  if (moment(dateA).unix() < moment(dateB).unix()) {
    return isAscending ? -1 : 1;
  }

  if (moment(dateA).unix() > moment(dateB).unix()) {
    return isAscending ? 1 : -1;
  }

  return 0;
};
