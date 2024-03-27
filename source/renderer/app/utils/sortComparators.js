'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.dateComparator = exports.stringComparator = exports.bigNumberComparator = void 0;
const moment_1 = __importDefault(require('moment'));
const bigNumberComparator = (numberA, numberB, isAscending = true) => {
  if (numberA.isLessThan(numberB)) {
    return isAscending ? -1 : 1;
  }
  if (numberA.isGreaterThan(numberB)) {
    return isAscending ? 1 : -1;
  }
  return 0;
};
exports.bigNumberComparator = bigNumberComparator;
const stringComparator = (stringA, stringB, isAscending = true) => {
  if (stringA < stringB) {
    return isAscending ? -1 : 1;
  }
  if (stringA > stringB) {
    return isAscending ? 1 : -1;
  }
  return 0;
};
exports.stringComparator = stringComparator;
const dateComparator = (dateA, dateB, isAscending = true) => {
  if (
    (0, moment_1.default)(dateA).unix() < (0, moment_1.default)(dateB).unix()
  ) {
    return isAscending ? -1 : 1;
  }
  if (
    (0, moment_1.default)(dateA).unix() > (0, moment_1.default)(dateB).unix()
  ) {
    return isAscending ? 1 : -1;
  }
  return 0;
};
exports.dateComparator = dateComparator;
//# sourceMappingURL=sortComparators.js.map
