'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.randomInRange = exports.closestNumber = exports.rangeMap = void 0;
const rangeMap = (n, start1, stop1, start2, stop2) => {
  return ((n - start1) / (stop1 - start1)) * (stop2 - start2) + start2;
};
exports.rangeMap = rangeMap;
const closestNumber = (number, numbers) =>
  numbers.sort((a, b) => a - b).find((item) => item > number);
exports.closestNumber = closestNumber;
const randomInRange = (min, max) => Math.random() * (max - min) + min;
exports.randomInRange = randomInRange;
//# sourceMappingURL=numbers.js.map
