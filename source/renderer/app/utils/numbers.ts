export const rangeMap = (
  n: number,
  start1: number,
  stop1: number,
  start2: number,
  stop2: number
): number => {
  return ((n - start1) / (stop1 - start1)) * (stop2 - start2) + start2;
};
export const closestNumber = (number: number, numbers: Array<number>) =>
  numbers.sort((a, b) => a - b).find((item) => item > number);
export const randomInRange = (min: number, max: number) =>
  Math.random() * (max - min) + min;
