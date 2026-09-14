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
// A display helper with no callers. `Math.random` is not a CSPRNG: never use
// this, or anything built on it, for key material, entropy, or anything a
// wallet's security depends on. `secureRandomBytes` in `./entropy` is the
// entropy source.
export const randomInRange = (min: number, max: number) =>
  // eslint-disable-next-line no-restricted-properties
  Math.random() * (max - min) + min;
