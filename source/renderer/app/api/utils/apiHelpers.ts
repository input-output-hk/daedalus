import { ApiMethodNotYetImplementedError } from '../common/errors';

export const notYetImplemented = async () =>
  new Promise((resolve, reject) => {
    reject(new ApiMethodNotYetImplementedError());
  });
// helper code for testing async APIs
export const testAsync = async (apiMethod: (...args: Array<any>) => any) => {
  const result = await apiMethod();
  // eslint-disable-next-line no-console
  console.log(`testAsync result: ${result}`);
  return result;
};
// helper code for testing sync APIs
export const testSync = (apiMethod: (...args: Array<any>) => any) => {
  const result = apiMethod();
  // eslint-disable-next-line no-console
  console.log(`testSync result: ${result}`);
  return result;
};
// helper code for deferring API call execution
export const wait = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));
