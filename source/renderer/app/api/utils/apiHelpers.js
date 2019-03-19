// @flow
import { ApiMethodNotYetImplementedError } from '../common/errors';

export const notYetImplemented = async () =>
  new Promise((resolve, reject) => {
    reject(new ApiMethodNotYetImplementedError());
  });

// helper code for testing async APIs
export const testAsync = async (apiMethod: Function) => {
  const result = await apiMethod();
  // eslint-disable-next-line
  console.log(`testAsync result: ${result}`);
  return result;
};

// helper code for testing sync APIs
export const testSync = (apiMethod: Function) => {
  const result = apiMethod();
  // eslint-disable-next-line
  console.log(`testSync result: ${result}`);
  return result;
};
