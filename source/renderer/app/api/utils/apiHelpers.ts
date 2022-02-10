import { ApiMethodNotYetImplementedError } from '../common/errors';
import ApiError from '../../domains/ApiError';

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
export const throwErrorIfNotEnoughAdaToSupportTokens = (
  error: any,
  hasAssetsRemainingAfterTransaction?: boolean
) => {
  const adaToProceedRegex = new RegExp(
    /.*I need approximately([\s\d.,]+)ada to proceed.*/
  );

  if (
    error.code === 'cannot_cover_fee' &&
    hasAssetsRemainingAfterTransaction &&
    adaToProceedRegex.test(error.message)
  ) {
    const roundedAda = Math.ceil(
      Number(error.message.replace(adaToProceedRegex, '$1'))
    );
    const adaToRemain = roundedAda > 2 ? roundedAda : 2;
    throw new ApiError()
      .set('cannotLeaveWalletEmpty', true, {
        adaToRemain,
      })
      .result();
  }
};
