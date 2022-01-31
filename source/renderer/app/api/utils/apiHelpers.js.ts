import { ApiMethodNotYetImplementedError } from "../common/errors";
import ApiError from "../../domains/ApiError";

// @ts-ignore ts-migrate(2583) FIXME: Cannot find name 'Promise'. Do you need to change ... Remove this comment to see the full error message
export const notYetImplemented = async () => new Promise((resolve, reject) => {
  reject(new ApiMethodNotYetImplementedError());
});
// helper code for testing async APIs
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'Array'.
export const testAsync = async (apiMethod: (...args: Array<any>) => any) => {
  const result = await apiMethod();
  // eslint-disable-next-line no-console
  console.log(`testAsync result: ${result}`);
  return result;
};
// helper code for testing sync APIs
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'Array'.
export const testSync = (apiMethod: (...args: Array<any>) => any) => {
  const result = apiMethod();
  // eslint-disable-next-line no-console
  console.log(`testSync result: ${result}`);
  return result;
};
// helper code for deferring API call execution
// @ts-ignore ts-migrate(2583) FIXME: Cannot find name 'Promise'. Do you need to change ... Remove this comment to see the full error message
export const wait = (ms: number): Promise<void> => new Promise(resolve => setTimeout(resolve, ms));
export const throwErrorIfNotEnoughAdaToSupportTokens = (error: any, hasAssetsRemainingAfterTransaction?: boolean) => {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'RegExp'.
  const adaToProceedRegex = new RegExp(/.*I need approximately([\s\d.,]+)ada to proceed.*/);

  if (error.code === 'cannot_cover_fee' && hasAssetsRemainingAfterTransaction && adaToProceedRegex.test(error.message)) {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'Math'.
    const adaToRemain = Math.ceil(Number(error.message.replace(adaToProceedRegex, '$1')));
    throw new ApiError().set('cannotLeaveWalletEmpty', true, {
      adaToRemain
    }).result();
  }
};