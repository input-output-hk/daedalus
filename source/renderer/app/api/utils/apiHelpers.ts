import { ApiMethodNotYetImplementedError } from '../common/errors';
import { ErrorType } from '../../domains/ApiError';

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
export const doesWalletRequireAdaToRemainToSupportTokens = (
  error: ErrorType,
  hasAssetsRemainingAfterTransaction?: boolean
): {
  requiresAdaToRemainToSupportNativeTokens: boolean;
  adaToProceed?: number;
} => {
  const adaToProceedRegex = /I need approximately\s+([\d.,]+)\s+ada to proceed/;

  const [, adaToProceed] = adaToProceedRegex.exec(error.message) ?? [];

  if (
    error.code === 'cannot_cover_fee' &&
    hasAssetsRemainingAfterTransaction &&
    adaToProceed
  ) {
    return {
      requiresAdaToRemainToSupportNativeTokens: true,
      adaToProceed: Math.ceil(Number(adaToProceed)),
    };
  }
  return { requiresAdaToRemainToSupportNativeTokens: false };
};
