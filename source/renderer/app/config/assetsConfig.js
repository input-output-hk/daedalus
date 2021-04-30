// @flow
import assetsPredefinedDecimals from './assetsPredefinedDecimals.json';
import assetsPredefinedDecimalsTestnet from './assetsPredefinedDecimals-testnet.json';

const { isTestnet } = global.environment;

export const MAX_DECIMAL_PRECISION = 20;
export const DEFAULT_DECIMAL_PRECISION = 0;

/**
 *
 * Assets with predefined decimal precision
 * "[key: policyId + assetName]": number,
 *
 */
export const ASSETS_PREDEFINED_DECIMALS = isTestnet
  ? assetsPredefinedDecimalsTestnet
  : assetsPredefinedDecimals;
