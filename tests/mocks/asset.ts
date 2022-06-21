import BigNumber from 'bignumber.js';

import { AssetToken, Token } from '../../source/renderer/app/api/assets/types';

export const commonTokenProperties: Token = {
  policyId: '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7',
  assetName: '',
  quantity: new BigNumber(6),
  uniqueId: '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7',
};

export const zeroDecimalPlacesToken: AssetToken = {
  ...commonTokenProperties,
  fingerprint: 'asset1cvmyrfrc7lpht2hcjwr9lulzyyjv27uxh3kcz0',
  metadata: {
    url: 'https://developers.cardano.org/',
    name: 'ZeroDecimalCoin',
    ticker: 'ZDP',
    description: 'Zero decimal coin.',
  },
  recommendedDecimals: 0,
  decimals: undefined,
};

export const withDecimalPlacesToken: AssetToken = {
  ...commonTokenProperties,
  fingerprint: 'asset1cvmyrfrc7lpht2hcjwr9lulzyyjv27uxh3kcz1',
  metadata: {
    url: 'https://developers.cardano.org/',
    name: 'Non-zero DecimalCoin',
    ticker: 'nZDP',
    description: 'Non zero decimal coin.',
  },
  recommendedDecimals: 1,
  decimals: 2,
};
