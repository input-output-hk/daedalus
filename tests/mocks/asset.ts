import BigNumber from 'bignumber.js';

import { AssetToken } from '../../source/renderer/app/api/assets/types';

export const zeroDecimalPlacesToken: AssetToken = {
    policyId: '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7',
    assetName: '',
    quantity: new BigNumber(6),
    fingerprint: 'asset1cvmyrfrc7lpht2hcjwr9lulzyyjv27uxh3kcz0',
    metadata: {
        url: 'https://developers.cardano.org/',
        name: 'ZeroDecimalCoin',
        ticker: 'ZDP',
        description: 'Zero decimal coin.'
    },
    recommendedDecimals: 0,
    uniqueId: '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7',
    decimals: undefined,
}

export const withDecimalPlacesToken: AssetToken = {
    ...zeroDecimalPlacesToken,
    metadata: {
        url: 'https://developers.cardano.org/',
        name: 'Non-zero DecimalCoin',
        ticker: 'nZDP',
        description: 'Non zero decimal coin.'
    },
    recommendedDecimals: 1,
    decimals: 2,
}