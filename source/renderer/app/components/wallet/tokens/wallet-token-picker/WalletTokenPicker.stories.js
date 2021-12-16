// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../../../../../../../storybook/stories/_support/StoryDecorator';
import StoryProvider from '../../../../../../../storybook/stories/_support/StoryProvider';
import { generateHash } from '../../../../../../../storybook/stories/_support/utils';
import WalletTokenPicker from './WalletTokenPicker';

const assets = [
  {
    policyId: generateHash(),
    assetName: '546f6b656e31',
    assetNameASCII: 'Token1',
    quantity: new BigNumber(10),
    fingerprint: generateHash(),
    recommendedDecimals: null,
    uniqueId: generateHash(),
    metadata: {
      name: 'MakerDAO',
      ticker: 'DAI',
      description: 'Test description',
      unit: {
        name: 'DAI',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    },
  },
  {
    policyId: generateHash(),
    assetName: '546f6b656e32',
    assetNameASCII: 'Token2',
    quantity: new BigNumber(20),
    fingerprint: generateHash(),
    recommendedDecimals: null,
    uniqueId: generateHash(),
  },
  {
    policyId: generateHash(),
    assetName: '546f6b656e33',
    assetNameASCII: 'Token3',
    quantity: new BigNumber(30),
    fingerprint: generateHash(),
    recommendedDecimals: null,
    uniqueId: generateHash(),
  },
];

storiesOf('Wallets|Tokens', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)
  .add('WalletTokenPicker', () => (
    <WalletTokenPicker
      assets={assets}
      tokenFavorites={{ [assets[0].uniqueId]: true }}
    />
  ));
