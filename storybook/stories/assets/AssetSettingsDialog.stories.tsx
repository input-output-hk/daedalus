import React from 'react';
import BigNumber from 'bignumber.js';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, number } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import AssetSettingsDialog from '../../../source/renderer/app/components/assets/AssetSettingsDialog';

const asset = {
  policyId: '6e8dc8b1f3591e8febcc47c51e9f2667c413a497aebd54cf38979086',
  assetName: '6861707079636f696e',
  uniqueId:
    '6e8dc8b1f3591e8febcc47c51e9f2667c413a497aebd54cf389790866861707079636f696e',
  fingerprint: 'asset18v86ulgre52g4l7lvl5shl8h5cm4u3dmrjg2e8',
  quantity: new BigNumber(number('quantity', 1)),
  decimals: 0,
  recommendedDecimals: null,
  metadata: null,
};
storiesOf('Assets|AssetSettingsDialog', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs) // ====== Stories ======
  .add('Default', () => (
    <AssetSettingsDialog
      asset={asset}
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ asset: { policyId: string; assetName: stri... Remove this comment to see the full error message
      assetAmount={new BigNumber(number('assetAmount', 500))}
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
    />
  ))
  .add('With recommended decimal precision', () => (
    <AssetSettingsDialog
      asset={{
        ...asset,
        recommendedDecimals: number('recommendedDecimals', 2),
      }}
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ asset: { recommendedDecimals: number; poli... Remove this comment to see the full error message
      assetAmount={new BigNumber(number('assetAmount', 500))}
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
      recommendedDecimalPrecision={2}
    />
  ));
