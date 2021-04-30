// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, number, text } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import AssetToken from '../../../source/renderer/app/components/assets/AssetToken';

storiesOf('Assets|AssetToken', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Default', () => (
    <div style={{ padding: '30px' }}>
      <AssetToken
        asset={{
          policyId: text(
            'policyId',
            '6e8dc8b1f3591e8febcc47c51e9f2667c413a497aebd54cf38979086'
          ),
          assetName: text('assetName', '6861707079636f696e'),
          fingerprint: text(
            'fingerprint',
            'asset18v86ulgre52g4l7lvl5shl8h5cm4u3dmrjg2e8'
          ),
          quantity: new BigNumber(number('quantity', 1)),
          decimals: 0,
          recommendedDecimals: null,
          metadata: {
            name: text('name'),
            ticker: text('ticker'),
            description: text('description'),
            unit: {
              decimals: number('unit / decimals'),
              name: text('unit / name'),
            },
          },
        }}
        small={boolean('small', false)}
        hidePopOver={boolean('hidePopOver')}
        onCopyAssetItem={action('onCopyAssetItem')}
        onClickSettings={
          boolean('Is configurable', true) ? action('onClickSettings') : null
        }
      />
    </div>
  ));
