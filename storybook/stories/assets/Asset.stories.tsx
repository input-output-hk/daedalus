import React from 'react';
import { storiesOf } from '@storybook/react';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, number, text } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import Asset from '../../../source/renderer/app/components/assets/Asset';

storiesOf('Assets|Asset pill', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs) // ====== Stories ======
  .add('Default', () => {
    const policyId = text(
      'policyId',
      '6e8dc8b1f3591e8febcc47c51e9f2667c413a497aebd54cf38979086'
    );
    const assetName = text('assetName', '6861707079636f696e');
    return (
      <div
        style={{
          padding: '30px',
        }}
      >
        <Asset
          asset={{
            policyId,
            assetName,
            uniqueId: `${policyId}${assetName}`,
            fingerprint: text(
              'fingerprint',
              'asset18v86ulgre52g4l7lvl5shl8h5cm4u3dmrjg2e8'
            ),
            quantity: new BigNumber(number('quantity', 1)),
            decimals: 0,
            recommendedDecimals: null,
            metadata: {
              // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
              name: text('name'),
              // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
              ticker: text('ticker'),
              // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
              description: text('description'),
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              unit: {
                // @ts-ignore ts-migrate(2554) FIXME: Expected 2-4 arguments, but got 1.
                decimals: number('unit / decimals'),
                // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
                name: text('unit / name'),
              },
            },
          }}
          small={boolean('small', false)}
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
          hidePopOver={boolean('hidePopOver')}
          onCopyAssetParam={action('onCopyAssetParam')}
          onClickSettings={
            boolean('Is configurable', true) ? action('onClickSettings') : null
          }
        />
      </div>
    );
  });
