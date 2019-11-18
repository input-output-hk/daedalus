// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { number } from '@storybook/addon-knobs';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { generateAddress } from '../../_support/utils';

// Screens
import WalletReceive from '../../../../source/renderer/app/components/wallet/receive/WalletReceive';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';

storiesOf('Wallets|Receive', module)
  .addDecorator(WalletsWrapper)
  .add('Receive', () => (
    <VerticalFlexContainer>
      <WalletReceive
        walletAddresses={[
          ...Array.from(Array(number('Addresses', 10))).map(() =>
            generateAddress()
          ),
          ...Array.from(Array(number('Addresses (used)', 10))).map(() =>
            generateAddress(true)
          ),
        ]}
        onShareAddress={action('onGenerateAddress')}
        isAddressValid={() => parseInt(Math.random() * 10, 10) > 3}
      />
    </VerticalFlexContainer>
  ));
