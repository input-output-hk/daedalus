// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { text, boolean, number } from '@storybook/addon-knobs';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { generateAddress } from '../../_support/utils';

// Screens
import WalletReceive from '../../../../source/renderer/app/components/wallet/receive/WalletReceive';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';

storiesOf('Wallets|Actions', module)
  .addDecorator(WalletsWrapper)
  .add('Receive', () => (
    <VerticalFlexContainer>
      <WalletReceive
        walletAddress={text(
          'Wallet address',
          'DdzFFzCqrhsg9ngNRhHEa49se7qEMKyubT9tcE13Fkvh8QC82trpTDsNvdQV7mg9SCZiuENkf77zrtwPXrTyGMNznUsSinPC1gb2ZCqK'
        )}
        isWalletAddressUsed={boolean('isWalletAddressUsed', false)}
        walletAddresses={[
          ...Array.from(Array(number('Addresses', 10))).map(() =>
            generateAddress()
          ),
          ...Array.from(Array(number('Addresses (used)', 10))).map(() =>
            generateAddress(true)
          ),
        ]}
        onGenerateAddress={action('onGenerateAddress')}
        onCopyAddress={action('onGenerateAddress')}
        isSidebarExpanded={boolean('isSidebarExpanded', true)}
        walletHasPassword={boolean('walletHasPassword', false)}
        isSubmitting={boolean('isSubmitting', false)}
      />
    </VerticalFlexContainer>
  ));
