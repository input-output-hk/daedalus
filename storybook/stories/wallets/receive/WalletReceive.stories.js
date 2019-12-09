// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { number, boolean } from '@storybook/addon-knobs';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { generateAddress } from '../../_support/utils';

// Screens
import WalletReceive from '../../../../source/renderer/app/components/wallet/receive/WalletReceive';
import WalletReceiveDialog from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveDialog';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';

const onToggleSubMenus = {
  listen: action('onToggleSubMenus:listen'),
  remove: action('onToggleSubMenus:remove'),
};

storiesOf('Wallets|Receive', module)
  .addDecorator(WalletsWrapper)
  .add('Receive', ({ currentLocale }: { currentLocale: string }) => {
    const isIncentivizedTestnet = boolean('isIncentivizedTestnet', false);
    const showDialog = boolean('showDialog', false);
    return (
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
          onShareAddress={action('onShareAddress')}
          onCopyAddress={action('onCopyAddress')}
          isAddressValid={() => parseInt(Math.random() * 10, 10) > 3}
          isIncentivizedTestnet={isIncentivizedTestnet}
          currentLocale={currentLocale}
          onToggleSubMenus={onToggleSubMenus}
          isShowingSubMenus
        />
        {showDialog && !isIncentivizedTestnet && (
          <WalletReceiveDialog
            address={generateAddress()}
            onCopyAddress={action('onCopyAddress')}
            onDownloadPDF={action('onDownloadPDF')}
            onClose={action('onClose')}
          />
        )}
      </VerticalFlexContainer>
    );
  });
