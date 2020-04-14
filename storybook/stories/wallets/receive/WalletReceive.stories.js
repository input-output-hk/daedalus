// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { number, boolean } from '@storybook/addon-knobs';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { generateAddress } from '../../_support/utils';

// Screens
import WalletReceiveSequential from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveSequential';
import WalletReceiveRandom from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveRandom';
import WalletReceiveDialog from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveDialog';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';

const onToggleSubMenus = {
  listen: action('onToggleSubMenus:listen'),
  remove: action('onToggleSubMenus:remove'),
};

storiesOf('Wallets|Receive', module)
  .addDecorator(WalletsWrapper)
  .add('Receive - random', () => {
    const isSidebarExpanded = boolean('isSidebarExpanded', false);
    const walletHasPassword = boolean('walletHasPassword', false);
    const isSubmitting = boolean('isSubmitting', false);

    const walletAddress = generateAddress();
    return (
      <VerticalFlexContainer>
        <WalletReceiveRandom
          walletAddress={walletAddress.id}
          isWalletAddressUsed={walletAddress.used}
          walletAddresses={[
            ...Array.from(Array(number('Addresses', 5))).map(() =>
              generateAddress()
            ),
            ...Array.from(Array(number('Addresses (used)', 5))).map(() =>
              generateAddress(true)
            ),
          ]}
          onGenerateAddress={action('onGenerateAddress')}
          onCopyAddress={action('onCopyAddress')}
          onShareAddress={action('onShareAddress')}
          isSidebarExpanded={isSidebarExpanded}
          walletHasPassword={walletHasPassword}
          isSubmitting={isSubmitting}
        />
      </VerticalFlexContainer>
    );
  })
  .add('Receive - sequential', ({ locale }: { locale: string }) => {
    const isIncentivizedTestnet = boolean('isIncentivizedTestnet', true);
    const showDialog = boolean('showDialog', false);

    return (
      <VerticalFlexContainer>
        <WalletReceiveSequential
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
          currentLocale={locale}
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
