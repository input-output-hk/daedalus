import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { number, boolean, select } from '@storybook/addon-knobs';
// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { generateAddress } from '../../_support/utils';
// Screens
import WalletReceiveSequential from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveSequential';
import WalletReceiveRandom from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveRandom';
import WalletReceiveDialog from '../../../../source/renderer/app/components/wallet/receive/WalletReceiveDialog';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';

const onToggleSubMenus = {
  listen: action('onToggleSubMenus:listen'),
  remove: action('onToggleSubMenus:remove'),
};
storiesOf('Wallets|Receive', module)
  .addDecorator(WalletsWrapper)
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('Receive - sequential', ({ locale }: { locale: string }) => {
    const showDialog = boolean('showDialog', false);
    return (
      <VerticalFlexContainer>
        <WalletReceiveSequential
          walletAddresses={[
            ...Array.from(Array(number('Addresses (used)', 2))).map(() =>
              generateAddress(true)
            ),
            ...Array.from(Array(number('Addresses', 10))).map(() =>
              generateAddress()
            ),
          ]}
          onShareAddress={action('onShareAddress')}
          onCopyAddress={action('onCopyAddress')}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          isAddressValid={() => parseInt(Math.random() * 10, 10) > 3}
          currentLocale={locale}
          showUsed={boolean('showUsed', false)}
          onToggleUsedAddresses={action('onToggleUsedAddresses')}
          onToggleSubMenus={onToggleSubMenus}
          isShowingSubMenus
        />
        {showDialog && (
          <WalletReceiveDialog
            address={generateAddress()}
            onCopyAddress={action('onCopyAddress')}
            onDownloadPDF={action('onDownloadPDF')}
            onSaveQRCodeImage={action('onSaveQRCodeImage')}
            onClose={action('onClose')}
            hwDeviceStatus={HwDeviceStatuses.CONNECTING}
            isHardwareWallet={false}
            walletName="Wallet 1"
            isAddressDerived={false}
            isAddressChecked={false}
            onChangeVerificationStatus={action('onChangeVerificationStatus')}
            onSupportRequestClick={action('onSupportRequestClick')}
            isTrezor={boolean('isTrezor', false)}
          />
        )}
      </VerticalFlexContainer>
    );
  })
  .add(
    'Receive - sequential with address verification',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
    ({ locale }: { locale: string }) => {
      return (
        <VerticalFlexContainer>
          <WalletReceiveSequential
            walletAddresses={[
              ...Array.from(Array(number('Addresses (used)', 2))).map(() =>
                generateAddress(true)
              ),
              ...Array.from(Array(number('Addresses', 10))).map(() =>
                generateAddress()
              ),
            ]}
            onShareAddress={action('onShareAddress')}
            onCopyAddress={action('onCopyAddress')}
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            isAddressValid={() => parseInt(Math.random() * 10, 10) > 3}
            currentLocale={locale}
            onToggleSubMenus={onToggleSubMenus}
            isShowingSubMenus
            onToggleUsedAddresses={action('onToggleUsedAddresses')}
            showUsed={boolean('showUsed', false)}
          />
          <WalletReceiveDialog
            address={generateAddress()}
            onCopyAddress={action('onCopyAddress')}
            onDownloadPDF={action('onDownloadPDF')}
            onSaveQRCodeImage={action('onSaveQRCodeImage')}
            onClose={action('onClose')}
            hwDeviceStatus={select(
              'Address verification state',
              {
                Verify: HwDeviceStatuses.VERIFYING_ADDRESS,
                Verified: HwDeviceStatuses.VERIFYING_ADDRESS_SUCCEEDED,
                Errored: HwDeviceStatuses.VERIFYING_ADDRESS_FAILED,
              },
              HwDeviceStatuses.VERIFYING_ADDRESS
            )}
            isHardwareWallet
            walletName="Ledger Nano S"
            isAddressDerived={false}
            isAddressChecked={false}
            onChangeVerificationStatus={action('onChangeVerificationStatus')}
            onSupportRequestClick={action('onSupportRequestClick')}
            isTrezor={boolean('isTrezor', false)}
          />
        </VerticalFlexContainer>
      );
    }
  )
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
          showUsed={boolean('showUsed', false)}
          onToggleUsedAddresses={action('onToggleUsedAddresses')}
        />
      </VerticalFlexContainer>
    );
  });
