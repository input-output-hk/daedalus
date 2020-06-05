// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import ConnectHardwareWallet from '../../../../source/renderer/app/components/hardware-wallet/settings/ConnectHardwareWallet';
import HardwareWalletsWrapper from '../_utils/HardwareWalletsWrapper';

storiesOf('Wallets|Hardware Wallets', module)
  .addDecorator(HardwareWalletsWrapper)

  // ====== Stories ======

  .add('Hardware wallet connect Ledger step 1', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', false)}
      isDeviceConnected={boolean('isDeviceConnected', false)}
      fetchingDevice={boolean('fetchingDevice', true)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', null)}
      isExportingPublicKeyAborted={boolean(
        'isExportingPublicKeyAborted',
        false
      )}
    />
  ))

  .add('Hardware wallet connect Ledger step 2', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', false)}
      isDeviceConnected={boolean('isDeviceConnected', true)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', true)}
      isExportingPublicKeyAborted={boolean(
        'isExportingPublicKeyAborted',
        false
      )}
    />
  ))

  .add('Hardware wallet connect Ledger step 3', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', true)}
      isDeviceConnected={boolean('isDeviceConnected', null)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', null)}
      isExportingPublicKeyAborted={boolean(
        'isExportingPublicKeyAborted',
        false
      )}
    />
  ))

  .add('Hardware wallet connect Ledger step 4', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', true)}
      isDeviceConnected={boolean('isDeviceConnected', true)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', false)}
      isExportingPublicKeyAborted={boolean('isExportingPublicKeyAborted', true)}
    />
  ))

  .add('Hardware wallet connect Ledger step 5', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', false)}
      isDeviceConnected={boolean('isDeviceConnected', false)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', false)}
      isExportingPublicKeyAborted={boolean('isExportingPublicKeyAborted', false)}
    />
  ))

  .add('Hardware wallet connect Trezor step 1', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', false)}
      isDeviceConnected={boolean('isDeviceConnected', false)}
      fetchingDevice={boolean('fetchingDevice', true)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', null)}
      isExportingPublicKeyAborted={boolean(
        'isExportingPublicKeyAborted',
        false
      )}
    />
  ))

  .add('Hardware wallet connect Trezor step 2', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', false)}
      isDeviceConnected={boolean('isDeviceConnected', true)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', true)}
      isExportingPublicKeyAborted={boolean(
        'isExportingPublicKeyAborted',
        false
      )}
    />
  ))

  .add('Hardware wallet connect Trezor step 3', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', true)}
      isDeviceConnected={boolean('isDeviceConnected', null)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', null)}
      isExportingPublicKeyAborted={boolean(
        'isExportingPublicKeyAborted',
        false
      )}
    />
  ))

  .add('Hardware wallet connect Trezor step 4', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', true)}
      isDeviceConnected={boolean('isDeviceConnected', true)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', false)}
      isExportingPublicKeyAborted={boolean('isExportingPublicKeyAborted', true)}
    />
  ))

  .add('Hardware wallet connect Trezor step 5', () => (
    <ConnectHardwareWallet
      onOpenExternalLink={action('onOpenExternalLink')}
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      isCardanoAppLaunched={boolean('isCardanoAppLaunched', false)}
      isDeviceConnected={boolean('isDeviceConnected', false)}
      fetchingDevice={boolean('fetchingDevice', false)}
      exportingExtendedPublicKey={boolean('exportingExtendedPublicKey', false)}
      isExportingPublicKeyAborted={boolean('isExportingPublicKeyAborted', false)}
    />
  ));
