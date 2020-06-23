// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean } from '@storybook/addon-knobs';
import ConnectHardwareWallet from '../../../../source/renderer/app/components/hardware-wallet/settings/ConnectHardwareWallet';
import HardwareWalletsWrapper from '../_utils/HardwareWalletsWrapper';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';

storiesOf('Wallets|Hardware Wallets', module)
  .addDecorator(HardwareWalletsWrapper)

  // ====== Stories ======

  .add('Hardware wallet connect Ledger step 1', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
    />
  ))

  .add('Hardware wallet connect Ledger step 2', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      hwDeviceStatus={HwDeviceStatuses.LAUNCHING_CARDANO_APP}
    />
  ))

  .add('Hardware wallet connect Ledger step 3', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY}
    />
  ))

  .add('Hardware wallet connect Ledger step 4', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED}
    />
  ))

  .add('Hardware wallet connect Ledger step 5', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', true)}
      isTrezor={boolean('isTrezor', false)}
      hwDeviceStatus={HwDeviceStatuses.READY}
    />
  ))

  .add('Hardware wallet connect Ledger step 1', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
    />
  ))

  .add('Hardware wallet connect Ledger step 2', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      hwDeviceStatus={HwDeviceStatuses.LAUNCHING_CARDANO_APP}
    />
  ))

  .add('Hardware wallet connect Ledger step 3', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY}
    />
  ))

  .add('Hardware wallet connect Ledger step 4', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED}
    />
  ))

  .add('Hardware wallet connect Ledger step 5', () => (
    <ConnectHardwareWallet
      isLedger={boolean('isLedger', false)}
      isTrezor={boolean('isTrezor', true)}
      hwDeviceStatus={HwDeviceStatuses.READY}
    />
  ));
