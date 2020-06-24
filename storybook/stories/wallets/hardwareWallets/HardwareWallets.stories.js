// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import ConnectHardwareWallet from '../../../../source/renderer/app/components/hardware-wallet/settings/ConnectHardwareWallet';
import HardwareWalletsWrapper from '../_utils/HardwareWalletsWrapper';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../../source/renderer/app/stores/HardwareWalletsStore';

const ledgerDevice = {
  id: DeviceModels.LEDGER_NANO_S,
  productName: 'Ledger Nano S',
  deviceType: DeviceTypes.LEDGER,
};

const trezorDevice = {
  id: DeviceModels.TREZOR,
  productName: 'Trezor',
  deviceType: DeviceTypes.TREZOR,
};

storiesOf('Wallets|Hardware Wallets', module)
  .addDecorator(HardwareWalletsWrapper)

  // ====== Stories ======

  .add('Hardware wallet connect Ledger step 1', () => (
    <ConnectHardwareWallet
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
    />
  ))

  .add('Hardware wallet connect Ledger step 2', () => (
    <ConnectHardwareWallet
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.LAUNCHING_CARDANO_APP}
    />
  ))

  .add('Hardware wallet connect Ledger step 3', () => (
    <ConnectHardwareWallet
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY}
    />
  ))

  .add('Hardware wallet connect Ledger step 4', () => (
    <ConnectHardwareWallet
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED}
    />
  ))

  .add('Hardware wallet connect Ledger step 5', () => (
    <ConnectHardwareWallet
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.READY}
    />
  ))

  .add('Hardware wallet connect Trezor step 1', () => (
    <ConnectHardwareWallet
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
    />
  ))

  .add('Hardware wallet connect Trezor step 2', () => (
    <ConnectHardwareWallet
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.LAUNCHING_CARDANO_APP}
    />
  ))

  .add('Hardware wallet connect Trezor step 3', () => (
    <ConnectHardwareWallet
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY}
    />
  ))

  .add('Hardware wallet connect Trezor step 4', () => (
    <ConnectHardwareWallet
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED}
    />
  ))

  .add('Hardware wallet connect Trezor step 5', () => (
    <ConnectHardwareWallet
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.READY}
    />
  ));
