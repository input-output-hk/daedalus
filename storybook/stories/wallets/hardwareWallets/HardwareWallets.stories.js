// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import ConnectHardwareWallet from '../../../../source/renderer/app/components/hardware-wallet/settings/ConnectHardwareWallet';
import HardwareWalletsWrapper from '../_utils/HardwareWalletsWrapper';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../../source/common/types/hardware-wallets.types';

const ledgerDevice = {
  deviceId: '1',
  deviceType: DeviceTypes.LEDGER,
  deviceModel: DeviceModels.LEDGER_NANO_S,
  deviceName: 'Ledger Nano S',
};

const trezorDevice = {
  deviceId: '2',
  deviceType: DeviceTypes.TREZOR,
  deviceModel: DeviceModels.TREZOR_T,
  deviceName: 'Trezor Model T',
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
