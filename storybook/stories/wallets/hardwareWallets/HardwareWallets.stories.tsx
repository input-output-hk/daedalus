import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import WalletConnectDialog from '../../../../source/renderer/app/components/wallet/WalletConnectDialog';
import HardwareWalletsWrapper from '../_utils/HardwareWalletsWrapper';
import { HwDeviceStatuses } from '../../../../source/renderer/app/domains/Wallet';
import {
  MINIMAL_TREZOR_FIRMWARE_VERSION,
  MINIMAL_LEDGER_FIRMWARE_VERSION,
} from '../../../../source/renderer/app/config/hardwareWalletsConfig';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../../source/common/types/hardware-wallets.types';

const ledgerDevice = {
  deviceId: '1',
  deviceType: DeviceTypes.LEDGER,
  deviceModel: DeviceModels.LEDGER_NANO_S,
  deviceName: 'Ledger Nano S',
  path: null,
  firmwareVersion: MINIMAL_LEDGER_FIRMWARE_VERSION,
};
const trezorDevice = {
  deviceId: '2',
  deviceType: DeviceTypes.TREZOR,
  deviceModel: DeviceModels.TREZOR_T,
  deviceName: 'Trezor Model T',
  path: null,
  firmwareVersion: MINIMAL_TREZOR_FIRMWARE_VERSION,
};
storiesOf('Wallets|Hardware Wallets', module)
  .addDecorator(HardwareWalletsWrapper) // ====== Stories ======
  .add('Hardware wallet connect Ledger step 1', () => (
    <WalletConnectDialog
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Ledger step 2', () => (
    <WalletConnectDialog
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.LAUNCHING_CARDANO_APP}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Ledger step 3', () => (
    <WalletConnectDialog
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Ledger step 4', () => (
    <WalletConnectDialog
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Ledger step 5', () => (
    <WalletConnectDialog
      transportDevice={ledgerDevice}
      hwDeviceStatus={HwDeviceStatuses.READY}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Trezor step 1', () => (
    <WalletConnectDialog
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.CONNECTING}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Trezor step 2', () => (
    <WalletConnectDialog
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.LAUNCHING_CARDANO_APP}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Trezor step 3', () => (
    <WalletConnectDialog
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Trezor step 4', () => (
    <WalletConnectDialog
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ))
  .add('Hardware wallet connect Trezor step 5', () => (
    <WalletConnectDialog
      transportDevice={trezorDevice}
      hwDeviceStatus={HwDeviceStatuses.READY}
      onClose={action('onClose')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isSubmitting={action('isSubmitting')}
      onExternalLinkClick={action('onOpenExternalLink')}
      error={null}
    />
  ));
