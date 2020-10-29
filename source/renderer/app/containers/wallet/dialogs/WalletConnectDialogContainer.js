// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletConnectDialog from '../../../components/wallet/WalletConnectDialog';
import type { InjectedStoresProps } from '../../../types/injectedPropsType';

type Props = InjectedStoresProps;

@inject('stores')
@observer
export default class WalletConnectDialogContainer extends Component<Props> {
  static defaultProps = { stores: null };

  onClose = () => {
    const {
      stopCardanoAdaAppFetchPoller,
      resetInitializedConnection,
    } = this.props.stores.hardwareWallets;
    stopCardanoAdaAppFetchPoller();
    resetInitializedConnection();
    this.props.onClose();
  };

  render() {
    const { stores } = this.props;
    const { hardwareWallets, wallets } = stores;
    const { hwDeviceStatus, transportDevice } = hardwareWallets;
    const { createHardwareWalletRequest } = wallets;
    return (
      <WalletConnectDialog
        isSubmitting={createHardwareWalletRequest.isExecuting}
        error={createHardwareWalletRequest.error}
        onClose={this.onClose}
        hwDeviceStatus={hwDeviceStatus}
        transportDevice={transportDevice}
      />
    );
  }
}
