// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConnectHardwareWallet from '../../components/hardware-wallet/settings/ConnectHardwareWallet';
import Layout from '../MainLayout';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class HardwareWalletAddPage extends Component<InjectedContainerProps> {
  static defaultProps = { stores: null, actions: null };

  componentDidMount() {
    const { establishHardwareWalletConnection } = this.props.stores.hardwareWallets;
    establishHardwareWalletConnection();
  }

  componentWillUnmount() {
    const { hwDeviceStatus, stopCardanoAdaAppFetchPoller, resetInitializedConnection } = this.props.stores.hardwareWallets;
    stopCardanoAdaAppFetchPoller();
    if (!hwDeviceStatus.READY) {
      resetInitializedConnection();
    }
  }

  render() {
    const { hardwareWallets } = this.props.stores;
    const { isTrezor, isLedger, hwDeviceStatus } = hardwareWallets;

    return (
      <Layout>
        <ConnectHardwareWallet
          isLedger={isLedger}
          isTrezor={isTrezor}
          hwDeviceStatus={hwDeviceStatus}
        />
      </Layout>
    );
  }
}
