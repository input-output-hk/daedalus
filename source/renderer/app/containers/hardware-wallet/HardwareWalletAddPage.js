// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConnectHardwareWallet from '../../components/hardware-wallet/settings/ConnectHardwareWallet';
import Layout from '../MainLayout';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores')
@observer
export default class HardwareWalletAddPage extends Component<InjectedContainerProps> {
  static defaultProps = { stores: null };

  componentDidMount() {
    console.debug('!!!!!!! INITIATE POLLER - ADD PAGE !!!!!!!!');
    const { startDeviceFetchPoller } = this.props.stores.hardwareWallets;
    startDeviceFetchPoller();
  }

  componentWillUnmount() {
    const { hwDeviceStatus, stopDeviceFetchPoller, resetInitializedConnection } = this.props.stores.hardwareWallets;
    console.debug(
      '!!!!!!! STOP POLLER - ADD PAGE !!!!!!!!',
      hwDeviceStatus,
    );
    stopDeviceFetchPoller();
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
