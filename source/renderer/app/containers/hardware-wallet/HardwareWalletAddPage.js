// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConnectHardwareWallet from '../../components/hardware-wallet/settings/ConnectHardwareWallet';
import Layout from '../MainLayout';
import type { InjectedStoresProps } from '../../types/injectedPropsType';

type Props = InjectedStoresProps;

@inject('stores')
@observer
export default class HardwareWalletAddPage extends Component<Props> {
  static defaultProps = { stores: null };

  componentDidMount() {
    const {
      establishHardwareWalletConnection,
    } = this.props.stores.hardwareWallets;
    establishHardwareWalletConnection();
  }

  componentWillUnmount() {
    const {
      hwDeviceStatus,
      stopCardanoAdaAppFetchPoller,
      resetInitializedConnection,
    } = this.props.stores.hardwareWallets;
    stopCardanoAdaAppFetchPoller();
    if (!hwDeviceStatus.READY) {
      resetInitializedConnection();
    }
  }

  render() {
    const {
      hwDeviceStatus,
      transportDevice,
    } = this.props.stores.hardwareWallets;

    return (
      <Layout>
        <ConnectHardwareWallet
          transportDevice={transportDevice}
          hwDeviceStatus={hwDeviceStatus}
        />
      </Layout>
    );
  }
}
