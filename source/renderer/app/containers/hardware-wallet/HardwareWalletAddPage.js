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

  render() {
    const { stores } = this.props;
    const { app, wallets } = stores;
    const {
      isDeviceConnected,
      fetchingDevice,
      exportingExtendedPublicKey,
      isExportingPublicKeyAborted,
    } = wallets;

    // @TODO - add real values
    const isLedger = true;
    const isTrezor = false;

    return (
      <Layout>
        <ConnectHardwareWallet
          onOpenExternalLink={(url: string) => app.openExternalLink(url)}
          isLedger={isLedger}
          isTrezor={isTrezor}
          isDeviceConnected={isDeviceConnected}
          fetchingDevice={fetchingDevice}
          exportingExtendedPublicKey={exportingExtendedPublicKey}
          isExportingPublicKeyAborted={isExportingPublicKeyAborted}
        />
      </Layout>
    );
  }
}
