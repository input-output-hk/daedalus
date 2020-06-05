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
    const {
      stopDeviceFetchPoller,
      resetInitializedConnection,
      isDeviceConnected,
      isExtendedPublicKeyExported,
    } = this.props.stores.hardwareWallets;
    console.debug(
      '!!!!!!! STOP POLLER - ADD PAGE !!!!!!!!',
      isDeviceConnected,
      isExtendedPublicKeyExported
    );
    stopDeviceFetchPoller();
    if (
      !isDeviceConnected ||
      (isDeviceConnected && !isExtendedPublicKeyExported)
    ) {
      resetInitializedConnection();
    }
  }

  render() {
    const { stores } = this.props;
    const { app, hardwareWallets } = stores;

    const {
      fetchingDevice,
      isDeviceConnected,
      isExportingExtendedPublicKey,
      isExtendedPublicKeyExported,
      isExportingPublicKeyAborted,
      isCardanoAppLaunched,
      isTrezor,
      isLedger,
      transport,
    } = hardwareWallets;

    console.debug('>>>> TRANSPORT: ', {
      transport,
      isTrezor,
      isLedger,
      isExportingExtendedPublicKey,
    });

    return (
      <Layout>
        <ConnectHardwareWallet
          onOpenExternalLink={(url: string) => app.openExternalLink(url)}
          isLedger={isLedger}
          isTrezor={isTrezor}
          isDeviceConnected={isDeviceConnected}
          fetchingDevice={fetchingDevice}
          isExportingExtendedPublicKey={isExportingExtendedPublicKey}
          isExportingPublicKeyAborted={isExportingPublicKeyAborted}
          isExtendedPublicKeyExported={isExtendedPublicKeyExported}
          isCardanoAppLaunched={isCardanoAppLaunched}
        />
      </Layout>
    );
  }
}
