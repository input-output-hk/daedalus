// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import HardwareWalletNavigation from '../navigation/HardwareWalletNavigation';
import styles from './HardwareWalletWithNavigation.scss';
import ConnectHardwareWallet from '../settings/ConnectHardwareWallet';

type Props = {
  children?: Node,
  activeItem: string,
  hasNotification?: boolean,
  walletNotConnected?: boolean,
  isActiveScreen: Function,
  onOpenExternalLink: Function,
  onWalletNavItemClick: Function,
  isLedger: boolean,
  isTrezor: boolean,
  isDeviceConnected: boolean,
  fetchingDevice: boolean,
  exportingExtendedPublicKey: boolean,
  isExportingPublicKeyAborted: boolean,
};

@observer
export default class HardwareWalletWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      activeItem,
      hasNotification,
      walletNotConnected,
      isActiveScreen,
      onWalletNavItemClick,
      onOpenExternalLink,
      isLedger,
      isTrezor,
      isDeviceConnected,
      fetchingDevice,
      exportingExtendedPublicKey,
      isExportingPublicKeyAborted,
    } = this.props;

    return (
      <div className={styles.component}>
        {walletNotConnected ? (
          <ConnectHardwareWallet
            onOpenExternalLink={onOpenExternalLink}
            isLedger={isLedger}
            isTrezor={isTrezor}
            isDeviceConnected={isDeviceConnected}
            fetchingDevice={fetchingDevice}
            exportingExtendedPublicKey={exportingExtendedPublicKey}
            isExportingPublicKeyAborted={isExportingPublicKeyAborted}
          />
        ) : (
          <div className={styles.navigation}>
            <HardwareWalletNavigation
              isActiveNavItem={isActiveScreen}
              onNavItemClick={onWalletNavItemClick}
              activeItem={activeItem}
              hasNotification={hasNotification}
            />
          </div>
        )}

        <div className={styles.page}>{children}</div>
      </div>
    );
  }
}
