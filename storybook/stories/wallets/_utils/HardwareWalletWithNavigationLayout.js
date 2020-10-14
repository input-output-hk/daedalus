// @flow
import React, { Component, Children } from 'react';
import { linkTo } from '@storybook/addon-links';
import { get } from 'lodash';
import HardwareWalletWithNavigation from '../../../../source/renderer/app/components/hardware-wallet/layouts/HardwareWalletWithNavigation';

// Context has many changeable props but "kind" is required
type contextType = {
  kind: string,
};

type Props = {
  context: contextType,
  children?: any | Node,
  stores?: ?{},
};

const walletStories = {
  send: 'Wallets|Send',
  receive: 'Wallets|Receive',
  transactions: 'Wallets|Transactions',
  summary: 'Wallets|Summary',
  settings: 'Wallets|Settings',
};

export default class WalletWithNavigationLayout extends Component<Props> {
  static defaultProps = { stores: null, storiesProps: null };

  getItemFromContext = (context: contextType) => {
    return context.kind.replace('Wallets|', '').toLocaleLowerCase();
  };

  render() {
    const { stores, context, children } = this.props;
    const activeWallet = get(stores, ['wallets', 'active']);
    const { hasPassword, isLegacy, isNotResponding } = activeWallet;
    const contextItem = context.kind
      .replace('Wallets|', '')
      .toLocaleLowerCase();

    return (
      <HardwareWalletWithNavigation
        activeItem={contextItem}
        isActiveScreen={(item) => item === contextItem}
        hasPassword={hasPassword}
        hasNotification={false}
        isLegacy={isLegacy}
        isNotResponding={isNotResponding}
        isSetWalletPasswordDialogOpen={false}
        onWalletNavItemClick={linkTo((item) => walletStories[item])}
        onSetWalletPassword={() => {}}
        onOpenExternalLink={() => {}}
        onRestartNode={() => {}}
        walletNotConnected
        isDeviceConnected={false}
        fetchingDevice={false}
        exportingExtendedPublicKey={false}
        isExportingPublicKeyAborted={false}
        isLedger
        isTrezor={false}
      >
        {Children.map(children, (child) =>
          React.cloneElement(child, {
            stores,
            wallet: activeWallet || child.props.wallet,
          })
        )}
      </HardwareWalletWithNavigation>
    );
  }
}
