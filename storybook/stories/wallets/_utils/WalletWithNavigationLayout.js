import React, { Component, Children } from 'react';
import { linkTo } from '@storybook/addon-links';
import { get } from 'lodash';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

const walletStories = {
  send: 'Wallets|Send',
  receive: 'Wallets|Receive',
  transactions: 'Wallets|Transactions',
  summary: 'Wallets|Summary',
  settings: 'Wallets|Settings',
};

export default class WalletWithNavigationLayout extends Component<Props> {
  static defaultProps = { stores: null, storiesProps: null };

  getItemFromContext = context => {
    return context.kind.replace('Wallets|', '').toLocaleLowerCase();
  };

  render() {
    const { stores, context } = this.props;
    const activeWallet = get(stores, ['wallets', 'active']);
    const { hasPassword, isLegacy, isNotResponding } = activeWallet;
    const contextItem = context.kind
      .replace('Wallets|', '')
      .toLocaleLowerCase();

    return (
      <WalletWithNavigation
        activeItem={contextItem}
        isActiveScreen={item => item === contextItem}
        hasPassword={hasPassword}
        isLegacy={isLegacy}
        isNotResponding={isNotResponding}
        isSetWalletPasswordDialogOpen={false}
        onWalletNavItemClick={linkTo(item => walletStories[item])}
        onSetWalletPassword={() => {}}
        onOpenExternalLink={() => {}}
        onRestartNode={() => {}}
      >
        {Children.map(this.props.children, child => {
          return React.cloneElement(child, {
            stores,
            wallet: activeWallet || child.props.wallet,
          });
        })}
      </WalletWithNavigation>
    );
  }
}
