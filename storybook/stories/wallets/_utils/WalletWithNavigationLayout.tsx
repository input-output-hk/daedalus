import React, { Component, Children } from 'react';
import { linkTo } from '@storybook/addon-links';
import { get } from 'lodash';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';
// Context has many changeable props but "kind" is required
type contextType = {
  kind: string;
};
type Props = {
  context: contextType;
  children?: any | Node;
  stores?: {} | null | undefined;
};
const walletStories = {
  send: 'Wallets|Send',
  receive: 'Wallets|Receive',
  transactions: 'Wallets|Transactions',
  tokens: 'Wallets|Tokens',
  summary: 'Wallets|Summary',
  settings: 'Wallets|Settings',
};
export default class WalletWithNavigationLayout extends Component<Props> {
  static defaultProps = {
    stores: null,
    storiesProps: null,
  };
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
      <WalletWithNavigation
        activeItem={contextItem}
        isActiveScreen={(item) => item === contextItem}
        hasPassword={hasPassword}
        isLegacy={isLegacy}
        isNotResponding={isNotResponding}
        isSetWalletPasswordDialogOpen={false}
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(item: any) => any' is not assig... Remove this comment to see the full error message
        onWalletNavItemClick={linkTo((item) => walletStories[item])}
        onSetWalletPassword={() => {}}
        onOpenExternalLink={() => {}}
        onRestartNode={() => {}}
        isHardwareWallet={false}
      >
        {Children.map(children, (child) =>
          React.cloneElement(child, {
            stores,
            wallet: activeWallet || child.props.wallet,
          })
        )}
      </WalletWithNavigation>
    );
  }
}
