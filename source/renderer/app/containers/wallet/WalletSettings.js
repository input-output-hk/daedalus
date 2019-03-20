// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettingsLayout from '../../components/wallet/WalletSettingsLayout';
import WalletSettingsMenu from '../../components/wallet/menu/WalletSettingsMenu';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class Settings extends Component<InjectedContainerProps> {
  static defaultProps = { actions: null, stores: null };

  get activeWalletId() {
    const { active } = this.props.stores.wallets;
    return active ? active.id : null;
  }

  isActivePage = (route: string) => {
    const { stores } = this.props;
    const { location } = stores.router;
    const id = this.activeWalletId;
    if (location && id) {
      return location.pathname === buildRoute(route, { id });
    }
    return false;
  };

  handleItemClick = (pattern: string) => {
    const id = this.activeWalletId;
    if (id) {
      const route = buildRoute(pattern, { id });
      this.props.actions.router.goToRoute.trigger({ route });
    }
  };

  render() {
    const { children } = this.props;

    const menu = (
      <WalletSettingsMenu
        onItemClick={this.handleItemClick}
        isActiveItem={this.isActivePage}
      />
    );
    return (
      <div>
        <WalletSettingsLayout menu={menu}>{children}</WalletSettingsLayout>
      </div>
    );
  }
}
