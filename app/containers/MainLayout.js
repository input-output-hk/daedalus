// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';
import Wallet from '../domain/Wallet';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      sidebar: PropTypes.shape({
        isCreateWalletDialogOpen: PropTypes.bool.isRequired,
      }).isRequired,
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet).isRequired,
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
      createPersonalWallet: PropTypes.func.isRequired,
      toggleCreateWalletDialog: PropTypes.func.isRequired
    }).isRequired,
    children: oneOrManyChildElements
  };

  handleAddWalletSubmit = (values: Object) => {
    this.props.actions.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  };

  toggleCreateWalletDialog = () => {
    this.props.actions.toggleCreateWalletDialog();
  };

  routeToWallet = (walletId) => {
    const { actions, stores } = this.props;
    actions.goToRoute({ route: stores.wallets.getWalletRoute(walletId) });
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar } = stores;
    const activeWallet = stores.wallets.active;
    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: this.toggleCreateWalletDialog,
          onWalletItemClick: this.routeToWallet,
        }
      }
    };
    const sidebarComponent = (
      <Sidebar
        route={sidebar.route}
        menus={sidebarMenus}
        hidden={sidebar.hidden}
        isMaximized={sidebar.isMaximized}
        onCategoryClicked={route => actions.changeSidebarRoute({ route })}
        activeWalletId={activeWallet.id}
      />
    );
    const appbar = <AppBar onToggleSidebar={actions.toggleSidebar} />;
    const addWalletDialog = sidebar.isCreateWalletDialogOpen ? (
      <WalletCreateDialog
        onSubmit={this.handleAddWalletSubmit}
        onCancel={this.toggleCreateWalletDialog}
      />
    ) : null;
    return (
      <SidebarLayout sidebar={sidebarComponent} appbar={appbar}>
        {this.props.children}
        {addWalletDialog}
      </SidebarLayout>
    );
  }
}
