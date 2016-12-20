// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';

@observer(['state', 'controller'])
export default class MainLayout extends Component {

  static propTypes = {
    state: PropTypes.shape({
      sidebar: MobxPropTypes.observableObject.isRequired,
      activeWallet: PropTypes.shape({
        wallet: PropTypes.shape({
          id: PropTypes.string.isRequired
        }).isRequired
      }).isRequired,
      isCreateWalletDialogOpen: PropTypes.bool.isRequired
    }).isRequired,
    controller: PropTypes.shape({
      sidebar: PropTypes.shape({
        changeSidebarRoute: PropTypes.func.isRequired,
        toggleSidebar: PropTypes.func.isRequired,
      }).isRequired,
      wallets: PropTypes.shape({
        setActiveWallet: PropTypes.func.isRequired,
        createPersonalWallet: PropTypes.func.isRequired,
        toggleCreateWalletDialog: PropTypes.func.isRequired
      }).isRequired
    }).isRequired,
    children: oneOrManyChildElements
  };

  handleAddWalletSubmit(values: Object) {
    this.props.controller.wallets.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
    this.toggleCreateWalletDialog();
  }

  toggleCreateWalletDialog() {
    this.props.controller.wallets.toggleCreateWalletDialog();
  }

  render() {
    const { sidebar, activeWallet } = this.props.state;
    const { controller } = this.props;
    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: this.toggleCreateWalletDialog.bind(this),
          onWalletItemClick: wallet => controller.wallets.setActiveWallet(wallet)
        }
      }
    };
    const sidebarComponent = (
      <Sidebar
        route={sidebar.route}
        menus={sidebarMenus}
        hidden={sidebar.hidden}
        isMaximized={sidebar.isMaximized}
        onCategoryClicked={(cat) => controller.sidebar.changeSidebarRoute(cat)}
        activeWalletId={activeWallet.wallet.id}
      />
    );
    const appbar = <AppBar onToggleSidebar={() => controller.sidebar.toggleSidebar()} />;
    const addWalletDialog = this.props.state.isCreateWalletDialogOpen ? (
      <WalletCreateDialog
        onSubmit={this.handleAddWalletSubmit.bind(this)}
        onCancel={this.toggleCreateWalletDialog.bind(this)}
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
