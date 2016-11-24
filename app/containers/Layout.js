// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';

@observer(['state', 'controller'])
export default class Layout extends Component {

  static propTypes = {
    state: PropTypes.shape({
      sidebar: MobxPropTypes.observableObject.isRequired,
      activeWallet: PropTypes.shape({
        wallet: PropTypes.shape({
          address: PropTypes.string.isRequired
        }).isRequired
      }).isRequired
    }).isRequired,
    controller: PropTypes.shape({
      sidebar: PropTypes.shape({
        changeSidebarRoute: PropTypes.func.isRequired,
        toggleSidebar: PropTypes.func.isRequired,
      }).isRequired,
      wallets: PropTypes.shape({
        setActiveWallet: PropTypes.func.isRequired,
        createPersonalWallet: PropTypes.func.isRequired
      }).isRequired
    }).isRequired,
    children: oneOrManyChildElements
  };

  state = {
    isAddingWallet: false
  };

  handleAddWalletSubmit(values: Object) {
    this.props.controller.wallets.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
    this.cancelAddWalletDialog();
  }

  cancelAddWalletDialog() {
    this.setState({ isAddingWallet: false });
  }

  render() {
    const { sidebar, activeWallet } = this.props.state;
    const { controller } = this.props;
    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: () => this.setState({ isAddingWallet: true }),
          onWalletItemClick: (wallet) => controller.wallets.setActiveWallet(wallet)
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
        activeWalletId={activeWallet.wallet.address}
      />
    );
    const appbar = <AppBar onToggleSidebar={() => controller.sidebar.toggleSidebar()} />;
    const addWalletDialog = this.state.isAddingWallet ? (
      <WalletCreateDialog
        onSubmit={this.handleAddWalletSubmit.bind(this)}
        onCancel={this.cancelAddWalletDialog.bind(this)}
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
