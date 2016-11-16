// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';

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
        setActiveWallet: PropTypes.func.isRequired
      }).isRequired
    }).isRequired,
    children: oneOrManyChildElements
  };

  render() {
    const { sidebar, activeWallet } = this.props.state;
    const { controller } = this.props;
    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: () => {},
          onWalletItemClick: (wallet) => controller.wallets.setActiveWallet(wallet)
        }
      }
    };
    const sidebarComponent = (
      <Sidebar
        route={sidebar.route}
        menus={sidebarMenus}
        hidden={sidebar.hidden}
        showMenu={sidebar.showMenu}
        onCategoryClicked={(cat) => controller.sidebar.changeSidebarRoute(cat)}
        activeWalletId={activeWallet.wallet.address}
      />
    );
    const appbar = <AppBar onToggleSidebar={() => controller.sidebar.toggleSidebar()} />;
    return (
      <SidebarLayout sidebar={sidebarComponent} appbar={appbar}>
        {this.props.children}
      </SidebarLayout>
    );
  }
}
