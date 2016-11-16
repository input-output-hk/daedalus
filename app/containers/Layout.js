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
    }),
    controller: PropTypes.shape({
      sidebar: PropTypes.shape({
        changeSidebarRoute: PropTypes.func.isRequired,
        toggleSidebar: PropTypes.func.isRequired,
      }),
      wallets: PropTypes.shape({
        setActiveWallet: PropTypes.func.isRequired
      })
    }),
    children: oneOrManyChildElements
  };

  render() {
    const { sidebar } = this.props.state;
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
