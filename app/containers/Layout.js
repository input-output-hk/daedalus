// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';

@observer(['store', 'controller'])
export default class Layout extends Component {

  static propTypes = {
    store: PropTypes.shape({
      sidebar: MobxPropTypes.observableObject.isRequired,
    }),
    controller: PropTypes.shape({
      sidebar: PropTypes.shape({
        changeSidebarRoute: PropTypes.func.isRequired,
        toggleSidebar: PropTypes.func.isRequired,
      })
    }),
    children: oneOrManyChildElements
  };

  render() {
    const { sidebar } = this.props.store;
    const { controller } = this.props;
    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: () => {}
        }
      }
    };
    const sidebarComponent = (
      <Sidebar
        route={sidebar.route}
        menus={sidebarMenus}
        hidden={sidebar.hidden}
        showMenu={sidebar.showMenu}
        onCategoryClicked={controller.sidebar.changeSidebarRoute}
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
