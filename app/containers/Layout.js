// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import { changeSidebarRoute, toggleSidebar } from '../actions/sidebar-actions';

@observer(['state'])
export default class Layout extends Component {

  static propTypes = {
    state: PropTypes.shape({
      uiStore: PropTypes.shape({
        sidebar: PropTypes.object
      }),
    }),
    children: oneOrManyChildElements,
  };

  render() {
    const { uiStore } = this.props.state;
    const { sidebar } = uiStore;
    const sidebarMenus = {
      wallets: {
        items: uiStore.walletsForSidebar,
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
        onCategoryClicked={changeSidebarRoute}
      />
    );
    const appbar = <AppBar onToggleSidebar={() => toggleSidebar()} />;
    return (
      <SidebarLayout sidebar={sidebarComponent} appbar={appbar}>
        {this.props.children}
      </SidebarLayout>
    );
  }
}
