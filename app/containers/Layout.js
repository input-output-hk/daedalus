// @flow
import React, { Component } from 'react';
import { observer, PropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import { changeSidebarRoute, toggleSidebar } from '../actions/sidebar-actions';

@observer(['sidebar', 'walletsForSidebar'])
export default class Layout extends Component {

  static propTypes = {
    sidebar: PropTypes.observableObject,
    walletsForSidebar: PropTypes.observableArray,
    children: oneOrManyChildElements,
  };

  render() {
    const { sidebar, walletsForSidebar } = this.props;
    const sidebarMenus = {
      wallets: {
        items: walletsForSidebar,
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
