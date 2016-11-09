// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import { toggleSidebar } from '../actions/ui-actions';

@observer(['store'])
export default class Layout extends Component {

  static propTypes = {
    store: PropTypes.shape({
      sidebar: PropTypes.object,
    }),
    children: oneOrManyChildElements,
  };

  render() {
    const { store } = this.props;
    const sidebarMenus = {
      wallets: {
        items: [
          { id: '1', title: 'Main wallet', info: 'ADA' },
          { id: '2', title: 'House rent', info: '274912874,35 ADA' },
          { id: '3', title: 'Mining', info: '0,0004924712 BTC' },
          { id: '4', title: 'Shopping wallet', info: 'ADA' },
        ],
        actions: {
          onAddWallet: () => {}
        }
      }
    };
    const sidebar = (
      <Sidebar
        routePath="/wallets"
        menus={sidebarMenus}
        hidden={store.sidebar.hidden}
        showMenus={store.sidebar.showMenus}
      />
    );
    const appbar = <AppBar onToggleSidebar={toggleSidebar} />;
    return (
      <SidebarLayout sidebar={sidebar} appbar={appbar}>
        {this.props.children}
      </SidebarLayout>
    );
  }
}
