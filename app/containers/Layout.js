// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import { changeSidebarRoute, toggleSidebar } from '../actions/sidebar-actions';

@observer(['store'])
export default class Layout extends Component {

  static propTypes = {
    store: PropTypes.shape({
      sidebar: MobxPropTypes.observableObject.isRequired,
      walletsForSidebar: MobxPropTypes.arrayOrObservableArray.isRequired
    }),
    children: oneOrManyChildElements
  };

  render() {
    const { sidebar, walletsForSidebar } = this.props.store;
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
