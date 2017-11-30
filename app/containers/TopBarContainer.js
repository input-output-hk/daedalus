// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
import type { InjectedProps } from '../types/injectedPropsType';
import environment from '../environment';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class TopBarContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, app, networkStatus } = stores;
    const isMainnet = environment.isMainnet();
    const isAdaApi = environment.isAdaApi();
    const testnetLabel = (
      isAdaApi && !isMainnet ? <WalletTestEnvironmentLabel /> : null
    );

    return (
      <TopBar
        onToggleSidebar={actions.sidebar.toggleSubMenus.trigger}
        activeWallet={stores[environment.API].wallets.active}
        currentRoute={app.currentRoute}
        showSubMenus={sidebar.isShowingSubMenus}
      >
        {testnetLabel}
        <NodeSyncStatusIcon
          networkStatus={networkStatus}
          isMainnet={isMainnet}
        />
      </TopBar>
    );
  }

}
