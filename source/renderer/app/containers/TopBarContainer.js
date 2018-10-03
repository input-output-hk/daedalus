// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
import type { InjectedProps } from '../types/injectedPropsType';
import environment from '../../../common/environment';
import { formattedWalletAmount } from '../utils/formatters';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class TopBarContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, app, networkStatus } = stores;
    const walletsStore = stores.ada.wallets;
    const isMainnet = environment.isMainnet();
    const testnetLabel = (
      !isMainnet ? <WalletTestEnvironmentLabel network={environment.NETWORK} /> : null
    );

    return (
      <TopBar
        onToggleSidebar={actions.sidebar.toggleSubMenus.trigger}
        activeWallet={walletsStore.active}
        currentRoute={app.currentRoute}
        showSubMenus={sidebar.isShowingSubMenus}
        formattedWalletAmount={formattedWalletAmount}
        showSubMenuToggle={walletsStore.isWalletRoute && walletsStore.hasAnyWallets}
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
