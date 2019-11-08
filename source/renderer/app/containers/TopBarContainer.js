// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import NewsFeedIcon from '../components/widgets/NewsFeedIcon';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
import type { InjectedProps } from '../types/injectedPropsType';
import menuIconOpened from '../assets/images/menu-opened-ic.inline.svg';
import menuIconClosed from '../assets/images/menu-ic.inline.svg';
import { matchRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class TopBarContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, app, networkStatus, wallets, newsFeed } = stores;
    const { active, isWalletRoute, hasAnyWallets } = wallets;
    const {
      currentRoute,
      environment: { isMainnet, network },
    } = app;

    const walletRoutesMatch = matchRoute(
      `${ROUTES.WALLETS.ROOT}/:id(*page)`,
      currentRoute
    );
    const showSubMenuToggle = isWalletRoute && hasAnyWallets;
    const activeWallet = walletRoutesMatch && active != null ? active : null;
    const leftIconSVG = sidebar.isShowingSubMenus
      ? menuIconOpened
      : menuIconClosed;
    const leftIcon = showSubMenuToggle ? leftIconSVG : null;
    const testnetLabel = !isMainnet ? (
      <WalletTestEnvironmentLabel network={network} />
    ) : null;

    const onWalletAdd = actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.ADD,
    });

    const onTransferFunds = (sourceWalletId: string) =>
      actions.wallets.transferFundsSetSourceWalletId.trigger({
        sourceWalletId,
      });

    const { unread } = newsFeed.newsFeedData;
    const hasUnreadNews = unread.length > 0;

    return (
      <TopBar
        leftIcon={leftIcon}
        onLeftIconClick={actions.sidebar.toggleSubMenus.trigger}
        activeWallet={activeWallet}
        onTransferFunds={onTransferFunds}
        hasAnyWallets={hasAnyWallets}
        onWalletAdd={onWalletAdd}
      >
        {testnetLabel}
        <NodeSyncStatusIcon networkStatus={networkStatus} />
        <NewsFeedIcon
          onNewsFeedIconClick={actions.app.toggleNewsFeed.trigger}
          showDot={hasUnreadNews}
        />
      </TopBar>
    );
  }
}
