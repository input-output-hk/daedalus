// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import NewsFeedIcon from '../components/widgets/NewsFeedIcon';
import CountdownPartyIcon from '../components/widgets/CountdownPartyIcon';
import FullyDecentralizedEffect from '../components/widgets/FullyDecentralizedEffect';
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
    const {
      sidebar,
      app,
      networkStatus,
      wallets,
      newsFeed,
      appUpdate,
    } = stores;
    const { isSynced, syncPercentage } = networkStatus;
    const { active, isWalletRoute, hasAnyWallets, hasRewardsWallets } = wallets;
    const { isShelleyActivated, epochToFullyDecentralized } = networkStatus;
    const {
      currentRoute,
      environment: { isMainnet, network },
      openExternalLink,
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

    const onWalletAdd = () => {
      actions.router.goToRoute.trigger({
        route: ROUTES.WALLETS.ADD,
      });
    };

    const onTransferFunds = (sourceWalletId: string) =>
      actions.wallets.transferFundsSetSourceWalletId.trigger({
        sourceWalletId,
      });

    const { unread } = newsFeed.newsFeedData;
    const { displayAppUpdateNewsItem } = appUpdate;

    const hasUnreadNews = unread.length > 0;

    return (
      <TopBar
        leftIcon={leftIcon}
        onLeftIconClick={actions.sidebar.toggleSubMenus.trigger}
        activeWallet={activeWallet}
        onTransferFunds={onTransferFunds}
        hasRewardsWallets={hasRewardsWallets}
        onWalletAdd={onWalletAdd}
        onLearnMore={openExternalLink}
        isShelleyActivated={isShelleyActivated}
        isFullyDecentralized={isFullyDecentralized}
      >
        {testnetLabel}
        <NodeSyncStatusIcon
          isSynced={isSynced}
          syncPercentage={syncPercentage}
        />
        {isFullyDecentralized && (
          <CountdownPartyIcon onIconClick={() => {}} shouldAnimate={false} />
        )}

        <NewsFeedIcon
          onNewsFeedIconClick={() => {}}
          hasNotification={false}
          hasUpdate={false}
        />
        <FullyDecentralizedEffect isActive={isFullyDecentralized} />
        <NewsFeedIcon
          onNewsFeedIconClick={actions.app.toggleNewsFeed.trigger}
          hasNotification={hasUnreadNews}
          hasUpdate={displayAppUpdateNewsItem}
        />
      </TopBar>
    );
  }
}
