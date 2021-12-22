import React from 'react';
import { observer, inject } from 'mobx-react';
import classnames from 'classnames';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import { DiscreetToggleTopBar } from '../features/discreet-mode';
import NewsFeedIcon from '../components/widgets/NewsFeedIcon';
import TadaButton from '../components/widgets/TadaButton';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
import type { InjectedProps } from '../types/injectedPropsType';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/menu-opened-i... Remove this comment to see the full error message
import menuIconOpened from '../assets/images/menu-opened-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/menu-ic.inlin... Remove this comment to see the full error message
import menuIconClosed from '../assets/images/menu-ic.inline.svg';
import { matchRoute } from '../utils/routing';
import { ROUTES } from '../routes-config';
import { IS_TADA_ICON_AVAILABLE } from '../config/topBarConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../components/layout/TopBar.sc... Remove this comment to see the full error message
import topBarStyles from '../components/layout/TopBar.scss';

const TopBarContainer = (
  { actions, stores }: InjectedProps = {
    actions: null,
    stores: null,
  }
) => {
  const {
    sidebar,
    app,
    networkStatus,
    wallets,
    newsFeed,
    appUpdate,
    staking,
  } = stores;
  const {
    isSynced,
    syncPercentage,
    isShelleyActivated,
    isAlonzoActivated,
    isAlonzoPending,
  } = networkStatus;
  const { stakingInfoWasOpen } = staking;
  const shouldShowTadaIconAnimation = isAlonzoActivated && !stakingInfoWasOpen;
  const shouldShowTadaIcon =
    IS_TADA_ICON_AVAILABLE && (isAlonzoPending || isAlonzoActivated);
  const { active, isWalletRoute, hasAnyWallets, hasRewardsWallets } = wallets;
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

  const onClickTadaButton = () => {
    actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.INFO,
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
    >
      {testnetLabel}
      <NodeSyncStatusIcon
        isSynced={isSynced}
        syncPercentage={syncPercentage}
        hasTadaIcon={shouldShowTadaIcon}
      />
      <span
        className={classnames(
          topBarStyles.rectangle,
          shouldShowTadaIcon && topBarStyles.hasTadaIcon
        )}
      />
      <DiscreetToggleTopBar hasTadaIcon={shouldShowTadaIcon} />
      {shouldShowTadaIcon && (
        <TadaButton
          onClick={onClickTadaButton}
          shouldAnimate={shouldShowTadaIconAnimation}
        />
      )}
      <NewsFeedIcon
        onNewsFeedIconClick={actions.app.toggleNewsFeed.trigger}
        hasNotification={hasUnreadNews}
        hasUpdate={displayAppUpdateNewsItem}
      />
    </TopBar>
  );
};

export default inject('stores', 'actions')(observer(TopBarContainer));
