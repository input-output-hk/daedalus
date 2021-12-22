import React, { Component, Children } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import { action } from '@storybook/addon-actions';
import { select, boolean } from '@storybook/addon-knobs';
import classNames from 'classnames';
import { isShelleyTestnetTheme } from './utils';
// Assets and helpers
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';
import {
  DEFAULT_NUMBER_FORMAT,
  NUMBER_FORMATS,
} from '../../../source/common/types/number.types';
import { NUMBER_OPTIONS } from '../../../source/renderer/app/config/profileConfig';
import { formattedWalletAmount } from '../../../source/renderer/app/utils/formatters';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import TadaButton from '../../../source/renderer/app/components/widgets/TadaButton';
import { DiscreetToggleTopBar } from '../../../source/renderer/app/features';
import Wallet, {
  WalletSyncStateStatuses,
} from '../../../source/renderer/app/domains/Wallet';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import type { SidebarMenus } from '../../../source/renderer/app/components/sidebar/types';
import type { SidebarWalletType } from '../../../source/renderer/app/types/sidebarTypes';
// Empty screen elements
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/a... Remove this comment to see the full error message
import menuIconOpened from '../../../source/renderer/app/assets/images/menu-opened-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/a... Remove this comment to see the full error message
import menuIconClosed from '../../../source/renderer/app/assets/images/menu-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/c... Remove this comment to see the full error message
import topBarStyles from '../../../source/renderer/app/components/layout/TopBar.scss';

export type StoriesProps = {
  wallets: Array<Wallet>;
  activeWalletId: string;
  setActiveWalletId: (...args: Array<any>) => any;
};
type Props = {
  activeSidebarCategory: string;
  currentTheme: string;
  storiesProps: any | StoriesProps;
  story?: string;
  children?: any | Node;
  stores?: {} | null | undefined;
};
const CATEGORIES_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
];
const CATEGORIES = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.SETTINGS,
];

@inject('stores', 'storiesProps')
@observer
class StoryLayout extends Component<Props> {
  static defaultProps = {
    stores: null,
    storiesProps: null,
  };

  render() {
    const {
      activeSidebarCategory,
      currentTheme,
      story = '',
      storiesProps = {},
      stores,
      children,
    } = this.props;
    const { wallets, activeWalletId, setActiveWalletId } = storiesProps;
    const activeWallet: Wallet = wallets[parseInt(activeWalletId, 10)];
    const activeNavItem = story.split(' ')[0].toLowerCase();
    const sidebarMenus = this.getSidebarMenus(
      this.getSidebarWallets(wallets),
      activeWalletId,
      setActiveWalletId
    );
    const currentNumberFormat = select(
      'currentNumberFormat',
      NUMBER_OPTIONS.reduce((obj, option) => {
        obj[option.label] = option.value;
        return obj;
      }, {}),
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ value: string; label: string; ... Remove this comment to see the full error message
      NUMBER_OPTIONS[0]
    );
    const FORMAT = {
      ...DEFAULT_NUMBER_FORMAT,
      // @ts-ignore ts-migrate(2538) FIXME: Type 'PropertyKey[]' cannot be used as an index ty... Remove this comment to see the full error message
      ...NUMBER_FORMATS[currentNumberFormat],
    };
    BigNumber.config({
      FORMAT,
    });
    return (
      <div
        style={{
          minHeight: '100%',
          height: '100vh',
        }}
      >
        <SidebarLayout
          sidebar={this.getSidebar(
            story,
            activeSidebarCategory,
            sidebarMenus,
            currentTheme
          )}
          topbar={this.getTopbar(
            activeSidebarCategory,
            activeWallet,
            activeNavItem,
            currentTheme
          )}
        >
          {Children.map(children, (child) =>
            React.cloneElement(child, {
              stores,
              storiesProps,
            })
          )}
        </SidebarLayout>
      </div>
    );
  }

  @observable
  isShowingSubMenus =
    this.props.activeSidebarCategory === '/wallets' && !!this.props.children;
  getSidebarWallets = (wallets: Array<Wallet>): Array<SidebarWalletType> =>
    wallets.map((wallet: Wallet) => ({
      id: wallet.id,
      title: wallet.name,
      amount: wallet.amount,
      isConnected: true,
      hasPassword: wallet.hasPassword,
      isNotResponding:
        get(wallet, 'syncState.status', WalletSyncStateStatuses.READY) ===
        WalletSyncStateStatuses.NOT_RESPONDING,
      isRestoreActive:
        get(wallet, 'syncState.status', WalletSyncStateStatuses.READY) ===
        WalletSyncStateStatuses.RESTORING,
      restoreProgress: get(wallet, 'syncState.progress.quantity', 0),
      isLegacy: wallet.isLegacy,
      hasNotification: false,
    }));
  getSidebarMenus = (
    items: Array<SidebarWalletType>,
    activeWalletId: string,
    setActiveWalletId: (...args: Array<any>) => any
  ) => ({
    wallets: {
      items,
      activeWalletId,
      actions: {
        onAddWallet: action('toggleAddWallet'),
        onWalletItemClick: setActiveWalletId,
      },
    },
  });
  getSidebar = (
    story: string,
    activeSidebarCategory: string,
    sidebarMenus: SidebarMenus,
    currentTheme: string
  ) => {
    const sidebarCategories =
      story === 'Decentralization Start Info'
        ? CATEGORIES_COUNTDOWN
        : CATEGORIES;
    return (
      <Sidebar
        categories={sidebarCategories}
        activeSidebarCategory={activeSidebarCategory}
        menus={sidebarMenus}
        isShowingSubMenus={this.isShowingSubMenus}
        onActivateCategory={action('onActivateCategory')}
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ categories: { name: string; icon: any; rou... Remove this comment to see the full error message
        isDialogOpen={() => false}
        onAddWallet={action('onAddWallet')}
        onOpenDialog={action('onOpenDialog')}
        onSubmitSupportRequest={() => {}}
        pathname="/"
        currentTheme={currentTheme}
        network="testnet"
        isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
      />
    );
  };
  getTopbar = (
    activeSidebarCategory: string,
    activeWallet: Wallet,
    activeNavItem: string,
    currentTheme: string
  ) => (
    <TopBar
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      onToggleSidebar={() => {
        runInAction(() => {
          this.isShowingSubMenus = !this.isShowingSubMenus;
        });
      }}
      formattedWalletAmount={formattedWalletAmount}
      currentRoute={`/wallets/${activeWallet.id}/${activeNavItem}`}
      activeWallet={
        activeSidebarCategory === '/wallets' && activeNavItem !== 'empty'
          ? activeWallet
          : null
      }
      showSubMenuToggle
      showSubMenus={this.isShowingSubMenus}
      leftIcon={this.isShowingSubMenus ? menuIconOpened : menuIconClosed}
      onTransferFunds={action('onTransferFunds')}
      onWalletAdd={action('onWalletAdd')}
      hasRewardsWallets={boolean('hasRewardsWallets', true)}
      isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
      isAlonzoActivated={boolean('isAlonzoActivated', false)}
    >
      <NodeSyncStatusIcon
        isSynced
        syncPercentage={100}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        isProduction
        isMainnet
        {...(boolean('hasTadaIcon', true)
          ? {
              hasTadaIcon: true,
            }
          : {})}
      />
      <span
        className={classNames(
          topBarStyles.rectangle,
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
          boolean('hasTadaIcon') && topBarStyles.hasTadaIcon
        )}
      />
      {/* @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1. */}
      <DiscreetToggleTopBar hasTadaIcon={boolean('hasTadaIcon')} />
      {/* @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1. */}
      {boolean('hasTadaIcon') && (
        <TadaButton onClick={action('onClickTadaButton')} shouldAnimate />
      )}
      <NewsFeedIcon
        onNewsFeedIconClick={action('onNewsFeedIconClick')}
        hasNotification={false}
        hasUpdate={false}
      />
    </TopBar>
  );
}

export default StoryLayout;
