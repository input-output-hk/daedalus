// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find } from 'lodash';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarCategoryNetworkInfo from './SidebarCategoryNetworkInfo';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig.js';
import { ROUTES } from '../../routes-config';
import type {
  SidebarHardwareWalletType,
  SidebarWalletType,
} from '../../types/sidebarTypes';
import type { networkType } from '../../types/networkTypes';
import type { SidebarCategoryInfo } from '../../config/sidebarConfig';

type Props = {
  menus: SidebarMenus,
  categories: Array<SidebarCategoryInfo>,
  activeSidebarCategory: string,
  isShowingSubMenus: boolean,
  pathname: string,
  network: networkType,
  onActivateCategory: Function,
  onAddWallet: Function,
  isIncentivizedTestnet: boolean,
};

export type SidebarMenus = {
  wallets: ?{
    items: Array<SidebarWalletType>,
    activeWalletId: ?string,
    actions: {
      onWalletItemClick: Function,
    },
  },
  hardwareWallets: ?{
    items: Array<SidebarHardwareWalletType>,
    activeWalletId: ?string,
    actions: {
      onHardwareWalletItemClick: Function,
    },
  },
};

@observer
export default class Sidebar extends Component<Props> {
  static defaultProps = {
    isShowingSubMenus: false,
  };

  render() {
    const {
      menus,
      categories,
      activeSidebarCategory,
      pathname,
      isShowingSubMenus,
      onAddWallet,
      isIncentivizedTestnet,
      onActivateCategory,
    } = this.props;

    let subMenu = null;

    const walletsCategory = find(categories, {
      name: CATEGORIES_BY_NAME.WALLETS.name,
    });
    const walletsCategoryRoute = walletsCategory ? walletsCategory.route : null;

    const hardwareWalletsCategory =
      menus &&
      menus.hardwareWallets &&
      find(categories, {
        name: CATEGORIES_BY_NAME.HARDWARE_WALLETS.name,
      });
    const hardwareWalletsCategoryRoute = hardwareWalletsCategory
      ? hardwareWalletsCategory.route
      : null;

    if (
      menus &&
      menus.wallets &&
      menus.wallets.items &&
      activeSidebarCategory === walletsCategoryRoute
    ) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets ? menus.wallets.items : []}
          onAddWallet={onAddWallet}
          onWalletItemClick={
            menus.wallets && menus.wallets.actions
              ? menus.wallets.actions.onWalletItemClick
              : null
          }
          isActiveWallet={id =>
            id === (menus.wallets ? menus.wallets.activeWalletId : null)
          }
          isAddWalletButtonActive={pathname === ROUTES.WALLETS.ADD}
          isIncentivizedTestnet={isIncentivizedTestnet}
          visible={isShowingSubMenus}
        />
      );
    }

    if (
      menus &&
      menus.hardwareWallets &&
      menus.hardwareWallets.items &&
      activeSidebarCategory === hardwareWalletsCategoryRoute
    ) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.hardwareWallets ? menus.hardwareWallets.items : []}
          onAddWallet={onAddWallet}
          onWalletItemClick={
            menus.hardwareWallets && menus.hardwareWallets.actions
              ? menus.hardwareWallets.actions.onHardwareWalletItemClick
              : null
          }
          isActiveWallet={id =>
            id ===
            (menus.hardwareWallets
              ? menus.hardwareWallets.activeWalletId
              : null)
          }
          isHardwareWalletsMenu
          isAddWalletButtonActive={pathname === ROUTES.HARDWARE_WALLETS.ADD}
          isIncentivizedTestnet={isIncentivizedTestnet}
          visible={isShowingSubMenus}
        />
      );
    }

    const sidebarStyles = classNames([
      styles.component,
      !isShowingSubMenus || subMenu == null ? styles.minimized : null,
    ]);

    return (
      <div className={sidebarStyles}>
        <div className={styles.minimized}>
          {categories.map((category: SidebarCategoryInfo) => {
            const content = this.getCategoryContent(category.name);
            const isActive = activeSidebarCategory === category.route;
            return (
              <SidebarCategory
                key={category.name}
                category={category}
                isActive={isActive}
                onClick={onActivateCategory}
                content={content}
              />
            );
          })}
        </div>
        {subMenu}
      </div>
    );
  }

  getCategoryContent = (categoryName: string) => {
    if (categoryName === 'NETWORK_INFO') {
      return <SidebarCategoryNetworkInfo network={this.props.network} />;
    }
    return null;
  };
}
