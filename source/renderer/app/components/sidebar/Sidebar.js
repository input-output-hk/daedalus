// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find } from 'lodash';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarCategoryNetworkInfo from './SidebarCategoryNetworkInfo';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import InstructionsDialog from '../wallet/paper-wallet-certificate/InstructionsDialog';
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
  onOpenDialog: Function,
  onAddWallet: Function,
  onOpenSplashNetwork?: Function,
  isIncentivizedTestnet: boolean,
};

export type SidebarMenus = {
  nonHardwareWallets: ?{
    wallets: {
      items: Array<SidebarWalletType>,
      activeWalletId: ?string,
      actions: {
        onWalletItemClick: Function,
      },
    },
  },
  hardwareWallets: ?{
    wallets: {
      items: Array<SidebarHardwareWalletType>,
      activeWalletId: ?string,
      actions: {
        onHardwareWalletItemClick: Function,
      },
    },
  },
};

@observer
export default class Sidebar extends Component<Props> {
  static defaultProps = {
    isShowingSubMenus: false,
    onOpenSplashNetwork: () => null,
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
    } = this.props;

    let subMenu = null;

    const walletsCategory = find(categories, {
      name: CATEGORIES_BY_NAME.WALLETS.name,
    }).route;

    const hardwareWalletsCategory = find(categories, {
      name: CATEGORIES_BY_NAME.HARDWARE_WALLETS.name,
    }).route;

    if (
      menus &&
      menus.nonHardwareWallets &&
      menus.nonHardwareWallets.wallets &&
      activeSidebarCategory === walletsCategory
    ) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={
            menus.nonHardwareWallets && menus.nonHardwareWallets.wallets
              ? menus.nonHardwareWallets.wallets.items
              : []
          }
          onAddWallet={onAddWallet}
          onWalletItemClick={
            menus.nonHardwareWallets && menus.nonHardwareWallets.wallets
              ? menus.nonHardwareWallets.wallets.actions.onWalletItemClick
              : null
          }
          isActiveWallet={id =>
            id ===
            (menus.nonHardwareWallets && menus.nonHardwareWallets.wallets
              ? menus.nonHardwareWallets.wallets.activeWalletId
              : null)
          }
          isAddWalletButtonActive={pathname === '/wallets/add'}
          isIncentivizedTestnet={isIncentivizedTestnet}
          visible={isShowingSubMenus}
        />
      );
    }

    if (
      menus &&
      menus.hardwareWallets &&
      activeSidebarCategory === hardwareWalletsCategory
    ) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={
            menus.hardwareWallets && menus.hardwareWallets.wallets
              ? menus.hardwareWallets.wallets.items
              : []
          }
          onAddWallet={onAddWallet}
          onWalletItemClick={
            menus.hardwareWallets && menus.hardwareWallets.wallets
              ? menus.hardwareWallets.wallets.actions.onHardwareWalletItemClick
              : null
          }
          isActiveWallet={id =>
            id ===
            (menus.hardwareWallets && menus.hardwareWallets.wallets
              ? menus.hardwareWallets.wallets.activeWalletId
              : null)
          }
          isAddWalletButtonActive={pathname === '/hardware-wallets/add'}
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
                onClick={this.handleClick}
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

  handleClick = (categoryRoute: string) => {
    const {
      onActivateCategory,
      onOpenDialog,
      onOpenSplashNetwork,
    } = this.props;
    if (categoryRoute === ROUTES.PAPER_WALLET_CREATE_CERTIFICATE) {
      onOpenDialog(InstructionsDialog);
    } else if (categoryRoute === ROUTES.NETWORK_INFO) {
      if (onOpenSplashNetwork) {
        onOpenSplashNetwork();
      }
    } else {
      onActivateCategory(categoryRoute);
    }
  };
}
