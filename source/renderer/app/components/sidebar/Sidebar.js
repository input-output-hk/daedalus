// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find } from 'lodash';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import InstructionsDialog from '../wallet/paper-wallet-certificate/InstructionsDialog';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig.js';
import { ROUTES } from '../../routes-config';
import type { SidebarWalletType } from '../../types/sidebarTypes';

type Props = {
  menus: SidebarMenus,
  categories: SidebarCategories,
  activeSidebarCategory: string,
  onCategoryClicked: Function,
  isShowingSubMenus: boolean,
  openDialogAction: Function,
  onAddWallet: Function,
  pathname: string,
};

export type SidebarMenus = ?{
  wallets: {
    items: Array<SidebarWalletType>,
    activeWalletId: ?string,
    actions: {
      onWalletItemClick: Function,
    },
  },
};

export type SidebarCategories = Array<{
  name: string,
  route: string,
  icon: string,
}>;

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
    } = this.props;
    let subMenu = null;

    const walletsCategory = find(categories, {
      name: CATEGORIES_BY_NAME.WALLETS.name,
    }).route;

    if (menus && activeSidebarCategory === walletsCategory) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets.items}
          onAddWallet={onAddWallet}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === menus.wallets.activeWalletId}
          isAddWalletButtonActive={pathname === '/wallets/add'}
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
          {categories.map((category: Category) => {
            return (
              <SidebarCategory
                key={category.name}
                category={category}
                isActive={activeSidebarCategory === category.route}
                onClick={this.handleClick}
              />
            );
          })}
        </div>
        {subMenu}
      </div>
    );
  }

  handleClick = (categoryRoute: string) => {
    if (categoryRoute === ROUTES.PAPER_WALLET_CREATE_CERTIFICATE) {
      this.props.openDialogAction({
        dialog: InstructionsDialog,
      });
    } else {
      this.props.onCategoryClicked(categoryRoute);
    }
  };
}
