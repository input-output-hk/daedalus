// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find, camelCase } from 'lodash';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarCategoryNetworkInfo from './SidebarCategoryNetworkInfo';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import InstructionsDialog from '../wallet/paper-wallet-certificate/InstructionsDialog';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig.js';
import { ROUTES } from '../../routes-config';
import type { SidebarWalletType } from '../../types/sidebarTypes';
import type { networkType } from '../../types/networkTypes';
import type { SidebarCategoryInfo } from '../../config/sidebarConfig';

type Props = {
  menus: SidebarMenus,
  categories: Array<SidebarCategoryInfo>,
  activeSidebarCategory: string,
  onCategoryClicked: Function,
  isShowingSubMenus: boolean,
  openDialogAction: Function,
  onAddWallet: Function,
  pathname: string,
  network: networkType,
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

@observer
export default class Sidebar extends Component<Props> {
  static defaultProps = {
    isShowingSubMenus: false,
  };

  get networkInfoContent() {
    return <SidebarCategoryNetworkInfo network={this.props.network} />;
  }

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
          {categories.map((category: SidebarCategoryInfo) => {
            const categoryName = camelCase(category.name);
            const content = (this: any)[`${categoryName}Content`];
            const isActive = activeSidebarCategory === category.route;
            return (
              <SidebarCategory
                key={categoryName}
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
