// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { find, kebabCase } from 'lodash';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import InstructionsDialog from '../wallet/paper-wallet-certificate/InstructionsDialog';
import supportIcon from '../../assets/images/sidebar/bug-report-ic.inline.svg';
import type { SidebarWalletType } from '../../types/sidebarTypes';
import { ROUTES } from '../../routes-config';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig.js';

type Props = {
  menus: SidebarMenus,
  categories: SidebarCategories,
  activeSidebarCategory: string,
  onCategoryClicked: Function,
  isShowingSubMenus: boolean,
  openDialogAction: Function,
  onAddWallet: Function,
  onSubmitSupportRequest: Function,
};

export type SidebarMenus = ?{
  wallets: {
    items: Array<SidebarWalletType>,
    activeWalletId: ?string,
    actions: {
      onWalletItemClick: Function,
    }
  }
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
      menus, categories, activeSidebarCategory,
      isShowingSubMenus, onAddWallet, onSubmitSupportRequest,
    } = this.props;
    let subMenu = null;

    const walletsCategory = find(categories, {
      name: CATEGORIES_BY_NAME.WALLETS.name
    }).route;

    if (menus && activeSidebarCategory === walletsCategory) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets.items}
          onAddWallet={onAddWallet}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === menus.wallets.activeWalletId}
          visible={isShowingSubMenus}
        />
      );
    }

    const sidebarStyles = classNames([
      styles.component,
      !isShowingSubMenus || subMenu == null ? styles.minimized : null
    ]);

    return (
      <div className={sidebarStyles}>
        <div className={styles.minimized}>
          {categories.map((category, index) => {
            const categoryClassName = kebabCase(category.name);
            return (
              <SidebarCategory
                key={index}
                className={categoryClassName}
                icon={category.icon}
                active={activeSidebarCategory === category.route}
                onClick={() => this.handleClick(category.route)}
              />
            );
          })}

          <SidebarCategory
            className="supportRequest"
            icon={supportIcon}
            active={false}
            onClick={onSubmitSupportRequest}
          />
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
