// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import adaRedemptionIcon from '../../assets/images/sidebar/ada.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.svg';
import type { SidebarWalletType } from '../../stores/SidebarStore';

@observer
export default class Sidebar extends Component {

  props: {
    menus: {
      wallets: {
        items: Array<SidebarWalletType>,
        activeWalletId: ?string,
        actions: {
          onAddWallet: Function,
          onWalletItemClick: Function,
        }
      }
    },
    categories: {
      WALLETS: string,
      ADA_REDEMPTION: string,
      SETTINGS: string,
    },
    activeSidebarCategory: string,
    onCategoryClicked: Function,
    isShowingSubMenus: boolean,
  };

  static defaultProps = {
    isShowingSubMenus: false,
  };

  render() {
    const {
      menus, categories, activeSidebarCategory,
      isShowingSubMenus, onCategoryClicked
    } = this.props;

    let subMenu = null;

    if (menus && activeSidebarCategory === categories.WALLETS) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets.items}
          onAddWallet={menus.wallets.actions.onAddWallet}
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
          <SidebarCategory
            className="wallets"
            icon={walletsIcon}
            active={activeSidebarCategory === categories.WALLETS}
            onClick={() => onCategoryClicked(categories.WALLETS)}
          />
          <SidebarCategory
            className="ada-redemption"
            icon={adaRedemptionIcon}
            active={activeSidebarCategory === categories.ADA_REDEMPTION}
            onClick={() => onCategoryClicked(categories.ADA_REDEMPTION)}
          />
          <SidebarCategory
            className="settings"
            icon={settingsIcon}
            active={activeSidebarCategory === categories.SETTINGS}
            onClick={() => onCategoryClicked(categories.SETTINGS)}
          />

        </div>
        {subMenu}
      </div>
    );
  }

}
