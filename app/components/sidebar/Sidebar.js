// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.inline.svg';
import adaRedemptionIcon from '../../assets/images/sidebar/ada-redemption-ic.inline.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.inline.svg';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import type { SidebarWalletType } from '../../stores/SidebarStore';

@observer
export default class Sidebar extends Component {

  props: {
    menus: {
      wallets: {
        items: Array<SidebarWalletType>,
        activeWalletId: ?string,
        actions: {
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
    openDialogAction: Function,
  };

  static defaultProps = {
    isShowingSubMenus: false,
  };

  render() {
    const {
      menus, categories, activeSidebarCategory,
      isShowingSubMenus, onCategoryClicked,
      openDialogAction,
    } = this.props;
    let subMenu = null;

    if (menus && activeSidebarCategory === categories.WALLETS) {
      subMenu = (
        <SidebarWalletsMenu
          wallets={menus.wallets.items}
          onAddWallet={() => openDialogAction({
            dialog: WalletAddDialog,
          })}
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
