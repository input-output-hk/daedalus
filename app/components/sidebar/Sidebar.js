// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import adaRedemptionIcon from '../../assets/images/sidebar/ada.svg';

@observer
export default class Sidebar extends Component {

  static propTypes = {
    menus: PropTypes.shape({
      wallets: PropTypes.shape({
        items: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.object).isRequired,
        activeWalletId: PropTypes.string,
        actions: PropTypes.shape({
          onAddWallet: PropTypes.func,
          onWalletItemClick: PropTypes.func
        })
      })
    }),
    categories: PropTypes.shape({
      WALLETS: PropTypes.string.isRequired,
      ADA_REDEMPTION: PropTypes.string.isRequired,
    }),
    activeSidebarCategory: PropTypes.string,
    onCategoryClicked: PropTypes.func,
    isShowingSubMenus: PropTypes.bool,
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
        </div>
        {subMenu}
      </div>
    );
  }

}
