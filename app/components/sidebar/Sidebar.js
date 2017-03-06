// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './wallets/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
// import adaRedemptionIcon from '../../assets/images/sidebar/ada.svg';

@observer
export default class Sidebar extends Component {

  static propTypes = {
    menus: PropTypes.shape({
      wallets: PropTypes.shape({
        items: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.object).isRequired,
        actions: PropTypes.shape({
          onAddWallet: PropTypes.func,
          onWalletItemClick: PropTypes.func
        })
      })
    }).isRequired,
    categories: PropTypes.shape({
      WALLETS: PropTypes.string.isRequired,
      ADA_REDEMPTION: PropTypes.string.isRequired,
    }).isRequired,
    currentCategory: PropTypes.string,
    onCategoryClicked: PropTypes.func, // TODO: temporary disabled
    isShowingSubMenus: PropTypes.bool.isRequired,
    activeWalletId: PropTypes.string,
  };

  render() {
    const {
      menus, activeWalletId, categories, currentCategory,
      isShowingSubMenus, onCategoryClicked
    } = this.props;

    let subMenu = null;

    switch (currentCategory) {
      case categories.WALLETS:
        subMenu = (
          <SidebarWalletsMenu
            wallets={menus.wallets.items}
            onAddWallet={menus.wallets.actions.onAddWallet}
            onWalletItemClick={menus.wallets.actions.onWalletItemClick}
            isActiveWallet={id => id === activeWalletId}
            visible
          />
        );
        break;
      default:
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
            active={currentCategory === categories.WALLETS}
            onClick={() => onCategoryClicked(categories.WALLETS)}
          />
          {/*<SidebarCategory*/}
            {/*className="ada-redemption"*/}
            {/*icon={adaRedemptionIcon}*/}
            {/*active={currentCategory === categories.ADA_REDEMPTION}*/}
            {/*onClick={() => onCategoryClicked(categories.ADA_REDEMPTION)}*/}
          {/*/>*/}
        </div>
        {subMenu}
      </div>
    );
  }

}
