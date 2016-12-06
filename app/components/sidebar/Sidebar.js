// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './menus/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.svg';
import stakingIcon from '../../assets/images/sidebar/staking-ic.svg';

@observer
export default class Sidebar extends Component {

  static propTypes = {
    route: PropTypes.string.isRequired,
    menus: PropTypes.shape({
      wallets: PropTypes.shape({
        items: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.object).isRequired,
        actions: PropTypes.shape({
          onAddWallet: PropTypes.func,
          onWalletItemClick: PropTypes.func
        })
      })
    }).isRequired,
    onCategoryClicked: PropTypes.func,
    hidden: PropTypes.bool,
    isMaximized: PropTypes.bool,
    activeWalletId: PropTypes.string
  };

  matches(path: string) {
    return this.props.route.indexOf(path) !== -1;
  }

  render() {
    const { hidden, isMaximized, menus, onCategoryClicked, activeWalletId } = this.props;

    let sidebarStyle = null;
    let categoriesStyle = null;
    let subMenu = null;
    let hasMinimizedCategories = true;

    if (hidden) {
      sidebarStyle = styles.hidden;
    } else if (isMaximized) {
      categoriesStyle = styles.maximized;
      hasMinimizedCategories = false;
    } else if (this.matches('/wallets')) {
      subMenu = (
        <SidebarWalletsMenu
          visible={this.matches('/wallets')}
          wallets={menus.wallets.items}
          onAddWallet={menus.wallets.actions.onAddWallet}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === activeWalletId}
        />
      );
      categoriesStyle = styles.minimized;
    } else {
      sidebarStyle = styles.minimized;
    }

    const sidebarStyles = classNames([styles.component, sidebarStyle]);

    return (
      <div className={sidebarStyles}>
        <div className={categoriesStyle}>
          <SidebarCategory
            className="wallets"
            label="Wallets"
            icon={walletsIcon}
            active={this.matches('/wallets')}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked('/wallets')}
          />
          <SidebarCategory
            className="settings"
            label="Settings"
            icon={settingsIcon}
            active={this.matches('/settings')}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked('/settings')}
          />
          <SidebarCategory
            className="staking"
            label="Staking"
            icon={stakingIcon}
            active={this.matches('/staking')}
            minimized={hasMinimizedCategories}
            onClick={() => onCategoryClicked('/staking')}
          />
        </div>
        {subMenu}
      </div>
    );
  }

}
