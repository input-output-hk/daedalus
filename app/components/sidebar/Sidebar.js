// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarCategory from './SidebarCategory';
import SidebarWalletsMenu from './menus/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.svg';

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
    showMenu: PropTypes.bool,
    activeWalletId: PropTypes.string
  };

  matches(path: string) {
    return this.props.route.indexOf(path) !== -1;
  }

  render() {
    const { hidden, showMenu, menus, onCategoryClicked, activeWalletId } = this.props;
    const sidebarStyles = classNames([
      styles.component,
      hidden ? styles.hidden : styles.visible,
    ]);

    let subMenu = null;

    if (showMenu && this.matches('/wallets')) {
      subMenu = (
        <SidebarWalletsMenu
          visible={this.matches('/wallets')}
          wallets={menus.wallets.items}
          onAddWallet={menus.wallets.actions.onAddWallet}
          onWalletItemClick={menus.wallets.actions.onWalletItemClick}
          isActiveWallet={id => id === activeWalletId}
        />
      );
    }
    return (
      <div className={sidebarStyles}>
        <div className={subMenu != null ? styles.minimized : styles.maximized}>
          <SidebarCategory
            label="Wallets"
            icon={walletsIcon}
            active={this.matches('/wallets')}
            minimized={subMenu != null}
            onClick={() => onCategoryClicked('/wallets')}
          />
          <SidebarCategory
            label="Settings"
            icon={settingsIcon}
            active={this.matches('/settings')}
            minimized={subMenu != null}
            onClick={() => onCategoryClicked('/settings')}
          />
        </div>
        {subMenu}
      </div>
    );
  }

}
