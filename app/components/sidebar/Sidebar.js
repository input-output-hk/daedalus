// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarMainItem from './SidebarCategory';
import SidebarWalletsMenu from './menus/SidebarWalletsMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.svg';

@observer
export default class Sidebar extends Component {

  static propTypes = {
    routePath: PropTypes.string.isRequired,
    menus: PropTypes.shape({
      wallets: PropTypes.shape({
        items: PropTypes.arrayOf(PropTypes.object).isRequired,
        actions: PropTypes.shape({
          onAddWallet: PropTypes.func.isRequired,
        })
      })
    }).isRequired,
    hidden: PropTypes.bool,
    showMenus: PropTypes.bool
  };

  matches(path: string) {
    return this.props.routePath.indexOf(path) !== -1;
  }

  render() {
    const { hidden, showMenus, menus } = this.props;
    const sidebarStyles = classNames([
      styles.component,
      hidden ? styles.hidden : styles.visible,
    ]);
    const subMenus = showMenus ? (
      <SidebarWalletsMenu
        visible={this.matches('/wallets')}
        wallets={menus.wallets.items}
        onAddWallet={menus.wallets.actions.onAddWallet}
        isActiveWallet={(id) => this.matches(`/wallets/${id}`)}
      />
    ) : null;
    return (
      <div className={sidebarStyles}>
        <div className={showMenus ? styles.minimized : styles.maximized}>
          <SidebarMainItem
            label="Wallets"
            icon={walletsIcon}
            active={this.matches('/wallets')}
            minimized={showMenus}
          />
          <SidebarMainItem
            label="Settings"
            icon={settingsIcon}
            active={this.matches('/settings')}
            minimized={showMenus}
          />
        </div>
        {subMenus}
      </div>
    );
  }

}
