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
          onAddWallet: PropTypes.func.isRequired,
        })
      })
    }).isRequired,
    onCategoryClicked: PropTypes.func,
    hidden: PropTypes.bool,
    showMenu: PropTypes.bool
  };

  matches(path: string) {
    return this.props.route.indexOf(path) !== -1;
  }

  render() {
    const { hidden, showMenu, menus, onCategoryClicked } = this.props;
    const sidebarStyles = classNames([
      styles.component,
      hidden ? styles.hidden : styles.visible,
    ]);
    const subMenus = showMenu ? (
      <SidebarWalletsMenu
        visible={this.matches('/wallets')}
        wallets={menus.wallets.items}
        onAddWallet={menus.wallets.actions.onAddWallet}
        isActiveWallet={(id) => this.matches(`/wallets/${id}`)}
      />
    ) : null;
    return (
      <div className={sidebarStyles}>
        <div className={showMenu ? styles.minimized : styles.maximized}>
          <SidebarCategory
            label="Wallets"
            icon={walletsIcon}
            active={this.matches('/wallets')}
            minimized={showMenu}
            onClick={() => onCategoryClicked('/wallets')}
          />
          <SidebarCategory
            label="Settings"
            icon={settingsIcon}
            active={this.matches('/settings')}
            minimized={showMenu}
            onClick={() => onCategoryClicked('/settings')}
          />
        </div>
        {subMenus}
      </div>
    );
  }

}
