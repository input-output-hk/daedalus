// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './Sidebar.scss';
import SidebarMainItem from './SidebarMainItem';
import SidebarSubMenu from './SidebarSubMenu';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.svg';

@observer
export default class Sidebar extends Component {

  static propTypes = {
    routePath: PropTypes.string.isRequired,
    hidden: PropTypes.bool,
    showSubMenus: PropTypes.bool
  };

  matches(path: string) {
    return this.props.routePath.indexOf(path) !== -1;
  }

  render() {
    const { hidden, showSubMenus } = this.props;
    const sidebarStyles = classNames([
      styles.component,
      hidden ? styles.hidden : styles.visible,
    ]);
    const subMenus = showSubMenus ? (
      <SidebarSubMenu visible={this.matches('/wallets')}>
        <div>Add wallet</div>
      </SidebarSubMenu>
    ) : null;
    return (
      <div className={sidebarStyles}>
        <div className={showSubMenus ? styles.minimized : styles.maximized}>
          <SidebarMainItem
            label="Wallets"
            icon={walletsIcon}
            active={this.matches('/wallets')}
            minimized={showSubMenus}
          />
          <SidebarMainItem
            label="Settings"
            icon={settingsIcon}
            active={this.matches('/settings')}
            minimized={showSubMenus}
          />
        </div>
        {subMenus}
      </div>
    );
  }

}
