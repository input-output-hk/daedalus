// @flow
import React, { Component, PropTypes } from 'react';
import classNames from 'classnames';
import RTAppBar from 'react-toolbox/lib/app_bar/AppBar';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import menuIcon from '../../assets/images/menu-ic.svg';
import styles from './TopBar.scss';

@observer
export default class TopBar extends Component {

  static propTypes = {
    onToggleSidebar: PropTypes.func,
    children: PropTypes.element,
    activeWallet: MobxPropTypes.observableObject,
  };

  render() {
    const { onToggleSidebar, activeWallet } = this.props;
    const sidebarToggleIcon = onToggleSidebar && <img className={styles.sidebarIcon} src={menuIcon} role="presentation" />;
    const topBarStyles = classNames([
      !activeWallet ? styles.noWallet : null,
    ]);

    const topBarTitle = activeWallet ? (
      <div className={styles.walletInfo}>
        <div className={styles.walletName}>{activeWallet.name}</div>
        <div className={styles.walletAmount}>{activeWallet.amount + ' ' + activeWallet.currency}</div>
      </div>
    ) : null;

    return (
      <RTAppBar
        className={topBarStyles}
        leftIcon={sidebarToggleIcon}
        onLeftIconClick={onToggleSidebar}
      >
        <div className={styles.topBarTitle}>{topBarTitle}</div>
        {this.props.children}
      </RTAppBar>
    );
  }
}
