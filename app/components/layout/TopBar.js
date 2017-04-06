// @flow
import React, { Component, PropTypes } from 'react';
import classNames from 'classnames';
import RTAppBar from 'react-toolbox/lib/app_bar/AppBar';
import { observer } from 'mobx-react';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import Wallet from '../../domain/Wallet';
import menuIcon from '../../assets/images/menu-ic.svg';
import { oneOrManyChildElements } from '../../propTypes';
import styles from './TopBar.scss';

@observer
export default class TopBar extends Component {

  static propTypes = {
    onToggleSidebar: PropTypes.func,
    children: oneOrManyChildElements,
    activeWallet: PropTypes.instanceOf(Wallet),
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
        <div className={styles.walletAmount}>
          {activeWallet.amount.toFormat(DECIMAL_PLACES_IN_ADA) + ' ' + activeWallet.currency}
        </div>
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
