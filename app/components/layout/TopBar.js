// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import classNames from 'classnames';
import RTAppBar from 'react-toolbox/lib/app_bar/AppBar';
import { observer } from 'mobx-react';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import Wallet from '../../domain/Wallet';
import menuIcon from '../../assets/images/menu-ic.svg';
import styles from './TopBar.scss';
import { matchRoute } from '../../lib/routing-helpers';
import { ROUTES } from '../../Routes';

@observer
export default class TopBar extends Component {

  props: {
    onToggleSidebar?: ?Function,
    children?: ?Children,
    activeWallet?: ?Wallet,
    currentRoute: string,
  };

  render() {
    const { onToggleSidebar, activeWallet, currentRoute } = this.props;
    const walletRoutesMatch = matchRoute(`${ROUTES.WALLETS.ROOT}/:id(*page)`, currentRoute);
    const sidebarToggleIcon = onToggleSidebar && <img className={styles.sidebarIcon} src={menuIcon} role="presentation" />;
    const showWalletInfo = walletRoutesMatch && activeWallet != null;
    const topBarStyles = classNames([
      showWalletInfo ? styles.withWallet : styles.withoutWallet,
    ]);

    const topBarTitle = walletRoutesMatch && activeWallet != null ? (
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
        leftIcon={walletRoutesMatch ? sidebarToggleIcon : null}
        onLeftIconClick={walletRoutesMatch ? onToggleSidebar : null}
      >
        <div className={styles.topBarTitle}>{topBarTitle}</div>
        {this.props.children}
      </RTAppBar>
    );
  }
}
