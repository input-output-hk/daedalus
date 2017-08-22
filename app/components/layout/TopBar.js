// @flow
import React, { Component } from 'react';
import SvgInline  from 'react-svg-inline';
import type { Children } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import Wallet from '../../domain/Wallet';
import menuIconOpened from '../../assets/images/menu-opened-ic.inline.svg';
import menuIconClosed from '../../assets/images/menu-ic.inline.svg';
import styles from './TopBar.scss';
import { matchRoute } from '../../lib/routing-helpers';
import { ROUTES } from '../../routes-config';

@observer
export default class TopBar extends Component {

  props: {
    onToggleSidebar?: ?Function,
    children?: ?Children,
    activeWallet?: ?Wallet,
    currentRoute: string,
    showSubMenus?: ?boolean,
  };

  render() {
    const { onToggleSidebar, activeWallet, currentRoute, showSubMenus } = this.props;
    const walletRoutesMatch = matchRoute(`${ROUTES.WALLETS.ROOT}/:id(*page)`, currentRoute);
    const showWalletInfo = walletRoutesMatch && activeWallet != null;
    const topBarStyles = classNames([
      styles.topBar,
      showWalletInfo ? styles.withWallet : styles.withoutWallet,
    ]);

    const topBarTitle = walletRoutesMatch && activeWallet != null ? (
      <div className={styles.walletInfo}>
        <div className={styles.walletName}>{activeWallet.name}</div>
        <div className={styles.walletAmount}>
          {activeWallet.amount.toFormat(DECIMAL_PLACES_IN_ADA) + ' ADA'}
        </div>
      </div>
    ) : null;

    const sidebarToggleIcon = (
      <SvgInline svg={showSubMenus ? menuIconOpened : menuIconClosed} className={styles.sidebarIcon} />
    );

    return (
      <header className={topBarStyles}>
        {walletRoutesMatch && (
          <button className={styles.leftIcon} onClick={onToggleSidebar}>
            {sidebarToggleIcon}
          </button>
        )}
        <div className={styles.topBarTitle}>{topBarTitle}</div>
        {this.props.children}
      </header>
    );
  }
}
