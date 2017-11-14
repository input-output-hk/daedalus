// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import Wallet from '../../domain/Wallet';
import menuIconOpened from '../../assets/images/menu-opened-ic.inline.svg';
import menuIconClosed from '../../assets/images/menu-ic.inline.svg';
import styles from './TopBar.scss';
import resolver from '../../utils/imports';
import { matchRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';

const { formattedWalletAmount } = resolver('utils/formatters');

type Props = {
  onToggleSidebar?: ?Function,
  children?: ?Node,
  activeWallet?: ?Wallet,
  currentRoute: string,
  showSubMenus?: ?boolean,
};

@observer
export default class TopBar extends Component<Props> {

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
          {
            // show currency and use long format (e.g. in ETC show all decimal places)
            formattedWalletAmount(activeWallet.amount, true, true)
          }
        </div>
      </div>
    ) : null;

    const sidebarToggleIcon = (
      <SvgInline
        svg={showSubMenus ? menuIconOpened : menuIconClosed}
        className={styles.sidebarIcon}
      />
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
