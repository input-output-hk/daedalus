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
import { matchRoute } from '../../lib/routing-helpers';
import { ROUTES } from '../../Routes';

@observer
export default class TopBar extends Component {

  static propTypes = {
    onToggleSidebar: PropTypes.func,
    children: oneOrManyChildElements,
    activeWallet: PropTypes.instanceOf(Wallet),
    currentRoute: PropTypes.string.isRequired,
  };

  render() {
    const { onToggleSidebar, activeWallet, currentRoute } = this.props;
    const walletRoutesMatch = matchRoute(`${ROUTES.WALLETS.ROOT}/:id(*page)`, currentRoute);
    const sidebarToggleIcon = onToggleSidebar && <img className={styles.sidebarIcon} src={menuIcon} role="presentation" />;
    const showWalletInfo = activeWallet && walletRoutesMatch;
    const topBarStyles = classNames([
      showWalletInfo ? styles.withWallet : styles.withoutWallet,
    ]);

    const topBarTitle = showWalletInfo ? (
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
