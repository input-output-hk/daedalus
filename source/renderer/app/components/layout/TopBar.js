// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import LegacyBadge, { LEGACY_BADGE_MODES } from '../notifications/LegacyBadge';
import LegacyNotification from '../notifications/LegacyNotification';
import Wallet from '../../domains/Wallet';
import styles from './TopBar.scss';
import { formattedWalletAmount } from '../../utils/formatters';
import headerLogo from '../../assets/images/header-logo.inline.svg';

type Props = {
  onLeftIconClick?: ?Function,
  leftIcon?: ?string,
  children?: ?Node,
  activeWallet?: ?Wallet,
  onTransferFunds?: Function,
  onWalletAdd?: Function,
  hasAnyWallets?: boolean,
};

@observer
export default class TopBar extends Component<Props> {
  render() {
    const {
      onLeftIconClick,
      leftIcon,
      activeWallet,
      children,
      onTransferFunds,
      hasAnyWallets,
      onWalletAdd,
    } = this.props;

    const topBarStyles = classNames([
      styles.topBar,
      activeWallet ? styles.withWallet : styles.withoutWallet,
    ]);

    const hasLegacyNotification =
      activeWallet &&
      activeWallet.isLegacy &&
      activeWallet.amount > 0 &&
      ((hasAnyWallets && onTransferFunds) || onWalletAdd);

    const topBarTitle = activeWallet ? (
      <span className={styles.walletInfo}>
        <span className={styles.walletName}>
          {activeWallet.name}
          {activeWallet.isLegacy && (
            <LegacyBadge mode={LEGACY_BADGE_MODES.NATURAL} />
          )}
        </span>
        <span className={styles.walletAmount}>
          {// show currency and use long format
          formattedWalletAmount(activeWallet.amount, true)}
        </span>
      </span>
    ) : null;

    const leftIconSVG = leftIcon && (
      <SVGInline svg={leftIcon} className={styles.sidebarIcon} />
    );

    return (
      <header>
        <div className={topBarStyles}>
          {leftIcon && (
            <button className={styles.leftIcon} onClick={onLeftIconClick}>
              {leftIconSVG}
            </button>
          )}
          {activeWallet ? (
            <div className={styles.topBarTitle}>{topBarTitle}</div>
          ) : (
            <SVGInline svg={headerLogo} className={styles.headerLogo} />
          )}
          {children}
        </div>
        {hasLegacyNotification && (
          <LegacyNotification
            onLearnMore={() => null}
            onTransferFunds={() => onTransferFunds(activeWallet.id)}
            hasAnyWallets={hasAnyWallets}
            onWalletAdd={onWalletAdd}
          />
        )}
      </header>
    );
  }
}
