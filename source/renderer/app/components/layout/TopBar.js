// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import LegacyBadge from '../notifications/LegacyBadge';
import LegacyNotification from '../notifications/LegacyNotification';
import Wallet from '../../domains/Wallet';
import styles from './TopBar.scss';
import { formattedWalletAmount } from '../../utils/formatters';

type Props = {
  onLeftIconClick?: ?Function,
  leftIcon?: ?string,
  children?: ?Node,
  activeWallet?: ?Wallet,
};

@observer
export default class TopBar extends Component<Props> {
  render() {
    const { onLeftIconClick, leftIcon, activeWallet, children } = this.props;

    const topBarStyles = classNames([
      styles.topBar,
      activeWallet ? styles.withWallet : styles.withoutWallet,
    ]);

    const topBarTitle = activeWallet ? (
      <span className={styles.walletInfo}>
        <span className={styles.walletName}>{activeWallet.name}</span>
        <span className={styles.walletAmount}>
          {// show currency and use long format
          formattedWalletAmount(activeWallet.amount, true)}
        </span>
        {activeWallet && activeWallet.isLegacy && (
          <LegacyBadge
            style={{
              position: 'absolute',
              right: 0,
              top: 0,
              transform: 'translate(100%, -40%)',
            }}
          />
        )}
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
          <div className={styles.topBarTitle}>{topBarTitle}</div>
          {children}
        </div>
        {activeWallet && activeWallet.isLegacy && (
          <LegacyNotification onLearnMore={() => null} onMove={() => null} />
        )}
      </header>
    );
  }
}
