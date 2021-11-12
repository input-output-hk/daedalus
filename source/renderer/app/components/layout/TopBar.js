// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { IS_BYRON_WALLET_MIGRATION_ENABLED } from '../../config/walletsConfig';
import LegacyBadge, { LEGACY_BADGE_MODES } from '../notifications/LegacyBadge';
import LegacyNotification from '../notifications/LegacyNotification';
import Wallet from '../../domains/Wallet';
import styles from './TopBar.scss';
import { formattedWalletAmount } from '../../utils/formatters';
import headerLogo from '../../assets/images/header-logo.inline.svg';
import { DiscreetValue } from '../../features/discreet-mode';

type Props = {
  onLeftIconClick?: ?Function,
  leftIcon?: ?string,
  children?: ?Node,
  activeWallet?: ?Wallet,
  onTransferFunds?: Function,
  onWalletAdd?: Function,
  hasRewardsWallets?: boolean,
  onLearnMore?: Function,
  isShelleyActivated: boolean,
};

@observer
export default class TopBar extends Component<Props> {
  render() {
    const {
      onLeftIconClick,
      leftIcon,
      activeWallet,
      children,
      hasRewardsWallets,
      onTransferFunds,
      onWalletAdd,
      onLearnMore,
      isShelleyActivated,
    } = this.props;
    const topBarStyles = classNames([
      styles.topBar,
      activeWallet ? styles.withWallet : styles.withoutWallet,
    ]);

    const hasLegacyNotification =
      activeWallet &&
      activeWallet.isLegacy &&
      isShelleyActivated &&
      activeWallet.amount.gt(0) &&
      !activeWallet.isRestoring &&
      ((hasRewardsWallets && onTransferFunds) || onWalletAdd);

    const onTransferFundsFn =
      onTransferFunds && activeWallet
        ? () => onTransferFunds(activeWallet.id)
        : () => {};

    const isRestoreActive = activeWallet ? activeWallet.isRestoring : false;

    const topBarTitle = activeWallet ? (
      <span className={styles.walletInfo}>
        <span className={styles.walletName}>
          {activeWallet.name}
          {activeWallet.isLegacy && (
            <LegacyBadge mode={LEGACY_BADGE_MODES.NATURAL} />
          )}
        </span>
        <span className={styles.walletAmount}>
          <DiscreetValue ticker={{ show: true }}>
            {
              // show currency and use long format
              isRestoreActive ? '-' : formattedWalletAmount(activeWallet.amount)
            }
          </DiscreetValue>
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
        {IS_BYRON_WALLET_MIGRATION_ENABLED &&
          hasLegacyNotification &&
          activeWallet && (
            <LegacyNotification
              activeWalletName={activeWallet.name}
              onLearnMore={onLearnMore}
              onTransferFunds={onTransferFundsFn}
              hasRewardsWallets={hasRewardsWallets}
              onWalletAdd={onWalletAdd}
            />
          )}
      </header>
    );
  }
}
