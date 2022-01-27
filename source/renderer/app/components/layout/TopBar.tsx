import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { IS_BYRON_WALLET_MIGRATION_ENABLED } from '../../config/walletsConfig';
import LegacyBadge, { LEGACY_BADGE_MODES } from '../notifications/LegacyBadge';
import LegacyNotification from '../notifications/LegacyNotification';
import Wallet from '../../domains/Wallet';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TopBar.scss' or its correspo... Remove this comment to see the full error message
import styles from './TopBar.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/header-log... Remove this comment to see the full error message
import headerLogo from '../../assets/images/header-logo.inline.svg';
import { DiscreetWalletAmount } from '../../features/discreet-mode';

type Props = {
  onLeftIconClick?: ((...args: Array<any>) => any) | null | undefined;
  leftIcon?: string | null | undefined;
  children?: Node | null | undefined;
  activeWallet?: Wallet | null | undefined;
  onTransferFunds?: (...args: Array<any>) => any;
  onWalletAdd?: (...args: Array<any>) => any;
  hasRewardsWallets?: boolean;
  onLearnMore?: (...args: Array<any>) => any;
  isShelleyActivated: boolean;
};

@observer
class TopBar extends Component<Props> {
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
          {
            // show currency and use long format
            isRestoreActive ? (
              '-'
            ) : (
              <DiscreetWalletAmount amount={activeWallet.amount} />
            )
          }
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

export default TopBar;
