import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import LegacyBadge, {
  LEGACY_BADGE_MODES,
} from '../../notifications/LegacyBadge';
import ProgressBar from '../../widgets/ProgressBar';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SidebarWalletMenuItem.scss' ... Remove this comment to see the full error message
import styles from './SidebarWalletMenuItem.scss';
import { isHardwareWalletIndicatorEnabled } from '../../../config/hardwareWalletsConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hardwar... Remove this comment to see the full error message
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import { DiscreetWalletAmount } from '../../../features/discreet-mode';

type Props = {
  title: string;
  amount: number;
  active: boolean;
  className: string;
  onClick: (...args: Array<any>) => any;
  isRestoreActive?: boolean;
  isShelleyActivated: boolean;
  restoreProgress?: number;
  isLegacy: boolean;
  isNotResponding: boolean;
  hasNotification: boolean;
  isHardwareWalletDisconnected?: boolean;
  isHardwareWallet: boolean;
};

@observer
class SidebarWalletMenuItem extends Component<Props> {
  render() {
    const {
      title,
      amount,
      active,
      className,
      onClick,
      isRestoreActive,
      isShelleyActivated,
      restoreProgress,
      isLegacy,
      isNotResponding,
      hasNotification,
      isHardwareWalletDisconnected,
      isHardwareWallet,
    } = this.props;
    const showLegacyBadge = isLegacy && isShelleyActivated;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      showLegacyBadge ? styles.legacyItem : null,
      className,
      hasNotification ? styles.notification : null,
      isNotResponding ? styles.notResponding : null,
    ]);
    const hwIconStyles = classNames([
      styles.hardwareWalletsIcon,
      isHardwareWallet &&
      isHardwareWalletDisconnected &&
      isHardwareWalletIndicatorEnabled
        ? styles.disconnected
        : styles.connected,
    ]);
    return (
      <button className={componentStyles} onClick={onClick}>
        <div className={styles.meta}>
          <div className={styles.topContainer}>
            <div className={styles.title}>{title}</div>
            {isHardwareWallet && (
              <div className={styles.hardwareWalletsIconWrapper}>
                <SVGInline svg={hardwareWalletsIcon} className={hwIconStyles} />
              </div>
            )}
          </div>
          <div className={styles.info}>
            {isRestoreActive ? (
              '-'
            ) : (
              // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
              <DiscreetWalletAmount amount={amount} withCurrency long={false} />
            )}
          </div>
          {isRestoreActive ? <ProgressBar progress={restoreProgress} /> : null}
          {showLegacyBadge && (
            <LegacyBadge mode={LEGACY_BADGE_MODES.FLOATING} />
          )}
        </div>
      </button>
    );
  }
}

export default SidebarWalletMenuItem;
