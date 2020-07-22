// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import LegacyBadge, {
  LEGACY_BADGE_MODES,
} from '../../notifications/LegacyBadge';
import ProgressBar from '../../widgets/ProgressBar';
import styles from './SidebarWalletMenuItem.scss';
import disconnectedIcon from '../../../assets/images/hardware-wallet/disconnected.inline.svg';

type Props = {
  title: string,
  info: string,
  active: boolean,
  className: string,
  onClick: Function,
  isRestoreActive?: boolean,
  isIncentivizedTestnet: boolean,
  isShelleyTestnet: boolean,
  isShelleyActivated: boolean,
  restoreProgress?: number,
  isLegacy: boolean,
  isNotResponding: boolean,
  hasNotification: boolean,
  isHardwareWalletsMenu?: boolean,
};

@observer
export default class SidebarWalletMenuItem extends Component<Props> {
  render() {
    const {
      title,
      info,
      active,
      className,
      onClick,
      isRestoreActive,
      isIncentivizedTestnet,
      isShelleyTestnet,
      isShelleyActivated,
      restoreProgress,
      isLegacy,
      isNotResponding,
      hasNotification,
      isHardwareWalletsMenu,
    } = this.props;

    const showLegacyBadge =
      isLegacy &&
      ((isIncentivizedTestnet && !isShelleyTestnet) || isShelleyActivated);

    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      showLegacyBadge ? styles.legacyItem : null,
      className,
      !isIncentivizedTestnet && hasNotification ? styles.notification : null,
      isNotResponding ? styles.notResponding : null,
    ]);

    return (
      <button className={componentStyles} onClick={onClick}>
        <div className={styles.meta}>
          <div className={styles.topContainer}>
            <div className={styles.title}>{title}</div>
            {isHardwareWalletsMenu && (
              <SVGInline
                svg={disconnectedIcon}
                className={styles.disconnectedIcon}
              />
            )}
          </div>
          <div className={styles.info}>{info}</div>
          {isRestoreActive ? <ProgressBar progress={restoreProgress} /> : null}
          {showLegacyBadge && (
            <LegacyBadge mode={LEGACY_BADGE_MODES.FLOATING} />
          )}
        </div>
      </button>
    );
  }
}
