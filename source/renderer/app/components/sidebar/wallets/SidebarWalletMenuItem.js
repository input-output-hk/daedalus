// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import LegacyBadge, {
  LEGACY_BADGE_MODES,
} from '../../notifications/LegacyBadge';
import ProgressBar from '../../widgets/ProgressBar';
import styles from './SidebarWalletMenuItem.scss';

type Props = {
  title: string,
  info: string,
  active: boolean,
  className: string,
  onClick: Function,
  isRestoreActive?: boolean,
  isIncentivizedTestnet: boolean,
  restoreProgress?: number,
  isLegacy: boolean,
  isNotResponding: boolean,
  recoveryPhraseVerificationStatus: string,
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
      restoreProgress,
      isLegacy,
      isNotResponding,
      recoveryPhraseVerificationStatus,
    } = this.props;

    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      isLegacy && isIncentivizedTestnet ? styles.legacyItem : null,
      className,
      !isIncentivizedTestnet
        ? styles[
            `recoveryPhraseVerificationStatus-${recoveryPhraseVerificationStatus}`
          ]
        : null,
      isNotResponding ? styles.notResponding : null,
    ]);

    return (
      <button className={componentStyles} onClick={onClick}>
        <div className={styles.meta}>
          <div className={styles.title}>{title}</div>
          <div className={styles.info}>{info}</div>
          {isRestoreActive ? <ProgressBar progress={restoreProgress} /> : null}
          {isLegacy && isIncentivizedTestnet && (
            <LegacyBadge mode={LEGACY_BADGE_MODES.FLOATING} />
          )}
        </div>
      </button>
    );
  }
}
