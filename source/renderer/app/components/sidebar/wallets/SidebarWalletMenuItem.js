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
  restoreProgress?: number,
  isLegacy: boolean,
  mnemonicsConfirmationStatus: string,
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
      restoreProgress,
      isLegacy,
      mnemonicsConfirmationStatus,
    } = this.props;

    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      isLegacy ? styles.legacyItem : null,
      className,
      styles[`mnemonicsConfirmationStatus-${mnemonicsConfirmationStatus}`],
    ]);

    return (
      <button className={componentStyles} onClick={onClick}>
        <span className={styles.meta}>
          <span className={styles.title}>{title}</span>
          <span className={styles.info}>{info}</span>
          {isRestoreActive ? <ProgressBar progress={restoreProgress} /> : null}
          {isLegacy && <LegacyBadge mode={LEGACY_BADGE_MODES.FLOATING} />}
        </span>
      </button>
    );
  }
}
