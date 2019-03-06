// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
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
    } = this.props;

    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      className,
    ]);

    return (
      <button className={componentStyles} onClick={onClick}>
        <span className={styles.meta}>
          <span className={styles.title}>{title}</span>
          <span className={styles.info}>{info}</span>
          {isRestoreActive ? <ProgressBar progress={restoreProgress} /> : null}
        </span>
      </button>
    );
  }
}
