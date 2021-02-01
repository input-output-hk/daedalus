// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import styles from './ProgressBarLarge.scss';

type Props = {
  progress: number,
  showProgressLabel?: boolean,
  leftLabel?: string,
  rightLabel1?: string,
  rightLabel2?: string,
  isDarkMode?: boolean,
};

@observer
export default class ProgressBarLarge extends Component<Props> {
  static defaultProps = {
    progress: 0,
  };

  render() {
    const {
      progress,
      showProgressLabel,
      leftLabel,
      rightLabel1,
      rightLabel2,
      isDarkMode,
    } = this.props;

    const progressStyles = classnames([
      styles.progress,
      isDarkMode ? styles.progressDarkMode : styles.progressLightMode,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.content}>
          <p className={styles.leftLabel}>{leftLabel}</p>
          <p className={styles.rightLabel}>
            <b>{rightLabel1}</b> {rightLabel2}
          </p>
        </div>

        <div className={styles.progressBarContainer}>
          <div className={progressStyles} style={{ width: `${progress}%` }}>
            {showProgressLabel && (
              <div className={styles.progressLabel}>{progress}%</div>
            )}
          </div>
        </div>
      </div>
    );
  }
}
