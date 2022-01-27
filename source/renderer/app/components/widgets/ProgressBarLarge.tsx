import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ProgressBarLarge.scss' or it... Remove this comment to see the full error message
import styles from './ProgressBarLarge.scss';

type Props = {
  progress: number;
  showProgressLabel?: boolean;
  leftLabel?: string;
  rightLabel1?: string;
  rightLabel2?: string;
  isDarkMode?: boolean;
  loading?: boolean;
};

@observer
class ProgressBarLarge extends Component<Props> {
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
      loading,
    } = this.props;
    const isComplete = progress >= 100;
    const progressStyles = classnames([
      styles.progress,
      isComplete ? styles.isComplete : null,
      isDarkMode ? styles.progressDarkMode : styles.progressLightMode,
      loading ? styles.loading : null,
    ]);
    const progressBarContainerStyles = classnames([
      styles.progressBarContainer,
      loading ? styles.loading : null,
    ]);
    return (
      <div className={styles.component}>
        <div className={styles.content}>
          <p className={styles.leftLabel}>{leftLabel}</p>
          <p className={styles.rightLabel}>
            <b>{rightLabel1}</b> {rightLabel2}
          </p>
        </div>
        <div className={progressBarContainerStyles}>
          {!loading && (
            <div
              className={progressStyles}
              style={{
                width: `${progress}%`,
              }}
            >
              {showProgressLabel && (
                <div className={styles.progressLabel}>{progress}%</div>
              )}
            </div>
          )}
        </div>
      </div>
    );
  }
}

export default ProgressBarLarge;
