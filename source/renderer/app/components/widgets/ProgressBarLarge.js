// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './ProgressBarLarge.scss';

type Props = {
  progress: number,
  showProgressLabel?: boolean,
};

@observer
export default class ProgressBarLarge extends Component<Props> {
  static defaultProps = {
    progress: 0,
  };

  render() {
    const { progress, showProgressLabel } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.progressBarContainer}>
          <div className={styles.progress} style={{ width: `${progress}%` }}>
            {showProgressLabel && (
              <div className={styles.progressLabel}>{progress}%</div>
            )}
          </div>
        </div>
      </div>
    );
  }
}
