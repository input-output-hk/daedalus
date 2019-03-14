// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './ProgressBar.scss';

type Props = {
  progress: number,
};

@observer
export default class ProgressBar extends Component<Props> {
  static defaultProps = {
    progress: 0,
  };

  render() {
    const { progress } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.progress} style={{ width: `${progress}%` }} />
      </div>
    );
  }
}
