import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ProgressBar.scss' or its cor... Remove this comment to see the full error message
import styles from './ProgressBar.scss';

type Props = {
  progress: number;
};

@observer
class ProgressBar extends Component<Props> {
  static defaultProps = {
    progress: 0,
  };

  render() {
    const { progress } = this.props;
    return (
      <div className={styles.component}>
        <div
          className={styles.progress}
          style={{
            width: `${progress}%`,
          }}
        />
      </div>
    );
  }
}

export default ProgressBar;
