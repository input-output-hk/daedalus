// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import clockIcon from '../../../assets/images/clock.inline.svg';

import styles from './StakePool.scss';

export type StakePoolProps = {
  rank: number,
  id: string,
  // name: string,
  // description: string,
  // url: string,
  // controlledStake: number,
  // profitMargin: number,
  retiring?: Date,
};

@observer
export default class StakePool extends Component<StakePoolProps> {
  render() {
    const {
      rank,
      id /* name, description, url, controlledStake, profitMargin */,
      retiring,
    } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.id}>{id}</div>
        <div className={styles.rank}>{rank}</div>
        {retiring && (
          <div className={styles.clock}>
            <SVGInline svg={clockIcon} className={styles.clockIcon} />
          </div>
        )}
      </div>
    );
  }
}
