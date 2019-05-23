// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePool.scss';
import { getHSLColor } from '../../../utils/colors';
import type { StakePoolProps } from '../../../api/staking/types';

type Props = {
  ...$Exact<StakePoolProps>,
  ranking: number,
};

@observer
export default class StakePool extends Component<Props> {
  get color() {
    return getHSLColor(this.props.ranking);
  }

  render() {
    const {
      index,
      id,
      /* name, description, url, controlledStake, profitMargin, performance */
      retiring,
    } = this.props;

    return (
      <div
        className={styles.component}
        style={{
          borderBottomColor: this.color,
        }}
      >
        <div className={styles.id}>{id}</div>
        <div
          className={styles.index}
          style={{
            color: this.color,
          }}
        >
          {index}
        </div>
        {retiring && (
          <div className={styles.clock}>
            <SVGInline svg={clockIcon} className={styles.clockIcon} />
          </div>
        )}
      </div>
    );
  }
}
