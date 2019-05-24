// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePool.scss';
import { getHSLColor } from '../../../utils/colors';
import type { StakePoolProps } from '../../../api/staking/types';
import StakePoolTooltip from './StakePoolTooltip';

type Props = {
  ...$Exact<StakePoolProps>,
  ranking: number,
  onOpenExternalLink: Function,
};

@observer
export default class StakePool extends Component<Props> {
  get color() {
    return getHSLColor(this.props.ranking);
  }

  render() {
    const { index, id, retirement } = this.props;

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
        {retirement && (
          <div className={styles.clock}>
            <SVGInline svg={clockIcon} className={styles.clockIcon} />
          </div>
        )}
        <StakePoolTooltip {...this.props} className={styles.tooltip} />
      </div>
    );
  }
}
