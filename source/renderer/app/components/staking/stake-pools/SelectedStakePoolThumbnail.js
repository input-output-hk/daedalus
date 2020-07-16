// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import styles from './SelectedStakePoolThumbnail.scss';
import { getColorFromRange } from '../../../utils/colors';
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import questionmarkImage from '../../../assets/images/questionmark.inline.svg';
import clockImage from '../../../assets/images/clock.inline.svg';
import StakePool from '../../../domains/StakePool';

type Props = {
  stakePool?: StakePool,
  alreadyDelegated?: boolean,
  stakePoolsLength: number,
};

@observer
export default class StakePoolThumbnail extends Component<Props> {
  render() {
    const { stakePool, alreadyDelegated, stakePoolsLength } = this.props;

    const { ticker, retiring, ranking } = stakePool || {};

    const rankColor =
      stakePool && !retiring
        ? getColorFromRange(ranking, stakePoolsLength)
        : 'transparent';

    const selectedPoolBlockClasses = classnames([
      stakePool ? styles.selectedPoolBlock : styles.selectPoolBlockPlaceholder,
      retiring ? styles.retiring : null,
      alreadyDelegated ? styles.alreadyDelegated : null,
    ]);

    let icon = questionmarkImage;
    if (retiring) icon = clockImage;
    else if (stakePool) icon = checkmarkImage;

    return (
      <div
        role="presentation"
        className={selectedPoolBlockClasses}
        style={{
          background: rankColor,
        }}
      >
        {ticker && <div className={styles.ticker}>{ticker}</div>}
        <div className={styles.icon}>
          <SVGInline svg={icon} />
        </div>
      </div>
    );
  }
}
