import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ThumbSelectedPool.scss' or i... Remove this comment to see the full error message
import styles from './ThumbSelectedPool.scss';
import { getColorFromRange } from '../../../utils/colors';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/check-w... Remove this comment to see the full error message
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/questio... Remove this comment to see the full error message
import questionmarkImage from '../../../assets/images/questionmark.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clock.i... Remove this comment to see the full error message
import clockImage from '../../../assets/images/clock.inline.svg';
import StakePool from '../../../domains/StakePool';
import { IS_RANKING_DATA_AVAILABLE } from '../../../config/stakingConfig';

type Props = {
  stakePool?: StakePool;
  alreadyDelegated?: boolean;
  numberOfRankedStakePools: number;
};

@observer
class ThumbSelectedPool extends Component<Props> {
  render() {
    const {
      stakePool,
      alreadyDelegated,
      numberOfRankedStakePools,
    } = this.props;
    const { ticker, retiring, ranking } = stakePool || {};
    const rankColor =
      stakePool && !retiring && IS_RANKING_DATA_AVAILABLE
        ? getColorFromRange(ranking, numberOfRankedStakePools)
        : null;
    const selectedPoolBlockStyle = rankColor
      ? {
          background: rankColor,
        }
      : {};
    const selectedPoolBlockClasses = classnames([
      styles.component,
      stakePool ? styles.selectedPoolBlock : styles.selectPoolBlockPlaceholder,
      retiring ? styles.retiring : null,
      alreadyDelegated ? styles.alreadyDelegated : null,
    ]);
    let icon = questionmarkImage;

    if (retiring) {
      icon = clockImage;
    } else if (stakePool) {
      icon = checkmarkImage;
    }

    return (
      <div className={selectedPoolBlockClasses} style={selectedPoolBlockStyle}>
        {ticker && <div className={styles.ticker}>{ticker}</div>}
        <div className={styles.icon}>
          <SVGInline svg={icon} />
        </div>
      </div>
    );
  }
}

export default ThumbSelectedPool;
