// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePool.scss';
import { getHSLColor } from '../../../utils/colors';
import type { StakePoolProps } from '../../../api/staking/types';
import StakePoolTooltip from './StakePoolTooltip';

type Props = {
  stakePool: StakePoolProps,
  index: number,
  isSelected: boolean,
  currentTheme: string,
  flipHorizontal: boolean,
  flipVertical: boolean,
  onOpenExternalLink: Function,
  onClick: Function,
  onClose: Function,
};

@observer
export default class StakePool extends Component<Props> {
  get color() {
    const { index } = this.props;
    return getHSLColor(index);
  }

  render() {
    const {
      stakePool,
      index,
      isSelected,
      currentTheme,
      flipHorizontal,
      flipVertical,
      onClick,
      onClose,
      onOpenExternalLink,
    } = this.props;

    const { ranking, id, retirement } = stakePool;

    const componentClassnames = classnames([
      styles.component,
      isSelected ? styles.isSelected : null,
    ]);

    return (
      <div className={componentClassnames}>
        <div
          className={styles.content}
          onClick={(event: MouseEvent) => onClick(event, ranking)}
          role="link"
          aria-hidden
        >
          <div className={styles.id}>{id}</div>
          <div
            className={styles.ranking}
            style={{
              color: this.color,
            }}
          >
            {ranking}
          </div>
          {retirement && (
            <div className={styles.clock}>
              <SVGInline svg={clockIcon} className={styles.clockIcon} />
            </div>
          )}
          <div
            className={styles.colorBand}
            style={{
              background: this.color,
            }}
          />
        </div>
        <StakePoolTooltip
          stakePool={stakePool}
          index={index}
          className={styles.tooltip}
          isVisible={isSelected}
          onClick={onClose}
          currentTheme={currentTheme}
          flipHorizontal={flipHorizontal}
          flipVertical={flipVertical}
          onOpenExternalLink={onOpenExternalLink}
        />
      </div>
    );
  }
}
