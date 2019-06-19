// @flow
import React from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePoolThumbnail.scss';
import { getColorFromRange } from '../../../utils/colors';
import type { StakePool } from '../../../api/staking/types';
import StakePoolTooltip from './StakePoolTooltip';

type Props = {
  stakePool: StakePool,
  index: number,
  isSelected: boolean,
  currentTheme: string,
  flipHorizontal: boolean,
  flipVertical: boolean,
  onOpenExternalLink: Function,
  onClick: Function,
  onClose: Function,
};

export const StakePoolThumbnail = observer((props: Props) => {
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
  } = props;

  const color = getColorFromRange(index);

  const { ranking, id, retirement } = stakePool;

  const componentClassnames = classnames([
    styles.component,
    isSelected ? styles.isSelected : null,
  ]);

  return (
    <div className={componentClassnames}>
      <div
        className={styles.content}
        onClick={event => onClick(event, ranking)}
        role="link"
        aria-hidden
      >
        <div className={styles.id}>{id}</div>
        <div className={styles.ranking} style={{ color }}>
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
            background: color,
          }}
        />
      </div>
      {isSelected && (
        <StakePoolTooltip
          stakePool={stakePool}
          index={index}
          className={styles.tooltip}
          isVisible
          onClick={onClose}
          currentTheme={currentTheme}
          flipHorizontal={flipHorizontal}
          flipVertical={flipVertical}
          onOpenExternalLink={onOpenExternalLink}
        />
      )}
    </div>
  );
});
