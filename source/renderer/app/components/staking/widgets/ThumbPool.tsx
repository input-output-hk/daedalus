import React, { useState } from 'react';
import classnames from 'classnames';
import { PoolPopOver } from './PoolPopOver';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ThumbPool.scss' or its corre... Remove this comment to see the full error message
import styles from './ThumbPool.scss';
import { getColorFromRange } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import ThumbSelectedPool from './ThumbSelectedPool';
import ThumbPoolContent from './ThumbPoolContent';

/**
 * Stake pool thumbnail component that uses the PoolPopOver
 * to show stake pool information on click (by default) or
 * highlightOnHover (configurable via prop).
 *
 * It also renders differently depending on the isSelected prop
 */
export function ThumbPool(props: {
  currentTheme: string;
  isSelected: boolean;
  highlightOnHover?: boolean;
  highlightWithDelay?: boolean;
  onOpenExternalLink: (...args: Array<any>) => any;
  onSelect?: (poolId: string) => void;
  selectOnClick?: boolean;
  showWithSelectButton?: boolean;
  stakePool: StakePool;
  containerClassName: string;
  numberOfRankedStakePools: number;
  disabledStakePoolId: string | null | undefined;
  isGridRewardsView?: boolean;
}) {
  const {
    isGridRewardsView,
    isSelected,
    numberOfRankedStakePools,
    stakePool,
  } = props;
  const { ranking, id } = stakePool;
  const color = getColorFromRange(ranking, numberOfRankedStakePools);
  const isDisabled = props.disabledStakePoolId === id;
  const [isHighlighted, setIsHighlighted] = useState(false);
  const contentClassnames = classnames([
    styles.content,
    isDisabled ? styles.disabled : null,
    isHighlighted ? styles.isHighlighted : null,
    props.highlightOnHover ? styles.shouldHighlightOnHover : null,
  ]);
  const content = isSelected ? (
    <ThumbSelectedPool
      stakePool={stakePool}
      numberOfRankedStakePools={numberOfRankedStakePools}
    />
  ) : (
    <ThumbPoolContent
      stakePool={stakePool}
      isGridRewardsView={isGridRewardsView}
      numberOfRankedStakePools={numberOfRankedStakePools}
    />
  );
  return (
    <div className={styles.component}>
      <PoolPopOver
        color={color}
        currentTheme={props.currentTheme}
        openOnHover={props.highlightOnHover}
        onClose={() => setIsHighlighted(false)}
        onOpen={() => setIsHighlighted(true)}
        onOpenExternalLink={props.onOpenExternalLink}
        openWithDelay={props.highlightWithDelay}
        onSelect={props.onSelect}
        stakePool={stakePool}
        containerClassName={props.containerClassName}
        numberOfRankedStakePools={numberOfRankedStakePools}
        showWithSelectButton={props.showWithSelectButton}
        isGridRewardsView={isGridRewardsView}
      >
        <div
          className={contentClassnames}
          onClick={() =>
            props.selectOnClick && props.onSelect && props.onSelect(id)
          }
        >
          {content}
        </div>
      </PoolPopOver>
    </div>
  );
}
