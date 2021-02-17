// @flow
import React, { useEffect } from 'react';
import { hideAll } from 'tippy.js';
import styles from './StakePoolsList.scss';
import StakePool from '../../../domains/StakePool';
import { ThumbPool } from '../widgets/ThumbPool';

function hideAllPopOvers() {
  hideAll();
}

/**
 * Stake pool list renders ThumbPool tiles
 */

export function StakePoolsList(props: {
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  highlightOnHover?: boolean,
  onSelect?: (poolId: string) => void,
  selectOnClick?: boolean,
  showWithSelectButton?: boolean,
  containerClassName: string,
  numberOfRankedStakePools: number,
  selectedPoolId?: ?string,
  disabledStakePoolId?: ?string,
}) {
  useEffect(() => {
    window.addEventListener('scroll', hideAllPopOvers, true);
    return () => window.removeEventListener('scroll', hideAllPopOvers);
  });
  return (
    <div className={styles.component}>
      {props.stakePoolsList.map((stakePool) => {
        return (
          <ThumbPool
            stakePool={stakePool}
            key={stakePool.id + stakePool.ranking}
            onOpenExternalLink={props.onOpenExternalLink}
            highlightOnHover={props.highlightOnHover}
            showWithSelectButton={props.showWithSelectButton}
            onSelect={props.onSelect}
            selectOnClick={props.selectOnClick}
            isSelected={props.selectedPoolId === stakePool.id}
            currentTheme={props.currentTheme}
            containerClassName={props.containerClassName}
            numberOfRankedStakePools={props.numberOfRankedStakePools}
            disabledStakePoolId={props.disabledStakePoolId}
          />
        );
      })}
    </div>
  );
}

StakePoolsList.defaultProps = {
  showWithSelectButton: false,
};
