// @flow
import { observer } from 'mobx-react';
import React, { useEffect, useState } from 'react';
import { hideAll } from 'tippy.js';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import styles from './StakePoolsList.scss';
import StakePool from '../../../domains/StakePool';
import { ThumbPool } from '../widgets/ThumbPool';

// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;

type StakePoolsListProps = {
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  highlightOnHover?: boolean,
  highlightWithDelay?: boolean,
  onSelect?: (poolId: string) => void,
  selectOnClick?: boolean,
  showWithSelectButton?: boolean,
  containerClassName: string,
  numberOfRankedStakePools: number,
  selectedPoolId?: ?string,
  disabledStakePoolId?: ?string,
};

/**
 * Utility function to programmatically hide all pop overs
 * This is used to hide the pool tooltips on scrolling the list
 */
function hideAllPopOvers() {
  hideAll();
}

/**
 * The StakePoolTiles renders the list of ThumbPool tiles in a grid
 */
const StakePoolTiles = observer((props: StakePoolsListProps) => {
  return (
    <div className={styles.tiles}>
      {props.stakePoolsList.map((stakePool) => {
        return (
          <ThumbPool
            stakePool={stakePool}
            key={stakePool.id + stakePool.ranking}
            onOpenExternalLink={props.onOpenExternalLink}
            highlightOnHover={props.highlightOnHover}
            highlightWithDelay={props.highlightWithDelay}
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
});

/**
 * The StakePoolsList either renders a loading spinner when there are
 * more than PRELOADER_THRESHOLD stake pools to be loaded (to increase
 * initial rendering performance) or StakePoolTiles (if there are only
 * a few stake pools OR if the simulated "preloading" is done)
 */
export const StakePoolsList = observer((props: StakePoolsListProps) => {
  const [isLoading, setIsLoading] = useState(true);
  useEffect(() => {
    window.addEventListener('scroll', hideAllPopOvers, true);
    setTimeout(() => setIsLoading(false));
    return () => window.removeEventListener('scroll', hideAllPopOvers);
  });
  if (props.stakePoolsList.length > PRELOADER_THRESHOLD && isLoading) {
    return (
      <div className={styles.preloadingBlockWrapper}>
        <LoadingSpinner big />
      </div>
    );
  }
  return <StakePoolTiles {...props} />;
});

StakePoolsList.defaultProps = {
  showWithSelectButton: false,
  highlightWithDelay: false,
};
