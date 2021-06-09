// @flow
import { times } from 'lodash-es/util';
import { observer } from 'mobx-react';
import React, { useEffect, useState } from 'react';
import type { ElementRef } from 'react';
import { AutoSizer, List, WindowScroller } from 'react-virtualized';
import { hideAll } from 'tippy.js';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import styles from './StakePoolsList.scss';
import StakePool from '../../../domains/StakePool';
import { ThumbPool } from '../widgets/ThumbPool';

// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;
const POOL_THUMB_SIZE = 80;
const POOL_THUMB_GRID_GAP = 10;

/**
 * Utility function to programmatically hide all pop overs
 * This is used to hide the pool tooltips on scrolling the list
 */
function hideAllPopOvers() {
  hideAll();
}

/**
 * The StakePoolsList either renders a loading spinner when there are
 * more than PRELOADER_THRESHOLD stake pools to be loaded (to increase
 * initial rendering performance) or StakePoolTiles (if there are only
 * a few stake pools OR if the simulated "preloading" is done)
 */

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
  isGridRewardsView?: boolean,
  scrollElementRef?: ?ElementRef<*>,
};

export const StakePoolsList = observer((props: StakePoolsListProps) => {
  const [isLoading, setIsLoading] = useState(true);
  useEffect(() => {
    // Feature: Hide pool pop overs if the list scroll container is scrolled
    // Note: do not use window here otherwise the pool description cannot be
    // scrolled anymore because it closes the pop over immediately.
    const scrollContainer = props.scrollElementRef
      ? props.scrollElementRef.current
      : null;
    if (scrollContainer !== null) {
      scrollContainer.addEventListener('scroll', hideAllPopOvers, true);
    }
    setTimeout(() => setIsLoading(false));
    return () => {
      if (scrollContainer !== null) {
        scrollContainer.removeEventListener('scroll', hideAllPopOvers);
      }
    };
  });
  if (props.stakePoolsList.length > PRELOADER_THRESHOLD && isLoading) {
    return (
      <div className={styles.preloadingBlockWrapper}>
        <LoadingSpinner big />
      </div>
    );
  }
  const stakePoolsCount = props.stakePoolsList.length;

  function rowRenderer(itemsPerRow, { index, key, style }) {
    const startIndex = itemsPerRow * index;
    const endIndex = startIndex + itemsPerRow;
    const stakePools = props.stakePoolsList.slice(startIndex, endIndex);
    const numberOfMissingRowItems = itemsPerRow - stakePools.length;
    return (
      <div key={key} style={style}>
        <div className={styles.tiles}>
          {stakePools.map((stakePool) => (
            <ThumbPool
              key={stakePool.id}
              stakePool={stakePool}
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
              isGridRewardsView={props.isGridRewardsView}
            />
          ))}
          {numberOfMissingRowItems > 0
            ? times(numberOfMissingRowItems, (i) => (
                <div key={`${key}-${i}`} className={styles.rowFillerItem} />
              ))
            : null}
        </div>
      </div>
    );
  }

  return (
    <WindowScroller
      scrollElement={
        props.scrollElementRef && props.scrollElementRef.current
          ? props.scrollElementRef.current
          : window
      }
    >
      {({ height, scrollTop, registerChild }) => (
        <AutoSizer disableHeight>
          {({ width }) => {
            if (!stakePoolsCount || !width) {
              return null;
            }
            const itemsPerRow = Math.floor(
              width / (POOL_THUMB_SIZE + POOL_THUMB_GRID_GAP)
            );
            const rowCount = Math.ceil(stakePoolsCount / itemsPerRow);
            return (
              <div ref={(el) => registerChild(el)}>
                <List
                  autoHeight
                  className={styles.list}
                  width={width}
                  height={height}
                  scrollTop={scrollTop}
                  rowHeight={POOL_THUMB_SIZE}
                  rowCount={rowCount}
                  rowRenderer={(args) => rowRenderer(itemsPerRow, args)}
                  overscanRowCount={3}
                />
              </div>
            );
          }}
        </AutoSizer>
      )}
    </WindowScroller>
  );
});

StakePoolsList.defaultProps = {
  showWithSelectButton: false,
  highlightWithDelay: false,
};
