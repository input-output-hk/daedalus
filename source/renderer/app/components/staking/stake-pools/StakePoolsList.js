// @flow
import React from 'react';
import styles from './StakePoolsList.scss';
import type { StakePool } from '../../../api/staking/types';
import { StakePoolThumbnail } from './StakePoolThumbnail';

type Props = {
  listName: string,
  flipHorizontal: boolean,
  flipVertical: boolean,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  getIsSelected: Function,
  onClose: Function,
  onClick: Function,
  getIndex: Function,
  currentTheme: string,
};

export const StakePoolsList = ({
  listName,
  flipHorizontal,
  flipVertical,
  stakePoolsList,
  onOpenExternalLink,
  currentTheme,
  getIsSelected,
  onClose,
  onClick,
  getIndex,
}: Props) => (
  <div className={styles.component}>
    {stakePoolsList.map(stakePool => {
      const index = getIndex(stakePool.ranking);
      const isSelected = getIsSelected(listName, stakePool.ranking);

      const handleThumbnailClick = (event: SyntheticMouseEvent<HTMLElement>) =>
        onClick(listName, event, stakePool.ranking);

      return (
        <StakePoolThumbnail
          stakePool={stakePool}
          key={stakePool.id}
          onOpenExternalLink={onOpenExternalLink}
          isSelected={isSelected}
          onClose={onClose}
          onClick={handleThumbnailClick}
          currentTheme={currentTheme}
          flipHorizontal={flipHorizontal}
          flipVertical={flipVertical}
          index={index}
        />
      );
    })}
  </div>
);
