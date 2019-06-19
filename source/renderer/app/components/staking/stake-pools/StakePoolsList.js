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
  onClick?: Function,
  onHover?: Function,
  getIndex: Function,
  currentTheme: string,
  onSelect?: Function,
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
  onHover,
  getIndex,
  onSelect,
}: Props) => (
  <div className={styles.component}>
    {stakePoolsList.map(stakePool => {
      const index = getIndex(stakePool.ranking);
      const isSelected = getIsSelected(listName, stakePool.ranking);

      const handleThumbnailClick = (event: SyntheticMouseEvent<HTMLElement>) =>
        onClick(listName, event, stakePool.ranking);

      const handleThumbnailHover = (event: SyntheticMouseEvent<HTMLElement>) =>
        onHover(listName, event, stakePool.ranking);

      return (
        <StakePoolThumbnail
          stakePool={stakePool}
          key={stakePool.id}
          onOpenExternalLink={onOpenExternalLink}
          isSelected={isSelected}
          onClose={onClose}
          onClick={handleThumbnailClick}
          onHover={onHover && handleThumbnailHover}
          currentTheme={currentTheme}
          flipHorizontal={flipHorizontal}
          flipVertical={flipVertical}
          index={index}
          onSelect={onSelect}
        />
      );
    })}
  </div>
);
