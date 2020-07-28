// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './ThumbPool.scss';
import { getColorFromRange } from '../../../utils/colors';
import TooltipPool from './TooltipPool';
import StakePool from '../../../domains/StakePool';
import { STAKE_POOL_TOOLTIP_HOVER_WAIT } from '../../../config/timingConfig';
import { getRelativePosition } from '../../../utils/domManipulation';
import ThumbSelectedPool from './ThumbSelectedPool';
import ThumbPoolContent from './ThumbPoolContent';

type Props = {
  currentTheme: string,
  isHighlighted: boolean,
  onClick?: Function,
  onClose: Function,
  onHover?: Function,
  onOpenExternalLink: Function,
  onSelect: Function,
  showWithSelectButton?: boolean,
  showSelected?: boolean,
  stakePool: StakePool,
  isShelleyDataAvailable: boolean,
  isSelected?: ?Function,
  containerClassName: string,
  numberOfStakePools: number,
  disabledStakePoolId: ?string,
};

type State = {
  top: number,
  left: number,
};

@observer
export class ThumbPool extends Component<Props, State> {
  state = {
    top: 0,
    left: 0,
  };

  hoverWait: TimeoutID;

  handleHover = (stakePoolId: string) => {
    clearTimeout(this.hoverWait);
    this.hoverWait = setTimeout(() => {
      if (this.props.onHover) this.props.onHover(stakePoolId);
    }, STAKE_POOL_TOOLTIP_HOVER_WAIT);
  };

  handleClose = (stakePoolId: string) => {
    clearTimeout(this.hoverWait);
    this.props.onClose(stakePoolId);
  };

  handleOpen = (event: SyntheticMouseEvent<HTMLElement>) => {
    const {
      onClose,
      onClick,
      onHover,
      isHighlighted,
      stakePool,
      containerClassName,
    } = this.props;
    if (isHighlighted) return onClose();
    event.persist();
    const targetElement = event.target;
    if (targetElement instanceof HTMLElement) {
      const { top, left } = getRelativePosition(
        targetElement,
        `.${containerClassName}`
      );
      this.setState({ top, left });
      if (onHover) {
        this.handleHover(stakePool.id);
      } else if (onClick) {
        onClick(stakePool.id);
      }
    }
    return false;
  };

  handleSelect = () => {
    const { stakePool, onSelect } = this.props;
    onSelect(stakePool.id);
  };

  render() {
    const {
      currentTheme,
      isHighlighted,
      isSelected,
      onClose,
      onHover,
      onOpenExternalLink,
      showWithSelectButton,
      showSelected,
      stakePool,
      isShelleyDataAvailable,
      containerClassName,
      numberOfStakePools,
      disabledStakePoolId,
    } = this.props;
    const { top, left } = this.state;

    const { ranking, id } = stakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);
    const isDisabled = disabledStakePoolId === id;

    const componentClassnames = classnames([
      styles.component,
      isSelected && showSelected ? styles.isSelected : null,
    ]);

    const contentClassnames = classnames([
      styles.content,
      isHighlighted ? styles.isHighlighted : null,
      onHover ? styles.isOnHover : null,
      isDisabled ? styles.disabled : null,
    ]);

    const content =
      isSelected && showSelected ? (
        <ThumbSelectedPool
          stakePool={stakePool}
          isShelleyDataAvailable={isShelleyDataAvailable}
          numberOfStakePools={numberOfStakePools}
        />
      ) : (
        <ThumbPoolContent
          stakePool={stakePool}
          isShelleyDataAvailable={isShelleyDataAvailable}
          numberOfStakePools={numberOfStakePools}
        />
      );

    return (
      <div
        className={componentClassnames}
        onMouseLeave={onHover ? this.handleClose : null}
      >
        <button
          onMouseEnter={onHover ? this.handleOpen : null}
          onClick={!onHover ? this.handleOpen : this.handleSelect}
        />
        <div className={contentClassnames}>{content}</div>
        {isHighlighted && (
          <TooltipPool
            stakePool={stakePool}
            isShelleyDataAvailable={isShelleyDataAvailable}
            className={styles.tooltip}
            isVisible
            onClick={onClose}
            currentTheme={currentTheme}
            onOpenExternalLink={onOpenExternalLink}
            top={top}
            left={left}
            color={color}
            onSelect={this.handleSelect}
            showWithSelectButton={showWithSelectButton}
            containerClassName={containerClassName}
            numberOfStakePools={numberOfStakePools}
          />
        )}
      </div>
    );
  }
}
