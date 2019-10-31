// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePoolThumbnail.scss';
import { getColorFromRange } from '../../../utils/colors';
import StakePoolTooltip from './StakePoolTooltip';
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import type { StakePool } from '../../../api/staking/types';
import { STAKE_POOL_TOOLTIP_HOVER_WAIT } from '../../../config/timingConfig';
import { getRelativePosition } from '../../../utils/domManipulation';

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
  isSelected?: ?Function,
  containerClassName: string,
};

type State = {
  top: number,
  left: number,
};

@observer
export class StakePoolThumbnail extends Component<Props, State> {
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
      containerClassName,
    } = this.props;
    const { top, left } = this.state;

    const { ranking, slug, retiring } = stakePool;
    const color = getColorFromRange(ranking);

    const componentClassnames = classnames([
      styles.component,
      isHighlighted ? styles.isHighlighted : null,
      onHover ? styles.isOnHover : null,
      isSelected && showSelected ? styles.isSelected : null,
    ]);

    return (
      <div
        className={componentClassnames}
        onMouseLeave={onHover ? this.handleClose : null}
        style={{
          background: isSelected && showSelected && color,
        }}
      >
        <button
          onMouseEnter={onHover ? this.handleOpen : null}
          onClick={!onHover ? this.handleOpen : this.handleSelect}
        />
        <div className={styles.content}>
          <div className={styles.slug}>{slug}</div>

          {isSelected && showSelected ? (
            <div className={styles.checkmarkWrapper}>
              <SVGInline
                svg={checkmarkImage}
                className={styles.checkmarkImage}
              />
            </div>
          ) : (
            <div className={styles.ranking} style={{ color }}>
              {ranking}
            </div>
          )}

          {retiring && (
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
        {isHighlighted && (
          <StakePoolTooltip
            stakePool={stakePool}
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
          />
        )}
      </div>
    );
  }
}
