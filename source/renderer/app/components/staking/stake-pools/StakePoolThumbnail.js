// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePoolThumbnail.scss';
import { getColorFromRange } from '../../../utils/colors';
import type { StakePool } from '../../../api/staking/types';
import StakePoolTooltip from './StakePoolTooltip';

type Props = {
  currentTheme: string,
  index: number,
  isHighlighted: boolean,
  onClick?: Function,
  onClose: Function,
  onHover?: Function,
  onOpenExternalLink: Function,
  onSelect: Function,
  showWithSelectButton?: boolean,
  showSelected?: boolean,
  stakePool: StakePool,
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

  handleOpen = (event: SyntheticMouseEvent<HTMLElement>) => {
    const { onClose, onClick, onHover, isHighlighted, stakePool } = this.props;
    if (isHighlighted) return onClose();
    event.persist();
    if (event.target instanceof HTMLElement) {
      const targetElement =
        event.target.className === 'StakePool_content'
          ? event.target
          : event.target.parentNode;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = targetElement.getBoundingClientRect();
        this.setState({ top, left });
        onHover ? onHover(stakePool.id) : onClick(stakePool.id);
      }
    }
    return false;
  };

  handleSelect = () => {
    const { stakePool, onSelect } = this.props;
    onSelect(stakePool.id);
  }

  render() {
    const {
      currentTheme,
      index,
      isHighlighted,
      isSelected,
      onClose,
      onHover,
      onOpenExternalLink,
      onSelect,
      showWithSelectButton,
      showSelected,
      stakePool,
    } = this.props;
    const { top, left } = this.state;

    const { ranking, slug, retirement } = stakePool;
    const color = getColorFromRange(index);

    const componentClassnames = classnames([
      styles.component,
      isHighlighted ? styles.isHighlighted : null,
      (isSelected && showSelected) ? styles.isSelected : null,
    ]);

    return (
      <div
        className={componentClassnames}
        onMouseEnter={onHover ? this.handleOpen : null}
        onMouseLeave={onHover ? onClose : null}
      >
        <div
          className={styles.content}
          onClick={!onHover ? this.handleOpen : this.handleSelect}
          role="link"
          aria-hidden
        >
          <div className={styles.slug}>{slug}</div>
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
          />
        )}
      </div>
    );
  }
}
