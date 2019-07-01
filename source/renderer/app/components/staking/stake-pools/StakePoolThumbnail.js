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
  stakePool: StakePool,
  index: number,
  isSelected: boolean,
  currentTheme: string,
  onOpenExternalLink: Function,
  onClick?: Function,
  onHover?: Function,
  onClose: Function,
  onSelect: Function,
  showWithSelectButton?: boolean,
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

  handleClick = (event: SyntheticMouseEvent<HTMLElement>) => {
    const { onClose, onClick, onHover, isSelected, stakePool } = this.props;
    if (isSelected) return onClose();
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

  render() {
    const {
      stakePool,
      index,
      isSelected,
      currentTheme,
      onClose,
      onHover,
      onOpenExternalLink,
      onSelect,
      showWithSelectButton,
    } = this.props;

    const { top, left } = this.state;

    const color = getColorFromRange(index);

    const { ranking, slug, retirement } = stakePool;

    const componentClassnames = classnames([
      styles.component,
      isSelected ? styles.isSelected : null,
    ]);

    return (
      <div
        className={componentClassnames}
        onMouseEnter={onHover ? this.handleClick : null}
        onMouseLeave={onHover ? onClose : null}
      >
        <div
          className={styles.content}
          onClick={!onHover ? this.handleClick : null}
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
        {isSelected && (
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
            onSelect={onSelect}
            showWithSelectButton={showWithSelectButton}
          />
        )}
      </div>
    );
  }
}
