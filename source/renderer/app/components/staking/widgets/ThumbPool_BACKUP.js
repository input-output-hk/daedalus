// @flow
import type { ElementRef } from 'react';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import styles from './ThumbPool.scss';
import { getColorFromRange } from '../../../utils/colors';
import TooltipPool from './TooltipPool';
import StakePool from '../../../domains/StakePool';
import { STAKE_POOL_TOOLTIP_HOVER_WAIT } from '../../../config/timingConfig';
import ThumbSelectedPool from './ThumbSelectedPool';
import ThumbPoolContent from './ThumbPoolContent';

type Props = {
  currentTheme: string,
  isHighlighted: boolean,
  highlightOnHover?: boolean,
  onOpen?: Function,
  onClose: Function,
  onOpenExternalLink: Function,
  onSelect: Function,
  showWithSelectButton?: boolean,
  showSelected?: boolean,
  stakePool: StakePool,
  isSelected?: ?Function,
  containerClassName: string,
  numberOfRankedStakePools: number,
  disabledStakePoolId: ?string,
};

@observer
export class ThumbPool extends Component<Props> {
  tippyInstance: ElementRef<*>;

  handleClose = (stakePoolId: string) => {
    this.props.onClose(stakePoolId);
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
      highlightOnHover,
      onClose,
      onOpen,
      onOpenExternalLink,
      showWithSelectButton,
      showSelected,
      stakePool,
      containerClassName,
      numberOfRankedStakePools,
      disabledStakePoolId,
    } = this.props;

    const { ranking, id } = stakePool;
    const color = getColorFromRange(ranking, numberOfRankedStakePools);
    const isDisabled = disabledStakePoolId === id;

    const contentClassnames = classnames([
      styles.content,
      isDisabled ? styles.disabled : null,
      isSelected && showSelected ? styles.isSelected : null,
      isHighlighted ? styles.isHighlighted : null,
      highlightOnHover ? styles.isOnHover : null,
    ]);

    const content =
      isSelected && showSelected ? (
        <ThumbSelectedPool
          stakePool={stakePool}
          numberOfRankedStakePools={numberOfRankedStakePools}
        />
      ) : (
        <ThumbPoolContent
          stakePool={stakePool}
          numberOfRankedStakePools={numberOfRankedStakePools}
        />
      );

    return (
      <div className={styles.component}>
        <PopOver
          interactive
          placement="auto"
          delay={highlightOnHover ? STAKE_POOL_TOOLTIP_HOVER_WAIT : 0}
          trigger={highlightOnHover ? 'mouseenter' : 'click'}
          onTrigger={() => onOpen && onOpen(id)}
          onUntrigger={onClose}
          themeVariables={{
            '--rp-pop-over-bg-color':
              'var(--theme-staking-stake-pool-tooltip-background-color)',
            '--rp-pop-over-box-shadow':
              '0 1.5px 5px 0 var(--theme-staking-stake-pool-tooltip-shadow-color)',
            '--rp-pop-over-border-color':
              'var(--theme-staking-stake-pool-tooltip-border-color)',
            '--rp-pop-over-border-radius': '5px',
            '--rp-pop-over-border-style': 'solid',
          }}
          onShow={(tippyInstance) => {
            this.tippyInstance = tippyInstance;
          }}
          content={
            <TooltipPool
              stakePool={stakePool}
              onClose={() =>
                this.tippyInstance ? this.tippyInstance.hide() : onClose
              }
              currentTheme={currentTheme}
              onOpenExternalLink={onOpenExternalLink}
              color={color}
              onSelect={this.handleSelect}
              showWithSelectButton={showWithSelectButton}
              containerClassName={containerClassName}
              numberOfRankedStakePools={numberOfRankedStakePools}
              isListView={false}
            />
          }
        >
          <div className={contentClassnames}>{content}</div>
        </PopOver>
      </div>
    );
  }
}
