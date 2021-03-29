// @flow
import React, { useRef, useState } from 'react';
import type { Node } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { STAKE_POOL_TOOLTIP_HOVER_WAIT } from '../../../config/timingConfig';
import StakePool from '../../../domains/StakePool';
import TooltipPool from './TooltipPool';
import styles from './PoolPopOver.scss';

/**
 * Stake pool tooltip component that can be wrapped around
 * any trigger component (e.g: our pool thumbs and pool tickers) by
 * passing them as children of this component.
 *
 * By default it opens on "click" events on the trigger components
 * but can also be configured to openOnHover=true.
 */

export function PoolPopOver(props: {
  children: Node,
  color: string,
  currentTheme: string,
  onOpen?: (poolId: string) => void,
  onClose?: () => void,
  onOpenExternalLink: (string) => void,
  onSelect?: (poolId: string) => void,
  openOnHover?: boolean,
  openWithDelay?: boolean,
  showWithSelectButton?: boolean,
  stakePool: StakePool,
  containerClassName: string,
  numberOfRankedStakePools: number,
  isGridRewardsView?: boolean,
}) {
  // Track hover state manually to optimize performance by lazy init pop overs
  const [isHovered, setIsHovered] = useState(false);
  // The ref passed to Tippy.js as trigger target
  const popOverTargetRef = useRef(null);
  const poolId = props.stakePool.id;

  const close = () => {
    setIsHovered(false);
    if (props.onClose) props.onClose();
  };
  return (
    <>
      <div
        className={styles.triggerTarget}
        onMouseEnter={() => setIsHovered(true)}
        ref={popOverTargetRef}
      >
        {props.children}
      </div>
      {isHovered ? ( // Init the pop over only when the target is hovered
        <PopOver
          interactive
          delay={props.openWithDelay ? STAKE_POOL_TOOLTIP_HOVER_WAIT : 0}
          duration={0}
          appendTo={document.body}
          trigger={props.openOnHover ? 'mouseenter' : 'click'}
          placement="auto"
          onShow={() => props.onOpen && props.onOpen(poolId)}
          onHide={close}
          onClickOutside={close}
          themeVariables={{
            '--rp-pop-over-bg-color':
              'var(--theme-staking-stake-pool-tooltip-background-color)',
            '--rp-pop-over-box-shadow':
              '0 1.5px 5px 0 var(--theme-staking-stake-pool-tooltip-shadow-color)',
            '--rp-pop-over-border-color':
              'var(--theme-staking-stake-pool-tooltip-border-color)',
            '--rp-pop-over-border-radius': '5px',
            '--rp-pop-over-border-style': 'solid',
            '--rp-pop-over-padding': 0,
          }}
          // Passing in the trigger element allows lazy initialization
          reference={popOverTargetRef}
          content={
            <TooltipPool
              color={props.color}
              containerClassName={props.containerClassName}
              currentTheme={props.currentTheme}
              numberOfRankedStakePools={props.numberOfRankedStakePools}
              onClose={close}
              onOpenExternalLink={props.onOpenExternalLink}
              onSelect={() => {
                close();
                if (props.onSelect) props.onSelect(poolId);
              }}
              showWithSelectButton={props.showWithSelectButton}
              stakePool={props.stakePool}
              isGridRewardsView={props.isGridRewardsView}
            />
          }
        />
      ) : null}
    </>
  );
}

PoolPopOver.defaultProps = {
  openWithDelay: false,
};
