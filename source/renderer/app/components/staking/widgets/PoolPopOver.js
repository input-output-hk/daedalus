// @flow
import React, { useRef, useState } from 'react';
import type { Node } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { STAKE_POOL_TOOLTIP_HOVER_WAIT } from '../../../config/timingConfig';
import StakePool from '../../../domains/StakePool';
import TooltipPool from './TooltipPool';

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
  showWithSelectButton?: boolean,
  stakePool: StakePool,
  containerClassName: string,
  numberOfRankedStakePools: number,
}) {
  const [isOpen, setIsOpen] = useState(false);
  const [isHovered, setIsHovered] = useState(false);
  const hoverTimeout = useRef(null);
  const isVisible = isOpen || (isHovered && props.openOnHover);
  const poolId = props.stakePool.id;

  const close = () => {
    setIsOpen(false);
    if (props.onClose) props.onClose();
  };

  const hoverWithDelay = () => {
    hoverTimeout.current = setTimeout(
      () => setIsHovered(true),
      STAKE_POOL_TOOLTIP_HOVER_WAIT
    );
  };

  const cancelHover = () => {
    clearTimeout(hoverTimeout.current);
    setIsHovered(false);
  };

  return (
    <div
      onMouseEnter={hoverWithDelay}
      onMouseLeave={cancelHover}
      onClick={() => setIsOpen(true)}
    >
      {isVisible ? (
        <PopOver
          interactive
          appendTo={document.body}
          placement="left"
          visible={isVisible}
          onShow={() => props.onOpen && props.onOpen(poolId)}
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
            />
          }
        >
          {props.children}
        </PopOver>
      ) : (
        props.children
      )}
    </div>
  );
}
