// @flow
import React, { useRef } from 'react';
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
  const tippyRef = useRef(null);
  const close = () => {
    if (tippyRef.current) tippyRef.current.hide();
    if (props.onClose) props.onClose();
  };
  const poolId = props.stakePool.id;
  return (
    <PopOver
      interactive
      appendTo={document.body}
      placement="left"
      delay={props.openOnHover ? STAKE_POOL_TOOLTIP_HOVER_WAIT : 0}
      trigger={props.openOnHover ? 'mouseenter' : 'click'}
      onShow={(instance) => {
        tippyRef.current = instance;
        if (props.onOpen) props.onOpen(poolId);
      }}
      onHide={() => props.onClose && props.onClose()}
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
  );
}
