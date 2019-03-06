// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import classNames from 'classnames';
import styles from './StatusIcons.scss';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
import nodeStateIcon from '../../assets/images/node-state-icon.inline.svg';
import isNodeRespondingIcon from '../../assets/images/is-node-responding-icon.inline.svg';
import isNodeSubscribedIcon from '../../assets/images/is-node-subscribed-icon.inline.svg';
import isNodeTimeCorrectIcon from '../../assets/images/is-node-time-correct-icon.inline.svg';
import isNodeSyncingIcon from '../../assets/images/is-node-syncing-icon.inline.svg';
import type { CardanoNodeState } from '../../../../common/types/cardano-node.types';

type Props = {
  nodeState: ?CardanoNodeState,
  isNodeResponding?: boolean,
  isNodeSubscribed?: boolean,
  isNodeTimeCorrect?: boolean,
  isNodeSyncing?: boolean,
};

const STATUS_CLASSNAMES: Object = {
  [CardanoNodeStates.STARTING]: 'unloaded',
  [CardanoNodeStates.RUNNING]: 'on',
  [CardanoNodeStates.EXITING]: 'unloaded',
  [CardanoNodeStates.STOPPING]: 'unloaded',
  [CardanoNodeStates.STOPPED]: 'unloaded',
  [CardanoNodeStates.UPDATING]: 'unloaded',
  [CardanoNodeStates.UPDATED]: 'unloaded',
  [CardanoNodeStates.CRASHED]: 'off',
  [CardanoNodeStates.ERRORED]: 'off',
  [CardanoNodeStates.UNRECOVERABLE]: 'off',
  true: 'on',
  false: 'off',
  undefined: 'unloaded',
};

const PARAM_PRETTY_NAME: Object = {
  isNodeResponding: 'responding',
  isNodeSubscribed: 'subscribed',
  isNodeSyncing: 'syncing',
  isNodeTimeCorrect: 'correct',
};

export default class StatusIcon extends Component<Props> {

  getTip = (paramName: string) => {
    const paramValue: boolean = this.props[paramName];
    let status = paramValue ? 'is' : 'is not';
    let paramPrettyName = PARAM_PRETTY_NAME[paramName];
    if (paramName === 'nodeState') {
      status = 'is';
      paramPrettyName = paramValue;
    }
    let nodePrettyName = paramName !== 'isNodeTimeCorrect'
      ? `Node ${status} `
      : `Node time ${status} `;
    if (typeof paramValue === 'undefined') {
      nodePrettyName = 'Checking if node is ';
    }
    return [nodePrettyName, <b key="param">{paramPrettyName}</b>];
  };

  getClassName = (paramName: string) => {
    // If node is not running, it displays the icons with opacity
    // Whether {isNodeSyncing} it displays the icons for syncing or loading screen
    const { isNodeSyncing } = this.props;
    const paramValue = this.props[paramName];
    let status = STATUS_CLASSNAMES[paramValue];
    if (this.isDisabled(paramName)) {
      status = 'unknown';
    }
    return classNames([
      styles[`icon-${status}`],
      isNodeSyncing ? styles.syncing : styles.loading,
    ]);
  };

  getTooltipClassname = (paramName: string) => {
    const paramValue = this.props[paramName];
    if (typeof paramValue === 'undefined') {
      return 'tooltipEllipsis';
    }
    if (this.isDisabled(paramName)) {
      return 'tooltipDisabled';
    }
  }

  isDisabled = (paramName: string) => (
    paramName !== 'nodeState' && this.props.nodeState !== CardanoNodeStates.RUNNING
  );

  getIconWithToolTip = (icon: string, paramName: string) => {
    if (this.isDisabled(paramName)) {
      return (<SVGInline svg={icon} className={this.getClassName(paramName)} />);
    }
    return (
      <Tooltip
        skin={TooltipSkin}
        tip={this.getTip(paramName)}
        className={this.getTooltipClassname(paramName)}
      >
        <SVGInline svg={icon} className={this.getClassName(paramName)} />
      </Tooltip>
    );
  }

  render() {
    return (
      <div className={styles.component}>
        {[
          this.getIconWithToolTip(nodeStateIcon, 'nodeState'),
          this.getIconWithToolTip(isNodeRespondingIcon, 'isNodeResponding'),
          this.getIconWithToolTip(isNodeSubscribedIcon, 'isNodeSubscribed'),
          this.getIconWithToolTip(isNodeTimeCorrectIcon, 'isNodeTimeCorrect'),
          this.getIconWithToolTip(isNodeSyncingIcon, 'isNodeSyncing'),
        ]}
      </div>
    );
  }
}
