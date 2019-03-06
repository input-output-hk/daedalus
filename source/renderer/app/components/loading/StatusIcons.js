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

type Props = {
  nodeState: string,
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
    let status = paramValue ? ' is ' : ' is not ';
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
    const { nodeState, isNodeSyncing } = this.props;
    const paramValue = this.props[paramName];
    // If {!isNodeRunning} it displays the icons without opacity
    // Whether {isNodeSyncing} it displays the icons for syncing or loading screen
    let status = STATUS_CLASSNAMES[paramValue];
    if (paramName !== 'nodeState' && nodeState !== CardanoNodeStates.RUNNING) {
      status = 'unknown';
    }
    return classNames([
      styles[`icon-${status}`],
      isNodeSyncing ? styles.syncing : styles.loading,
    ]);
  };

  getIconWithToolTip = (icon: string, param: string) => (
    <Tooltip skin={TooltipSkin} tip={this.getTip(param)}>
      <SVGInline svg={icon} className={this.getClassName(param)} />
    </Tooltip>
  );

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
