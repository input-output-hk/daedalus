// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage, FormattedMessage } from 'react-intl';
import { camelCase } from 'lodash';
import SVGInline from 'react-svg-inline';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import classNames from 'classnames';
import styles from './StatusIcons.scss';
import tooltipStyles from '../../themes/overrides/TooltipOverrides.scss';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
import nodeStateIcon from '../../assets/images/node-state-icon.inline.svg';
import isNodeRespondingIcon from '../../assets/images/is-node-responding-icon.inline.svg';
import isNodeSubscribedIcon from '../../assets/images/is-node-subscribed-icon.inline.svg';
import isNodeTimeCorrectIcon from '../../assets/images/is-node-time-correct-icon.inline.svg';
import isNodeSyncingIcon from '../../assets/images/is-node-syncing-icon.inline.svg';
import type { CardanoNodeState } from '../../../../common/types/cardano-node.types';

const messages = defineMessages({
  nodeIsRunning: {
    id: 'status.icons.nodeIsRunning',
    defaultMessage: 'Node is running!',
    description: 'Message "Node is running" on the status icon tooltip',
  },
  nodeIsStarting: {
    id: 'status.icons.nodeIsStarting',
    defaultMessage: 'Node is starting!',
    description: 'Message "Node is starting" on the status icon tooltip',
  },
  nodeIsExiting: {
    id: 'status.icons.nodeIsExiting',
    defaultMessage: 'Node is exiting!',
    description: 'Message "Node is exiting" on the status icon tooltip',
  },
  nodeIsStopping: {
    id: 'status.icons.nodeIsStopping',
    defaultMessage: 'Node is stopping!',
    description: 'Message "Node is stopping" on the status icon tooltip',
  },
  nodeHasStopped: {
    id: 'status.icons.nodeHasStopped',
    defaultMessage: 'Node has stopped!',
    description: 'Message "Node has stopped" on the status icon tooltip',
  },
  nodeIsUpdating: {
    id: 'status.icons.nodeIsUpdating',
    defaultMessage: 'Node is updating!',
    description: 'Message "Node is updating" on the status icon tooltip',
  },
  nodeHasBeenUpdated: {
    id: 'status.icons.nodeHasBeenUpdated',
    defaultMessage: 'Node has been updated!',
    description: 'Message "Node has been updated" on the status icon tooltip',
  },
  nodeHasCrashed: {
    id: 'status.icons.nodeHasCrashed',
    defaultMessage: 'Node has crashed!',
    description: 'Message "Node has crashed" on the status icon tooltip',
  },
  nodeHasErrored: {
    id: 'status.icons.nodeHasErrored',
    defaultMessage: 'Node has errored!',
    description: 'Message "Node has errored" on the status icon tooltip',
  },
  nodeIsUnrecoverable: {
    id: 'status.icons.nodeIsUnrecoverable',
    defaultMessage: 'Node is unrecoverable!',
    description: 'Message "Node is unrecoverable" on the status icon tooltip',
  },
  nodeIsResponding: {
    id: 'status.icons.nodeIsResponding',
    defaultMessage: 'Node is responding!',
    description: 'Message "Node is responding" on the status icon tooltip',
  },
  nodeIsNotResponding: {
    id: 'status.icons.nodeIsNotResponding',
    defaultMessage: 'Node is not responding!',
    description: 'Message "Node is not responding" on the status icon tooltip',
  },
  checkingIfNodeIsResponding: {
    id: 'status.icons.checkingIfNodeIsResponding',
    defaultMessage: 'Checking if Node is responding!',
    description: 'Message "Checking if Node is responding" on the status icon tooltip',
  },
  nodeIsSubscribed: {
    id: 'status.icons.nodeIsSubscribed',
    defaultMessage: 'Node is subscribed!',
    description: 'Message "Node is subscribed" on the status icon tooltip',
  },
  nodeIsNotSubscribed: {
    id: 'status.icons.nodeIsNotSubscribed',
    defaultMessage: 'Node is not subscribed!',
    description: 'Message "Node is not subscribed" on the status icon tooltip',
  },
  checkYourInternetConnection: {
    id: 'status.icons.checkYourInternetConnection',
    defaultMessage: 'Check your Internet connection!',
    description: 'Message "Check your Internet connection" on the status icon tooltip',
  },
  checkingIfNodeIsSubscribed: {
    id: 'status.icons.checkingIfNodeIsSubscribed',
    defaultMessage: 'Checking if Node is subscribed!',
    description: 'Message "Checking if Node is subscribed" on the status icon tooltip',
  },
  nodeTimeIsCorrect: {
    id: 'status.icons.nodeTimeIsCorrect',
    defaultMessage: 'Node time is correct!',
    description: 'Message "Node time is correct" on the status icon tooltip',
  },
  nodeTimeIsNotCorrect: {
    id: 'status.icons.nodeTimeIsNotCorrect',
    defaultMessage: 'Node time is not correct!',
    description: 'Message "Node time is not correct" on the status icon tooltip',
  },
  checkingIfNodeTimeIsCorrect: {
    id: 'status.icons.checkingIfNodeTimeIsCorrect',
    defaultMessage: 'Checking if Node time is correct!',
    description: 'Message "Checking if Node time is correct" on the status icon tooltip',
  },
  nodeIsSyncing: {
    id: 'status.icons.nodeIsSyncing',
    defaultMessage: 'Node is syncing!',
    description: 'Message "Node is syncing" on the status icon tooltip',
  },
  nodeIsNotSyncing: {
    id: 'status.icons.nodeIsNotSyncing',
    defaultMessage: 'Node is not syncing!',
    description: 'Message "Node is not syncing" on the status icon tooltip',
  },
  checkingIfNodeIsSyncing: {
    id: 'status.icons.checkingIfNodeIsSyncing',
    defaultMessage: 'Checking if Node is syncing!',
    description: 'Message "Checking if Node is syncing" on the status icon tooltip',
  },
});


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

const CARDANO_STATES = {
  [CardanoNodeStates.STARTING]: 'is',
  [CardanoNodeStates.RUNNING]: 'is',
  [CardanoNodeStates.EXITING]: 'is',
  [CardanoNodeStates.STOPPING]: 'is',
  [CardanoNodeStates.STOPPED]: 'has',
  [CardanoNodeStates.UPDATING]: 'is',
  [CardanoNodeStates.UPDATED]: 'has been',
  [CardanoNodeStates.CRASHED]: 'has',
  [CardanoNodeStates.ERRORED]: 'has',
  [CardanoNodeStates.UNRECOVERABLE]: 'has',
};

export default class StatusIcon extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getNodeName = (paramName: string) => [
    typeof this.props[paramName] === 'undefined' ? 'checkingIf' : '',
    'node',
    paramName === 'isNodeTimeCorrect' ? 'time' : '',
  ];

  getStatus = (paramName: string) => {
    const paramValue: boolean = this.props[paramName];
    let status = 'is';
    if (paramName === 'nodeState') {
      status = CARDANO_STATES[paramValue];
    } else if (paramValue === false) {
      status = 'isNot';
    }
    return status;
  };

  getParamPrettyName = (paramName: string) => (
    paramName === 'nodeState'
      ? this.props[paramName] || CardanoNodeStates.STARTING
      : paramName
        .replace('isNodeTime', '')
        .replace('isNode', '')
  );

  getMessageParam = (paramName: string) => camelCase([
    this.getNodeName(paramName),
    this.getStatus(paramName),
    this.getParamPrettyName(paramName),
  ]);

  getTip = (paramName: string) => {
    const messageParam = this.getMessageParam(paramName);
    const message = messages[messageParam];
    return message && (
      <FormattedHTMLMessage
        {...message}
      />
    );
  }

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
      styles.icon,
      styles[`icon-${status}`],
      isNodeSyncing ? styles.syncing : styles.loading,
    ]);
  };

  getTooltipClassname = (paramName: string) => {
    const paramValue = this.props[paramName];
    return classNames([
      styles.tooltip,
      typeof paramValue === 'undefined' ? tooltipStyles.ellipsis : null,
      this.isDisabled(paramName) ? tooltipStyles.disabled : null,
    ]);
  }

  isDisabled = (paramName: string) => (
    paramName !== 'nodeState' && this.props.nodeState !== CardanoNodeStates.RUNNING
  );

  getIconWithToolTip = (icon: string, paramName: string) => (
    <Tooltip
      skin={TooltipSkin}
      tip={this.getTip(paramName)}
      className={this.getTooltipClassname(paramName)}
    >
      <SVGInline svg={icon} className={this.getClassName(paramName)} />
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
