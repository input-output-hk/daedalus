import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StatusIcons.scss' or its cor... Remove this comment to see the full error message
import styles from './StatusIcons.scss';
import { CardanoNodeStates } from '../../../../../common/types/cardano-node.types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/node-st... Remove this comment to see the full error message
import nodeStateIcon from '../../../assets/images/node-state-icon.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/is-node... Remove this comment to see the full error message
import isNodeRespondingIcon from '../../../assets/images/is-node-responding-icon.inline.svg';
// import isNodeSubscribedIcon from '../../../assets/images/is-node-subscribed-icon.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/is-node... Remove this comment to see the full error message
import isNodeTimeCorrectIcon from '../../../assets/images/is-node-time-correct-icon.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/is-node... Remove this comment to see the full error message
import isNodeSyncingIcon from '../../../assets/images/is-node-syncing-icon.inline.svg';
import type { CardanoNodeState } from '../../../../../common/types/cardano-node.types';

const messages = defineMessages({
  nodeIsRunning: {
    id: 'status.icons.nodeIsRunning',
    defaultMessage: '!!!Cardano node is running!',
    description: 'Message "Cardano node is running" on the status icon tooltip',
  },
  nodeIsStarting: {
    id: 'status.icons.nodeIsStarting',
    defaultMessage: '!!!Cardano node is starting!',
    description: 'Message "Node is starting" on the status icon tooltip',
  },
  nodeIsExiting: {
    id: 'status.icons.nodeIsExiting',
    defaultMessage: '!!!Cardano node is exiting!',
    description: 'Message "Cardano node is exiting" on the status icon tooltip',
  },
  nodeIsStopping: {
    id: 'status.icons.nodeIsStopping',
    defaultMessage: '!!!Cardano node is stopping!',
    description:
      'Message "Cardano node is stopping" on the status icon tooltip',
  },
  nodeHasStopped: {
    id: 'status.icons.nodeHasStopped',
    defaultMessage: '!!!Cardano node has stopped!',
    description:
      'Message "Cardano node has stopped" on the status icon tooltip',
  },
  nodeIsUpdating: {
    id: 'status.icons.nodeIsUpdating',
    defaultMessage: '!!!Cardano node is updating!',
    description:
      'Message "Cardano node is updating" on the status icon tooltip',
  },
  nodeHasBeenUpdated: {
    id: 'status.icons.nodeHasBeenUpdated',
    defaultMessage: '!!!Cardano node has been updated!',
    description:
      'Message "Cardano node has been updated" on the status icon tooltip',
  },
  nodeHasCrashed: {
    id: 'status.icons.nodeHasCrashed',
    defaultMessage: '!!!Cardano node has crashed!',
    description:
      'Message "Cardano node has crashed" on the status icon tooltip',
  },
  nodeHasErrored: {
    id: 'status.icons.nodeHasErrored',
    defaultMessage: '!!!Cardano node has errored!',
    description:
      'Message "Cardano node has errored" on the status icon tooltip',
  },
  nodeIsUnrecoverable: {
    id: 'status.icons.nodeIsUnrecoverable',
    defaultMessage: '!!!Cardano node is unrecoverable!',
    description:
      'Message "Cardano node is unrecoverable" on the status icon tooltip',
  },
  checkYourInternetConnection: {
    id: 'status.icons.checkYourInternetConnection',
    defaultMessage: '!!!Check your Internet connection!',
    description:
      'Message "Check your Internet connection" on the status icon tooltip',
  },
  isNodeRespondingOn: {
    id: 'status.icons.isNodeRespondingOn',
    defaultMessage: '!!!Cardano node is responding!',
    description:
      'Message "Cardano node is responding" on the status icon tooltip',
  },
  isNodeRespondingOff: {
    id: 'status.icons.isNodeRespondingOff',
    defaultMessage: '!!!Cardano node is not responding!',
    description:
      'Message "Cardano node is not responding" on the status icon tooltip',
  },
  isNodeRespondingLoading: {
    id: 'status.icons.isNodeRespondingLoading',
    defaultMessage: '!!!Checking if Cardano node is responding!',
    description:
      'Message "Checking if Cardano node is responding" on the status icon tooltip',
  },
  isNodeSubscribedOn: {
    id: 'status.icons.isNodeSubscribedOn',
    defaultMessage: '!!!Cardano node is subscribed!',
    description:
      'Message "Cardano node is subscribed" on the status icon tooltip',
  },
  isNodeSubscribedOff: {
    id: 'status.icons.isNodeSubscribedOff',
    defaultMessage: '!!!Cardano node is not subscribed!',
    description:
      'Message "Cardano node is not subscribed" on the status icon tooltip',
  },
  isNodeSubscribedLoading: {
    id: 'status.icons.isNodeSubscribedLoading',
    defaultMessage: '!!!Checking if Cardano node is subscribed!',
    description:
      'Message "Checking if Cardano node is subscribed" on the status icon tooltip',
  },
  isNodeTimeCorrectOn: {
    id: 'status.icons.isNodeTimeCorrectOn',
    defaultMessage: '!!!Cardano node time is correct!',
    description:
      'Message "Cardano node time is correct" on the status icon tooltip',
  },
  isNodeTimeCorrectOff: {
    id: 'status.icons.isNodeTimeCorrectOff',
    defaultMessage: '!!!Cardano node time is not correct!',
    description:
      'Message "Cardano node time is not correct" on the status icon tooltip',
  },
  isNodeTimeCorrectLoading: {
    id: 'status.icons.isNodeTimeCorrectLoading',
    defaultMessage: '!!!Checking if Cardano node time is correct!',
    description:
      'Message "Checking if Cardano node time is correct" on the status icon tooltip',
  },
  isNodeSyncingOn: {
    id: 'status.icons.isNodeSyncingOn',
    defaultMessage: '!!!Cardano node is syncing!',
    description: 'Message "Cardano node is syncing" on the status icon tooltip',
  },
  isNodeSyncingOff: {
    id: 'status.icons.isNodeSyncingOff',
    defaultMessage: '!!!Cardano node is not syncing!',
    description:
      'Message "Cardano node is not syncing" on the status icon tooltip',
  },
  isNodeSyncingLoading: {
    id: 'status.icons.isNodeSyncingLoading',
    defaultMessage: '!!!Checking if Cardano node is syncing!',
    description:
      'Message "Checking if Cardano node is syncing" on the status icon tooltip',
  },
});
type Props = {
  onIconClick: (...args: Array<any>) => any;
  nodeState: CardanoNodeState | null | undefined;
  isNodeResponding?: boolean;
  isNodeSubscribed?: boolean;
  isNodeTimeCorrect?: boolean;
  isNodeSyncing?: boolean;
};
type TipParamValue = true | false | null | string;
const STATUS_CLASSNAMES: Record<string, any> = {
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
const NODE_STATE_MESSAGES = {
  [CardanoNodeStates.RUNNING]: messages.nodeIsRunning,
  [CardanoNodeStates.STARTING]: messages.nodeIsStarting,
  [CardanoNodeStates.EXITING]: messages.nodeIsExiting,
  [CardanoNodeStates.STOPPING]: messages.nodeIsStopping,
  [CardanoNodeStates.STOPPED]: messages.nodeHasStopped,
  [CardanoNodeStates.UPDATING]: messages.nodeIsUpdating,
  [CardanoNodeStates.UPDATED]: messages.nodeHasBeenUpdated,
  [CardanoNodeStates.CRASHED]: messages.nodeHasCrashed,
  [CardanoNodeStates.ERRORED]: messages.nodeHasErrored,
  [CardanoNodeStates.UNRECOVERABLE]: messages.nodeIsUnrecoverable,
};
const VARIABLE_VALUES = {
  true: 'On',
  false: 'Off',
  undefined: 'Loading',
  null: 'IsStarting',
};
export default class StatusIcons extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  getTip = (paramName: string, paramValue: TipParamValue) => {
    let message;

    if (paramName === 'nodeState' && paramValue) {
      message = NODE_STATE_MESSAGES[String(paramValue)];
    } else {
      message = messages[`${paramName}${VARIABLE_VALUES[String(paramValue)]}`];
    }

    return message && <FormattedHTMLMessage {...message} />;
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
      styles.icon,
      styles[`icon-${status}`],
      styles[`icon-${paramName}`],
      isNodeSyncing ? styles.syncing : styles.loading,
    ]);
  };
  getTooltipClassname = (paramName: string) => {
    const paramValue = this.props[paramName];
    return classNames([
      styles.tooltip,
      typeof paramValue === 'undefined' ? styles.ellipsis : null,
      this.isDisabled(paramName) ? styles.disabled : null,
    ]);
  };
  isDisabled = (paramName: string) =>
    paramName !== 'nodeState' &&
    this.props.nodeState !== CardanoNodeStates.RUNNING;
  getIconWithPopover = (icon: string, paramName: string) => (
    <PopOver
      themeVariables={{
        '--rp-pop-over-bg-color':
          'var(--theme-loading-status-icons-tooltip-color)',
        '--rp-pop-over-border-radius': '5px',
        '--rp-bubble-padding': '6px 12px 7px',
      }}
      contentClassName={this.getTooltipClassname(paramName)}
      key={paramName}
      content={this.getTip(paramName, this.props[paramName])}
    >
      <button className={styles.iconButton} onClick={this.props.onIconClick}>
        <SVGInline svg={icon} className={this.getClassName(paramName)} />
      </button>
    </PopOver>
  );

  render() {
    return (
      <div className={styles.component}>
        {[
          this.getIconWithPopover(nodeStateIcon, 'nodeState'),
          this.getIconWithPopover(isNodeRespondingIcon, 'isNodeResponding'), // this.getIconWithPopover(isNodeSubscribedIcon, 'isNodeSubscribed'),
          this.getIconWithPopover(isNodeTimeCorrectIcon, 'isNodeTimeCorrect'),
          this.getIconWithPopover(isNodeSyncingIcon, 'isNodeSyncing'),
        ]}
      </div>
    );
  }
}
