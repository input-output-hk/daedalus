// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import styles from './StatusIcons.scss';
import isNodeRunningIcon from '../../assets/images/is-node-running-icon.inline.svg';
import isNodeRespondingIcon from '../../assets/images/is-node-responding-icon.inline.svg';
import isNodeSubscribedIcon from '../../assets/images/is-node-subscribed-icon.inline.svg';
import isNodeSyncingIcon from '../../assets/images/is-node-syncing-icon.inline.svg';
import isNodeTimeCorrectIcon from '../../assets/images/is-node-time-correct-icon.inline.svg';

type Props = {
  isNodeRunning: boolean,
  isNodeSubscribed: boolean,
  isNodeSyncing: boolean,
  isNodeInSync: boolean,
  isNodeTimeCorrect: boolean,
};

export default class StatusIcon extends Component<Props> {

  statusClassNames: Object = {
    true: 'on',
    false: 'off',
    undefined: 'unloaded'
  };

  getClassName = (param: boolean) => styles[`icon-${this.statusClassNames[param]}`];

  render() {
    const {
      isNodeRunning, isNodeResponding, isNodeSubscribed, isNodeSyncing, isNodeTimeCorrect
    } = this.props;

    return (
      <div className={styles.component}>
        <SVGInline svg={isNodeRunningIcon} className={this.getClassName(isNodeRunning)} />
        <SVGInline svg={isNodeRespondingIcon} className={this.getClassName(isNodeResponding)} />
        <SVGInline svg={isNodeSubscribedIcon} className={this.getClassName(isNodeSubscribed)} />
        <SVGInline svg={isNodeSyncingIcon} className={this.getClassName(isNodeSyncing)} />
        <SVGInline svg={isNodeTimeCorrectIcon} className={this.getClassName(isNodeTimeCorrect)} />
      </div>
    );
  }
}
