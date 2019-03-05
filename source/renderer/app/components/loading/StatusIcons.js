// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './StatusIcons.scss';
import isNodeRunningIcon from '../../assets/images/is-node-running-icon.inline.svg';
import isNodeRespondingIcon from '../../assets/images/is-node-responding-icon.inline.svg';
import isNodeSubscribedIcon from '../../assets/images/is-node-subscribed-icon.inline.svg';
import isNodeSyncingIcon from '../../assets/images/is-node-syncing-icon.inline.svg';
import isNodeTimeCorrectIcon from '../../assets/images/is-node-time-correct-icon.inline.svg';

type Props = {
  isNodeRunning?: boolean,
  isNodeResponding?: boolean,
  isNodeSubscribed?: boolean,
  isNodeSyncing?: boolean,
  isNodeTimeCorrect?: boolean,
};

export default class StatusIcon extends Component<Props> {

  statusClassNames: Object = {
    true: 'on',
    false: 'off',
    undefined: 'unloaded'
  };

  getClassName = (param?: boolean) => {
    const { isNodeRunning, isNodeSyncing } = this.props;
    // If {!isNodeRunning} it displays the icons without opacity
    // Whether {isNodeSyncing} it displays the icons for syncing or loading screen
    let status = this.statusClassNames[param];
    if (!isNodeRunning) status = 'unknown';
    return classNames([
      styles[`icon-${status}`],
      isNodeSyncing ? styles.syncing : styles.loading
    ]);
  };

  render() {
    const {
      isNodeRunning,
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeTimeCorrect
    } = this.props;

    return (
      <div className={styles.component}>
        <SVGInline svg={isNodeRunningIcon} className={this.getClassName(isNodeRunning)} />
        <SVGInline svg={isNodeRespondingIcon} className={this.getClassName(isNodeResponding)} />
        <SVGInline svg={isNodeSubscribedIcon} className={this.getClassName(isNodeSubscribed)} />
        <SVGInline svg={isNodeTimeCorrectIcon} className={this.getClassName(isNodeTimeCorrect)} />
        <SVGInline svg={isNodeSyncingIcon} className={this.getClassName(isNodeSyncing)} />
      </div>
    );
  }
}
