// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SyncingConnectingBackground.scss';

type Props = {
  hasLoadedCurrentTheme: boolean,
  isConnecting: boolean,
  isSyncing: boolean,
};

@observer
export default class SyncingConnectingBackground extends Component<Props> {
  render() {
    const { isConnecting, isSyncing, hasLoadedCurrentTheme } = this.props;
    const componentStyles = classNames([
      styles.component,
      !hasLoadedCurrentTheme ? styles.isLoadingTheme : null,
      isConnecting ? styles.isConnecting : null,
      isSyncing ? styles.isSyncing : null,
    ]);
    return <div className={componentStyles} />;
  }
}
