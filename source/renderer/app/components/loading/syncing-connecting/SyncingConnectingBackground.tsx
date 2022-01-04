import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SyncingConnectingBackground.... Remove this comment to see the full error message
import styles from './SyncingConnectingBackground.scss';

type Props = {
  hasLoadedCurrentTheme: boolean;
  isConnecting: boolean;
  isSyncing: boolean;
};

@observer
class SyncingConnectingBackground extends Component<Props> {
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

export default SyncingConnectingBackground;
