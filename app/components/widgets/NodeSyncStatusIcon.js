import React, { Component, PropTypes } from 'react';
import nodeSyncSpinner from '../../assets/images/app-bar/node-sync-spinner.png';
import nodeSyncSynced from '../../assets/images/app-bar/node-sync-synced.png';
import styles from './NodeSyncStatusIcon.scss';

export default class NodeSyncStatusIcon extends Component {

  static propTypes = {
    isSyncing: PropTypes.bool.isRequired,
    isSynced: PropTypes.bool.isRequired
  };

  render() {
    const { isSyncing, isSynced } = this.props;
    return (
      <div
      >
        {isSynced && (
          <img className={styles.synced} src={nodeSyncSynced} role="presentation" />
        )}
        {isSyncing && (
          <img className={styles.syncing} src={nodeSyncSpinner} role="presentation" />
        )}
      </div>
    );
  }
}
