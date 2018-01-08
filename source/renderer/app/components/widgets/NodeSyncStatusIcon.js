import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import spinnerIcon from '../../assets/images/top-bar/node-sync-spinner.png';
import syncedIcon from '../../assets/images/top-bar/node-sync-synced.png';
import styles from './NodeSyncStatusIcon.scss';

const messages = defineMessages({
  blocksSynced: {
    id: 'cardano.node.sync.status.blocksSynced',
    defaultMessage: '!!!Blocks synced {percentage}%',
    description: 'Label for the blocks synced info overlay on node sync status icon.'
  },
});

type Props = {
  networkStatus: {
    isSynced: boolean,
    syncPercentage: number,
  },
  isMainnet: boolean,
};

export default class NodeSyncStatusIcon extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  render() {
    const { networkStatus, isMainnet } = this.props;
    const { isSynced, syncPercentage } = networkStatus;
    const { intl } = this.context;
    const statusIcon = isSynced ? syncedIcon : spinnerIcon;
    const componentClasses = classNames([
      styles.component,
      isSynced ? styles.synced : styles.syncing,
      isMainnet && styles.mainnet,
    ]);
    return (
      <div className={componentClasses}>
        <img className={styles.icon} src={statusIcon} role="presentation" />
        <div className={styles.info}>
          {intl.formatMessage(messages.blocksSynced, { percentage: syncPercentage.toFixed(0) })}
        </div>
      </div>
    );
  }
}
