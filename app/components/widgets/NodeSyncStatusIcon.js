import React, { Component, PropTypes } from 'react';
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

export default class NodeSyncStatusIcon extends Component {

  static propTypes = {
    isSynced: PropTypes.bool.isRequired,
    syncPercentage: PropTypes.number.isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired
  };

  render() {
    const { isSynced, syncPercentage } = this.props;
    const { intl } = this.context;
    const statusIcon = isSynced ? syncedIcon : spinnerIcon;
    const componentClasses = classNames([
      styles.component,
      isSynced ? styles.synced : styles.syncing
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
