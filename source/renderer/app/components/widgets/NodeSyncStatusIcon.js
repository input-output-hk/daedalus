// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import spinnerIcon from '../../assets/images/top-bar/node-sync-spinner.inline.svg';
import syncedIcon from '../../assets/images/top-bar/node-sync-synced.inline.svg';
import styles from './NodeSyncStatusIcon.scss';

const messages = defineMessages({
  blocksSynced: {
    id: 'cardano.node.sync.status.blocksSynced',
    defaultMessage: '!!!Blocks synced {percentage}%',
    description:
      'Label for the blocks synced info overlay on node sync status icon.',
  },
});

type Props = {
  isSynced: boolean,
  syncPercentage: number,
};

export default class NodeSyncStatusIcon extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isSynced, syncPercentage } = this.props;
    const { intl } = this.context;
    const statusIcon = isSynced ? syncedIcon : spinnerIcon;
    const componentClasses = classNames([
      styles.component,
      isSynced ? styles.synced : styles.syncing,
    ]);
    const percentage = syncPercentage.toFixed(syncPercentage === 100 ? 0 : 2);

    return (
      <div className={componentClasses}>
        <SVGInline className={styles.icon} svg={statusIcon} />
        <div className={styles.info}>
          {intl.formatMessage(messages.blocksSynced, {
            percentage,
          })}
        </div>
      </div>
    );
  }
}
