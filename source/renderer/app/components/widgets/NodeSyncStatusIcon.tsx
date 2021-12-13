import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import { formattedNumber } from '../../utils/formatters';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/top-bar/no... Remove this comment to see the full error message
import spinnerIcon from '../../assets/images/top-bar/node-sync-spinner.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/top-bar/no... Remove this comment to see the full error message
import syncedIcon from '../../assets/images/top-bar/node-sync-synced.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NodeSyncStatusIcon.scss' or ... Remove this comment to see the full error message
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
  isSynced: boolean;
  syncPercentage: number;
  hasTadaIcon?: boolean;
};
export default class NodeSyncStatusIcon extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isSynced, syncPercentage, hasTadaIcon } = this.props;
    const { intl } = this.context;
    const statusIcon = isSynced ? syncedIcon : spinnerIcon;
    const componentClasses = classNames([
      styles.component,
      isSynced ? styles.synced : styles.syncing,
      hasTadaIcon ? styles.hasTadaIcon : null,
    ]);
    const percentage = syncPercentage.toFixed(syncPercentage === 100 ? 0 : 2);
    return (
      <div className={componentClasses}>
        <PopOver
          content={intl.formatMessage(messages.blocksSynced, {
            percentage: formattedNumber(percentage),
          })}
        >
          <div className={styles.questionMark}>
            <SVGInline className={styles.icon} svg={statusIcon} />
          </div>
        </PopOver>
      </div>
    );
  }
}
