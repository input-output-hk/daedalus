// @ts-nocheck
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import { formattedNumber } from '../../utils/formatters';
import spinnerIcon from '../../assets/images/top-bar/node-sync-spinner.inline.svg';
import syncedIcon from '../../assets/images/top-bar/node-sync-synced.inline.svg';
import styles from './NodeSyncStatusIcon.scss';
import { TOOLTIP_DELAY } from '../../config/timingConfig';

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
      !isSynced && styles.syncing,
      hasTadaIcon && styles.hasTadaIcon,
    ]);
    const percentage = syncPercentage.toFixed(syncPercentage === 100 ? 0 : 2);
    return (
      <div className={componentClasses}>
        <PopOver
          delay={TOOLTIP_DELAY}
          offset={[0, 10]}
          content={intl.formatMessage(messages.blocksSynced, {
            percentage: formattedNumber(percentage),
          })}
        >
          <div>
            <SVGInline className={styles.icon} svg={statusIcon} />
          </div>
        </PopOver>
      </div>
    );
  }
}
