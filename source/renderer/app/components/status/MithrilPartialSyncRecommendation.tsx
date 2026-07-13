import React, { Component } from 'react';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';

import styles from './DaedalusDiagnostics.scss';

const messages = defineMessages({
  recommendation: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation',
    defaultMessage:
      '!!!If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync.',
    description:
      'Tooltip copy shown on hover over the Mithril Sync button in diagnostics',
  },
  recommendationAtOrPastSnapshot: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationAtOrPastSnapshot',
    defaultMessage:
      '!!!Your node is at or past the latest Mithril snapshot. Blockchain Sync will finish the remaining blocks on its own. If sync seems slow or runs into verification issues, Mithril Sync can restore a verified ledger state.',
    description:
      'Tooltip copy for the Mithril Sync button in diagnostics when the node is at or past the latest certified snapshot',
  },
  recommendationNearTip: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationNearTip',
    defaultMessage:
      '!!!Your node is close to the blockchain tip. You can still use Mithril Sync to restore verified chain data.',
    description:
      'Tooltip copy for the Mithril Sync button in diagnostics when the node is not significantly behind',
  },
  recommendationUnknown: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendationUnknown',
    defaultMessage:
      '!!!Daedalus could not check how far behind your node is. You can still use Mithril Sync to restore verified chain data.',
    description:
      'Tooltip copy for the Mithril Sync button in diagnostics when the behind-ness check failed',
  },
  buttonLabel: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncButtonLabel',
    defaultMessage: '!!!Mithril Sync',
    description:
      'CTA label that opens the Mithril partial sync confirmation from diagnostics',
  },
  buttonHintBlocked: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncButtonHintBlocked',
    defaultMessage: '!!!Unavailable while Mithril work is already active.',
    description:
      'Hint text beneath the disabled Mithril partial sync diagnostics button while Mithril work is active',
  },
});

export type MithrilAvailabilityVariant =
  | 'behind'
  | 'near-tip'
  | 'at-or-past-snapshot'
  | 'availability-unknown';

type Props = {
  isActionBlocked: boolean;
  variant: MithrilAvailabilityVariant;
  onShowConfirmation: () => void;
};

export default class MithrilPartialSyncRecommendation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActionBlocked, variant, onShowConfirmation } = this.props;
    const { intl } = this.context;

    let tooltipMessage = messages.recommendation;
    if (variant === 'near-tip') {
      tooltipMessage = messages.recommendationNearTip;
    } else if (variant === 'at-or-past-snapshot') {
      tooltipMessage = messages.recommendationAtOrPastSnapshot;
    } else if (variant === 'availability-unknown') {
      tooltipMessage = messages.recommendationUnknown;
    }

    return (
      <div
        className={classNames(styles.layoutData, styles.mithrilPartialSyncData)}
      >
        <div className={styles.mithrilPartialSyncRecommendation}>
          <PopOver
            maxWidth={280}
            content={
              <div className={styles.tooltipLabelWrapper}>
                {intl.formatMessage(tooltipMessage)}
              </div>
            }
          >
            <button
              className={styles.mithrilPartialSyncButton}
              disabled={isActionBlocked}
              onClick={onShowConfirmation}
              type="button"
            >
              {intl.formatMessage(messages.buttonLabel)}
            </button>
          </PopOver>
          {isActionBlocked && (
            <div className={styles.mithrilPartialSyncHint}>
              {intl.formatMessage(messages.buttonHintBlocked)}
            </div>
          )}
        </div>
      </div>
    );
  }
}
