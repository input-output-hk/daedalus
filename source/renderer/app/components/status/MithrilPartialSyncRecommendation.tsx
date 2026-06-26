import React, { Component } from 'react';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';

import styles from './DaedalusDiagnostics.scss';

const messages = defineMessages({
  recommendation: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncRecommendation',
    defaultMessage:
      '!!!If Cardano node catch-up is taking longer than you want, Mithril Sync can restore verified chain data to help it catch up faster.',
    description:
      'Recommendation copy shown in diagnostics near sync status for Mithril partial sync',
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
  buttonHintReady: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncButtonHintReady',
    defaultMessage:
      '!!!Review what will happen before Daedalus starts Mithril Sync.',
    description:
      'Hint text beneath the Mithril partial sync diagnostics button before confirmation opens',
  },
});

type Props = {
  isActionBlocked: boolean;
  onShowConfirmation: () => void;
};

export default class MithrilPartialSyncRecommendation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActionBlocked, onShowConfirmation } = this.props;
    const { intl } = this.context;
    const hintMessage = isActionBlocked
      ? messages.buttonHintBlocked
      : messages.buttonHintReady;

    return (
      <div
        className={classNames(styles.layoutData, styles.mithrilPartialSyncData)}
      >
        <div className={styles.mithrilPartialSyncRecommendation}>
          <div className={styles.mithrilPartialSyncRecommendationCopy}>
            {intl.formatMessage(messages.recommendation)}
          </div>
          <button
            className={styles.mithrilPartialSyncButton}
            disabled={isActionBlocked}
            onClick={onShowConfirmation}
            type="button"
          >
            {intl.formatMessage(messages.buttonLabel)}
          </button>
          <div className={styles.mithrilPartialSyncHint}>
            {intl.formatMessage(hintMessage)}
          </div>
        </div>
      </div>
    );
  }
}
