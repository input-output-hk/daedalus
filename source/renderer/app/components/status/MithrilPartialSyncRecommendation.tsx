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

    return (
      <div
        className={classNames(styles.layoutData, styles.mithrilPartialSyncData)}
      >
        <div className={styles.mithrilPartialSyncRecommendation}>
          <PopOver
            maxWidth={280}
            content={
              <div className={styles.tooltipLabelWrapper}>
                {intl.formatMessage(messages.recommendation)}
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
