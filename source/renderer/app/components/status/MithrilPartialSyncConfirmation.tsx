import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';

import styles from './DaedalusDiagnostics.scss';

const messages = defineMessages({
  title: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationTitle',
    defaultMessage: '!!!Before Mithril partial sync begins',
    description: 'Title for the Mithril partial sync confirmation view',
  },
  intro: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationIntro',
    defaultMessage:
      '!!!Daedalus will stop Cardano node automatically, then download and restore verified Mithril data.',
    description:
      'Introductory copy for the Mithril partial sync confirmation view',
  },
  success: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationSuccess',
    defaultMessage:
      '!!!If Mithril partial sync succeeds, Daedalus will restart Cardano node automatically and normal syncing will resume.',
    description: 'Success copy for the Mithril partial sync confirmation view',
  },
  recovery: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationRecovery',
    defaultMessage:
      '!!!If the attempt fails, Daedalus can offer retry partial sync, restart normally on the current database, or wipe chain data and do a full Mithril sync.',
    description:
      'Failure recovery copy for the Mithril partial sync confirmation view',
  },
  cancel: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationCancel',
    defaultMessage: '!!!Back to diagnostics',
    description:
      'Cancel button label for the Mithril partial sync confirmation view',
  },
  confirm: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationConfirm',
    defaultMessage: '!!!Start Mithril partial sync',
    description:
      'Confirm button label for the Mithril partial sync confirmation view',
  },
});

type Props = {
  isActionBlocked: boolean;
  startError: string | null;
  onCancel: () => void;
  onConfirm: () => void;
};

export default class MithrilPartialSyncConfirmation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActionBlocked, startError, onCancel, onConfirm } = this.props;
    const { intl } = this.context;

    return (
      <div className={styles.mithrilPartialSyncConfirmation}>
        <h1 className={styles.mithrilPartialSyncConfirmationTitle}>
          {intl.formatMessage(messages.title)}
        </h1>

        <div className={styles.mithrilPartialSyncConfirmationBody}>
          <p>{intl.formatMessage(messages.intro)}</p>
          <p>{intl.formatMessage(messages.success)}</p>
          <p>{intl.formatMessage(messages.recovery)}</p>
        </div>

        {startError ? <div className={styles.error}>{startError}</div> : null}

        <div className={styles.mithrilPartialSyncConfirmationActions}>
          <button
            className={styles.mithrilPartialSyncConfirmationCancelButton}
            onClick={onCancel}
            type="button"
          >
            {intl.formatMessage(messages.cancel)}
          </button>
          <button
            className={styles.mithrilPartialSyncConfirmationCancelButton}
            disabled={isActionBlocked}
            onClick={onConfirm}
            type="button"
          >
            {intl.formatMessage(messages.confirm)}
          </button>
        </div>
      </div>
    );
  }
}
