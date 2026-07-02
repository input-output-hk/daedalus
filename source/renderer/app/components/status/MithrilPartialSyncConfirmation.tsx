import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';

import Dialog from '../widgets/Dialog';
import DialogBackButton from '../widgets/DialogBackButton';
import mithrilSyncProcessSummaryMessages from './MithrilSyncProcessSummary.messages';
import styles from './DaedalusDiagnostics.scss';

const messages = defineMessages({
  title: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationTitle',
    defaultMessage: '!!!Before Mithril Sync begins',
    description: 'Title for the Mithril partial sync confirmation view',
  },
  behind: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind',
    defaultMessage:
      '!!!Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.',
    description:
      'Behind-ness context line (epochs behind the blockchain tip) for the Mithril partial sync confirmation modal',
  },
  behindUnknown: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindUnknown',
    defaultMessage:
      '!!!Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster.',
    description:
      'Behind-ness context line shown when the epochs-behind figure is unavailable (tips/epoch missing)',
  },
  cancel: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationCancel',
    defaultMessage: '!!!Back to diagnostics',
    description:
      'Cancel button label for the Mithril partial sync confirmation view',
  },
  confirm: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationConfirm',
    defaultMessage: '!!!Start Mithril Sync',
    description:
      'Confirm button label for the Mithril partial sync confirmation view',
  },
});

type Props = {
  isActionBlocked: boolean;
  startError: string | null;
  behindByEpochs?: number;
  onCancel: () => void;
  onConfirm: () => void;
};

export default class MithrilPartialSyncConfirmation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActionBlocked, startError, behindByEpochs, onCancel, onConfirm } =
      this.props;
    const { intl } = this.context;

    const hasBehindFigure =
      typeof behindByEpochs === 'number' && Number.isFinite(behindByEpochs);

    return (
      <Dialog
        className={styles.mithrilPartialSyncConfirmationDialog}
        title={intl.formatMessage(messages.title)}
        closeOnOverlayClick
        primaryButtonAutoFocus
        backButton={<DialogBackButton onBack={onCancel} />}
        onClose={onCancel}
        actions={[
          {
            label: intl.formatMessage(messages.cancel),
            onClick: onCancel,
            className: styles.mithrilPartialSyncConfirmationSecondaryButton,
          },
          {
            label: intl.formatMessage(messages.confirm),
            primary: true,
            disabled: isActionBlocked,
            onClick: onConfirm,
            className: styles.mithrilPartialSyncConfirmationPrimaryButton,
          },
        ]}
      >
        <div className={styles.mithrilPartialSyncConfirmationBody}>
          <p className={styles.mithrilPartialSyncConfirmationBehind}>
            {hasBehindFigure
              ? intl.formatMessage(messages.behind, {
                  epochs: behindByEpochs,
                })
              : intl.formatMessage(messages.behindUnknown)}
          </p>

          <p className={styles.mithrilPartialSyncConfirmationRecovery}>
            {intl.formatMessage(
              mithrilSyncProcessSummaryMessages.processSummary
            )}
          </p>

          {startError ? (
            <div className={styles.mithrilPartialSyncConfirmationError}>
              {startError}
            </div>
          ) : null}
        </div>
      </Dialog>
    );
  }
}
