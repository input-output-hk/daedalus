import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';

import Dialog from '../widgets/Dialog';
import DialogBackButton from '../widgets/DialogBackButton';
import styles from './DaedalusDiagnostics.scss';

const messages = defineMessages({
  title: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationTitle',
    defaultMessage: '!!!Before Mithril Sync begins',
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
      '!!!If Mithril Sync succeeds, Daedalus will restart Cardano node automatically and standard syncing will resume.',
    description: 'Success copy for the Mithril partial sync confirmation view',
  },
  behind: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind',
    defaultMessage:
      '!!!Your node is about {epochs} epochs behind the blockchain tip. Mithril Sync can restore verified chain data to help it catch up faster than waiting for standard sync.',
    description:
      'Behind-ness context line (epochs behind the blockchain tip) for the Mithril partial sync confirmation modal',
  },
  behindUnknown: {
    id:
      'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindUnknown',
    defaultMessage: '!!!Your node is behind the latest verified snapshot.',
    description:
      'Behind-ness context line shown when the epochs-behind figure is unavailable (tips/epoch missing)',
  },
  stepStop: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepStop',
    defaultMessage: '!!!Daedalus stops Cardano node.',
    description:
      'What-happens step 1 for the Mithril partial sync confirmation modal',
  },
  stepDownload: {
    id:
      'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepDownload',
    defaultMessage: '!!!Daedalus downloads and verifies Mithril data.',
    description:
      'What-happens step 2 for the Mithril partial sync confirmation modal',
  },
  stepRestart: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepRestart',
    defaultMessage:
      '!!!Daedalus restarts Cardano node automatically and standard syncing resumes.',
    description:
      'What-happens step 3 for the Mithril partial sync confirmation modal',
  },
  recovery: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationRecovery',
    defaultMessage:
      "!!!You don't need to choose now. If the sync fails, the progress screen will then offer the available recovery options — such as retrying, restarting normally on your current data, or wiping chain data and running a full Mithril sync.",
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
    const {
      isActionBlocked,
      startError,
      behindByEpochs,
      onCancel,
      onConfirm,
    } = this.props;
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

          <ol className={styles.mithrilPartialSyncConfirmationSteps}>
            <li>{intl.formatMessage(messages.stepStop)}</li>
            <li>{intl.formatMessage(messages.stepDownload)}</li>
            <li>{intl.formatMessage(messages.stepRestart)}</li>
          </ol>

          <p className={styles.mithrilPartialSyncConfirmationRecovery}>
            {intl.formatMessage(messages.recovery)}
          </p>

          {startError ? <div className={styles.error}>{startError}</div> : null}
        </div>
      </Dialog>
    );
  }
}
