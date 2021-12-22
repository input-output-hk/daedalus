import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import Dialog from '../../../widgets/Dialog';
import DialogBackButton from '../../../widgets/DialogBackButton';
import WalletRestoreSteps from './WalletRestoreSteps';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRestoreDialog.scss' or... Remove this comment to see the full error message
import styles from './WalletRestoreDialog.scss';
import { RESTORE_WALLET_STEPS } from '../../../../config/walletRestoreConfig';
import type { DialogActionItems } from '../../../widgets/Dialog';
import type { RestoreWalletStep } from '../../../../types/walletRestoreTypes';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.restore.dialog.title',
    defaultMessage: '!!!Restore a wallet',
    description: 'Title "Create a new wallet" in the wallet restore form.',
  },
  dialogTitleSuccess: {
    id: 'wallet.restore.dialog.titleSuccess',
    defaultMessage: '!!!Restore a wallet',
    description: 'Title "Create a new wallet" in the wallet restore form.',
  },
  stepsCounter: {
    id: 'wallet.restore.dialog.stepsCounter',
    defaultMessage: '!!!Step {currentStep} of {totalSteps}',
    description: 'Step couters in the wallet restore dialog.',
  },
});
type Props = {
  stepNumber?: number;
  actions?: DialogActionItems;
  onClose?: (...args: Array<any>) => any;
  onBack?: (...args: Array<any>) => any;
  children: Node;
};
export default class WalletRestoreDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get filteredSteps(): Array<RestoreWalletStep> {
    return RESTORE_WALLET_STEPS.filter((stepId) => stepId !== 'success');
  }

  render() {
    const { intl } = this.context;
    const { actions, children, stepNumber, onClose, onBack } = this.props;
    const hasStep = stepNumber !== undefined;
    const title = hasStep
      ? intl.formatMessage(messages.dialogTitle)
      : intl.formatMessage(messages.dialogTitleSuccess);
    const currentStep = (stepNumber || 0) + 1;
    const totalSteps = this.filteredSteps.length;
    const subTitle = (
      <FormattedHTMLMessage
        {...messages.stepsCounter}
        values={{
          currentStep,
          totalSteps,
        }}
      />
    );
    return (
      <Dialog
        className={styles.component}
        title={title}
        subtitle={hasStep && subTitle}
        actions={actions}
        closeOnOverlayClick={false}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
        backButton={onBack && <DialogBackButton onBack={onBack} />}
      >
        {hasStep && <WalletRestoreSteps stepNumber={stepNumber || 0} />}
        {children}
      </Dialog>
    );
  }
}
