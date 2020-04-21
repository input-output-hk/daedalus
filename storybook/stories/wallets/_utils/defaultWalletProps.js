// @flow
import React from 'react';
import { text, boolean, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';

// Screens
import ChangeSpendingPasswordDialog from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import WalletRecoveryPhraseStep1Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep4Dialog';

export const defaultProps = {
  isDialogOpen: () => {},
  creationDate: new Date(),
  recoveryPhraseVerificationDate: new Date(),
  recoveryPhraseVerificationStatus: 'ok',
  recoveryPhraseVerificationStatusType: 'alreadyChecked',
  walletRecoveryPhraseStep1Container: (
    <WalletRecoveryPhraseStep1Dialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      wordCount={number('wordCount', 12)}
    />
  ),
  walletRecoveryPhraseStep2Container: (
    <WalletRecoveryPhraseStep2Dialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      wordCount={number('wordCount', 12)}
    />
  ),
  walletRecoveryPhraseStep3Container: (
    <WalletRecoveryPhraseStep3Dialog onClose={action('onClose')} />
  ),
  walletRecoveryPhraseStep4Container: (
    <WalletRecoveryPhraseStep4Dialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      openExternalLink={action('openExternalLink')}
    />
  ),
  activeField: null,
  isInvalid: boolean('isInvalid', false),
  isSubmitting: boolean('isSubmitting', false),
  isSpendingPasswordSet: boolean('isSpendingPasswordSet', false),
  lastUpdatedField: null,
  nameValidator: () => true,
  onCancelEditing: () => {},
  onFieldValueChange: () => {},
  onStartEditing: () => {},
  onStopEditing: () => {},
  openDialogAction: () => {},
  walletName: text('Wallet Name', 'Wallet Name'),
  spendingPasswordUpdateDate: moment()
    .subtract(1, 'month')
    .toDate(),
  changeSpendingPasswordDialog: (
    <ChangeSpendingPasswordDialog
      walletName={text('Wallet Name', 'Wallet Name')}
      currentPasswordValue="current"
      newPasswordValue="new"
      repeatedPasswordValue="new"
      isSpendingPasswordSet={boolean('isSpendingPasswordSet', false)}
      onSave={action('Change Password - onSave')}
      onCancel={action('Change Password - onCancel')}
      onPasswordSwitchToggle={action(
        'Change Password - onPasswordSwitchToggle'
      )}
      onDataChange={action('Change Password - onDataChange')}
      isSubmitting={boolean('Change Password - isSubmitting', false)}
      error={null}
    />
  ),
  deleteWalletDialogContainer: (
    <DeleteWalletConfirmationDialog
      walletName={text(
        'DeleteWalletConfirmationDialog: Wallet Name',
        'Wallet To Delete'
      )}
      hasWalletFunds={boolean('hasWalletFunds', false)}
      countdownFn={() => number('Delete Wallet Countdown', 9)}
      isBackupNoticeAccepted={boolean('isBackupNoticeAccepted', false)}
      onAcceptBackupNotice={action('Delete Wallet - onAcceptBackupNotice')}
      onContinue={action('Delete Wallet - onContinue')}
      onCancel={action('Delete Wallet - onCancel')}
      confirmationValue={text('Delete Wallet Confirmation Value')}
      onConfirmationValueChange={action(
        'Delete Wallet - onConfirmationValueChange'
      )}
      isSubmitting={boolean('Delete Wallet - isSubmitting', false)}
    />
  ),
};
