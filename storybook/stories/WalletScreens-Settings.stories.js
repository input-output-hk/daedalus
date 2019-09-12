// @flow
import React from 'react';
import { text, boolean, number, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';
import wordlist from 'bip39/wordlists/english';

// Screens
import WalletSettings from '../../source/renderer/app/components/wallet/settings/WalletSettings';
import { WalletAssuranceModeOptions } from '../../source/renderer/app/domains/Wallet';
import ChangeSpendingPasswordDialog from '../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import ExportWalletToFileDialog from '../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';
import WalletRecoveryPhraseStep1Dialog from '../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
import {
  MNEMONICS_CHECKING_WARNING,
  MNEMONICS_CHECKING_NOTIFICATION,
} from '../../source/renderer/app/config/walletsConfig';

/* eslint-disable react/display-name  */

const basicSettingsId = 'Basic Settings';
const changePasswordId = 'Change Password';
const deleteWalletId = 'Delete Wallet';
const exportWalletId = 'Export Wallet';
const recoveryPhraseId = 'Recovery Phrase';

const mnemonicsConfirmationDateOptions = {
  'Never Checked - Ok': {
    type: 'neverChecked',
    status: 'ok',
  },
  'Never Checked - Warning': {
    type: 'neverChecked',
    status: 'warning',
  },
  'Never Checked - Notification': {
    type: 'neverChecked',
    status: 'notification',
  },
  'Already Checked - Ok': {
    type: 'alreadyChecked',
    status: 'ok',
  },
  'Already Checked - Warning': {
    type: 'alreadyChecked',
    status: 'warning',
  },
  'Already Checked - Notification': {
    type: 'alreadyChecked',
    status: 'notification',
  },
};

const recoveryDialogOptions = {
  None: 0,
  'Step 1 - Explanation': 1,
  'Step 2 - Verification': 2,
  'Step 3 - Verification successful': 3,
  'Step 4 - Verification failure': 4,
};

const getWalletDates = (type: string, status: string) => {
  let date = new Date();
  if (status === 'warning')
    date = moment()
      .subtract(MNEMONICS_CHECKING_WARNING + 10, 'days')
      .toDate();
  else if (status === 'notification')
    date = moment()
      .subtract(MNEMONICS_CHECKING_NOTIFICATION + 10, 'days')
      .toDate();

  const mnemonicsConfirmationDate = date;
  const walletCreationDate = date;

  return {
    mnemonicsConfirmationDate,
    walletCreationDate,
  };
};

export default () => {
  const { type, status } = select(
    'Wallet Recovery Phrase Verification',
    mnemonicsConfirmationDateOptions,
    'Already Checked - Ok',
    recoveryPhraseId
  );

  const { mnemonicsConfirmationDate, walletCreationDate } = getWalletDates(
    type,
    status
  );

  const recoveryDialog = select(
    'Active dialog',
    recoveryDialogOptions,
    'None',
    recoveryPhraseId
  );

  return (
    <WalletSettings
      isDialogOpen={dialog => {
        if (dialog === ChangeSpendingPasswordDialog) {
          return boolean(
            'Change Password - Show dialog',
            false,
            changePasswordId
          );
        }
        if (dialog === DeleteWalletConfirmationDialog) {
          return boolean('Delete Wallet - Show dialog', false, deleteWalletId);
        }
        if (dialog === ExportWalletToFileDialog) {
          return boolean('Export Wallet - Show dialog', false, exportWalletId);
        }
        if (dialog === WalletRecoveryPhraseStep1Dialog) {
          return recoveryDialog === 1;
        }
        if (dialog === WalletRecoveryPhraseStep2Dialog) {
          return recoveryDialog === 2;
        }
        if (dialog === WalletRecoveryPhraseStep3Dialog) {
          return recoveryDialog === 3;
        }
        if (dialog === WalletRecoveryPhraseStep4Dialog) {
          return recoveryDialog === 4;
        }
        return false;
      }}
      activeField={null}
      assuranceLevels={[
        {
          value: WalletAssuranceModeOptions.NORMAL,
          label: {
            id: 'global.assuranceLevel.normal',
            defaultMessage: '!!!Normal',
            description: '',
          },
        },
        {
          value: WalletAssuranceModeOptions.STRICT,
          label: {
            id: 'global.assuranceLevel.strict',
            defaultMessage: '!!!Strict',
            description: '',
          },
        },
      ]}
      isInvalid={false}
      isSubmitting={false}
      lastUpdatedField={null}
      nameValidator={() => true}
      onCancelEditing={() => {}}
      onFieldValueChange={() => {}}
      onStartEditing={() => {}}
      onStopEditing={() => {}}
      openDialogAction={() => {}}
      walletAssurance={WalletAssuranceModeOptions.NORMAL}
      walletName={text('Wallet Name', 'Wallet Name', basicSettingsId)}
      spendingPasswordUpdateDate={moment()
        .subtract(1, 'month')
        .toDate()}
      isSpendingPasswordSet={boolean(
        'isSpendingPasswordSet',
        false,
        changePasswordId
      )}
      changeSpendingPasswordDialog={
        <ChangeSpendingPasswordDialog
          currentPasswordValue="current"
          newPasswordValue="new"
          repeatedPasswordValue="new"
          isSpendingPasswordSet={boolean(
            'isSpendingPasswordSet',
            false,
            changePasswordId
          )}
          onSave={action('Change Password - onSave')}
          onCancel={action('Change Password - onCancel')}
          onPasswordSwitchToggle={action(
            'Change Password - onPasswordSwitchToggle'
          )}
          onDataChange={action('Change Password - onDataChange')}
          isSubmitting={boolean(
            'Change Password - isSubmitting',
            false,
            changePasswordId
          )}
          error={null}
        />
      }
      deleteWalletDialogContainer={
        <DeleteWalletConfirmationDialog
          walletName={text(
            'DeleteWalletConfirmationDialog: Wallet Name',
            'Wallet To Delete',
            deleteWalletId
          )}
          hasWalletFunds={boolean('hasWalletFunds', false, basicSettingsId)}
          countdownFn={() =>
            number('Delete Wallet Countdown', 9, deleteWalletId)
          }
          isBackupNoticeAccepted={boolean(
            'isBackupNoticeAccepted',
            false,
            basicSettingsId
          )}
          onAcceptBackupNotice={action('Delete Wallet - onAcceptBackupNotice')}
          onContinue={action('Delete Wallet - onContinue')}
          onCancel={action('Delete Wallet - onCancel')}
          confirmationValue={text(
            'Delete Wallet Confirmation Value',
            'Wallet name',
            deleteWalletId
          )}
          onConfirmationValueChange={action(
            'Delete Wallet - onConfirmationValueChange'
          )}
          isSubmitting={boolean(
            'Delete Wallet - isSubmitting',
            false,
            deleteWalletId
          )}
        />
      }
      exportWalletDialogContainer={
        <ExportWalletToFileDialog
          walletName={text('Wallet Name', 'Wallet Name')}
          hasSpendingPassword={boolean(
            'isSpendingPasswordSet',
            false,
            basicSettingsId
          )}
          isSubmitting={boolean(
            'Export Wallet - isSubmitting',
            false,
            exportWalletId
          )}
          onSubmit={action('Export Wallet - onSubmit')}
          onClose={action('Export Wallet - onClose')}
        />
      }
      walletRecoveryPhraseStep1Container={
        <WalletRecoveryPhraseStep1Dialog
          onClose={action('onClose')}
          onContinue={action('onContinue')}
        />
      }
      walletRecoveryPhraseStep2Container={
        <WalletRecoveryPhraseStep2Dialog
          suggestedMnemonics={wordlist}
          mnemonicValidator={() => {}}
          isVerifying={false}
          onClose={action('onClose')}
          onVerify={action('onVerify')}
        />
      }
      walletRecoveryPhraseStep3Container={
        <WalletRecoveryPhraseStep3Dialog onClose={action('onClose')} />
      }
      walletRecoveryPhraseStep4Container={
        <WalletRecoveryPhraseStep4Dialog
          onClose={action('onClose')}
          onVerifyAgain={action('onVerifyAgain')}
        />
      }
      walletCreationDate={walletCreationDate}
      mnemonicsConfirmationDate={mnemonicsConfirmationDate}
      mnemonicsConfirmationStatus={status}
      mnemonicsConfirmationStatusType={type}
    />
  );
};
