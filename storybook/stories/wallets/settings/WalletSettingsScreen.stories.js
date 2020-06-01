// @flow
import React from 'react';
import { text, boolean, number, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';
import { isIncentivizedTestnetTheme } from '../../_support/utils';

// Screens
import WalletSettings from '../../../../source/renderer/app/components/wallet/settings/WalletSettings';
import ChangeSpendingPasswordDialog from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import WalletRecoveryPhraseStep1Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
import {
  RECOVERY_PHRASE_VERIFICATION_TIMES,
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../../../../source/renderer/app/config/walletRecoveryPhraseVerificationConfig';

/* eslint-disable react/display-name  */

const basicSettingsId = 'Basic Settings';
const changePasswordId = 'Change Password';
const deleteWalletId = 'Delete Wallet';
const recoveryPhraseId = 'Recovery Phrase';

const recoveryPhraseVerificationDateOptions = {
  'Never Checked - Ok': {
    type: RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
    status: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
  },
  'Never Checked - Warning': {
    type: RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
    status: RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING,
  },
  'Never Checked - Notification': {
    type: RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
    status: RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION,
  },
  'Already Checked - Ok': {
    type: RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_VERIFIED,
    status: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
  },
  'Already Checked - Warning': {
    type: RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_VERIFIED,
    status: RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING,
  },
  'Already Checked - Notification': {
    type: RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_VERIFIED,
    status: RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION,
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
      .subtract(RECOVERY_PHRASE_VERIFICATION_TIMES.warning + 10, 'days')
      .toDate();
  else if (status === 'notification')
    date = moment()
      .subtract(RECOVERY_PHRASE_VERIFICATION_TIMES.notification + 10, 'days')
      .toDate();

  const recoveryPhraseVerificationDate = date;
  const creationDate = date;

  return {
    recoveryPhraseVerificationDate,
    creationDate,
  };
};

export default (props: { currentTheme: string, locale: string }) => {
  const { currentTheme, locale } = props;

  const { type, status } = select(
    'Wallet Recovery Phrase Verification',
    recoveryPhraseVerificationDateOptions,
    'Already Checked - Ok',
    recoveryPhraseId
  );

  const { recoveryPhraseVerificationDate, creationDate } = getWalletDates(
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
      isIncentivizedTestnet={isIncentivizedTestnetTheme(currentTheme)}
      isLegacy={boolean('isLegacy', false)}
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
      isInvalid={false}
      isSubmitting={false}
      lastUpdatedField={null}
      nameValidator={() => true}
      onCancelEditing={() => {}}
      onFieldValueChange={() => {}}
      onStartEditing={() => {}}
      onStopEditing={() => {}}
      openDialogAction={() => {}}
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
          walletName={text('Wallet Name', 'Wallet Name')}
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
      onVerifyRecoveryPhrase={action('onVerifyRecoveryPhrase')}
      creationDate={creationDate}
      recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
      recoveryPhraseVerificationStatus={
        status || RECOVERY_PHRASE_VERIFICATION_STATUSES.OK
      }
      recoveryPhraseVerificationStatusType={
        type || RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED
      }
      locale={locale}
      wordCount={number('wordCount', 12)}
      shouldDisplayRecoveryPhrase={boolean('shouldDisplayRecoveryPhrase', true)}
    />
  );
};
