// @flow
import React from 'react';
import { text, boolean, number, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';
import wordlist from 'bip39/wordlists/english';
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
  RECOVERY_PHRASE_VERIFICATION_WARNING,
  RECOVERY_PHRASE_VERIFICATION_NOTIFICATION,
} from '../../../../source/renderer/app/config/walletsConfig';
import {
  WalletRecoveryPhraseVerificationStatuses,
  WalletRecoveryPhraseVerificationTypes,
} from '../../../../source/renderer/app/stores/WalletsStore';

/* eslint-disable react/display-name  */

const basicSettingsId = 'Basic Settings';
const changePasswordId = 'Change Password';
const deleteWalletId = 'Delete Wallet';
const recoveryPhraseId = 'Recovery Phrase';

const recoveryPhraseVerificationDateOptions = {
  'Never Checked - Ok': {
    type: WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
    status: WalletRecoveryPhraseVerificationStatuses.OK,
  },
  'Never Checked - Warning': {
    type: WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
    status: WalletRecoveryPhraseVerificationStatuses.WARNING,
  },
  'Never Checked - Notification': {
    type: WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
    status: WalletRecoveryPhraseVerificationStatuses.NOTIFICATION,
  },
  'Already Checked - Ok': {
    type: WalletRecoveryPhraseVerificationTypes.ALREADY_CHECKED,
    status: WalletRecoveryPhraseVerificationStatuses.OK,
  },
  'Already Checked - Warning': {
    type: WalletRecoveryPhraseVerificationTypes.ALREADY_CHECKED,
    status: WalletRecoveryPhraseVerificationStatuses.WARNING,
  },
  'Already Checked - Notification': {
    type: WalletRecoveryPhraseVerificationTypes.ALREADY_CHECKED,
    status: WalletRecoveryPhraseVerificationStatuses.NOTIFICATION,
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
      .subtract(RECOVERY_PHRASE_VERIFICATION_WARNING + 10, 'days')
      .toDate();
  else if (status === 'notification')
    date = moment()
      .subtract(RECOVERY_PHRASE_VERIFICATION_NOTIFICATION + 10, 'days')
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
      walletRecoveryPhraseStep1Container={
        <WalletRecoveryPhraseStep1Dialog
          onClose={action('onClose')}
          onContinue={action('onContinue')}
          wordCount={number('wordCount', 12)}
        />
      }
      walletRecoveryPhraseStep2Container={
        <WalletRecoveryPhraseStep2Dialog
          suggestedMnemonics={wordlist}
          mnemonicValidator={() => {}}
          isVerifying={false}
          onClose={action('onClose')}
          onVerify={action('onVerify')}
          wordCount={number('wordCount', 12)}
        />
      }
      walletRecoveryPhraseStep3Container={
        <WalletRecoveryPhraseStep3Dialog onClose={action('onClose')} />
      }
      walletRecoveryPhraseStep4Container={
        <WalletRecoveryPhraseStep4Dialog
          onClose={action('onClose')}
          onVerifyAgain={action('onVerifyAgain')}
          openExternalLink={action('openExternalLink')}
        />
      }
      creationDate={creationDate}
      recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
      recoveryPhraseVerificationStatus={
        status || WalletRecoveryPhraseVerificationStatuses.OK
      }
      recoveryPhraseVerificationStatusType={
        type || WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED
      }
      locale={locale}
      isForcedWalletResyncStarting={false}
      onResyncWallet={action('onResyncWallet')}
      wordCount={number('wordCount', 12)}
    />
  );
};
