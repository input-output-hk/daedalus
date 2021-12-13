import React from 'react';
import { boolean, number, select, text } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { defineMessages } from 'react-intl';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../../_support/utils';
import STAKE_POOLS from '../../../../source/renderer/app/config/stakingStakePools.dummy.json';
import type { Locale } from '../../../../source/common/types/locales.types';
// Screens
import WalletSettings from '../../../../source/renderer/app/components/wallet/settings/WalletSettings';
import ChangeSpendingPasswordDialog from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import PublicKeyQRCodeDialog from '../../../../source/renderer/app/components/wallet/settings/ICOPublicKeyQRCodeDialog';
import WalletPublicKeyDialog from '../../../../source/renderer/app/components/wallet/settings/WalletPublicKeyDialog';
import UndelegateWalletConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/UndelegateWalletConfirmationDialog';
import WalletSettingsRemoveConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';
import WalletRecoveryPhraseStep1Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  RECOVERY_PHRASE_VERIFICATION_TIMES,
  RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../../../../source/renderer/app/config/walletRecoveryPhraseVerificationConfig';
import ICOPublicKeyDialog from '../../../../source/renderer/app/components/wallet/settings/ICOPublicKeyDialog';
import {
  ICO_PUBLIC_KEY_DERIVATION_PATH,
  WALLET_PUBLIC_KEY_DERIVATION_PATH,
} from '../../../../source/renderer/app/config/walletsConfig';
import type { ReactIntlMessage } from '../../../../source/renderer/app/types/i18nTypes';

/* eslint-disable react/display-name  */
const basicSettingsId = 'Basic Settings';
const changePasswordId = 'Change Password';
const undelegateWalletId = 'Undelegate Wallet';
const deleteWalletId = 'Delete Wallet';
const walletPublicKeyId = 'Wallet Public Key';
const icoPublicKeyId = 'ICO Public Key';
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
const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};
const selectedWallet = generateWallet(
  'Wallet 1',
  '1000000000',
  assets,
  0,
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
  STAKE_POOLS[0]
);

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

export default (props: { locale: Locale }) => {
  const { locale } = props;
  // @ts-ignore ts-migrate(2339) FIXME: Property 'type' does not exist on type 'SelectType... Remove this comment to see the full error message
  const { type, status } = select(
    'Wallet Recovery Phrase Verification',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ 'Never Checked - Ok': { type: ... Remove this comment to see the full error message
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
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
    'None',
    recoveryPhraseId
  );
  const delegationStakePoolStatus = select(
    'Delegation status',
    {
      Delegating: 'delegating',
      'Not delegating': 'not_delegating',
    },
    'delegating',
    undelegateWalletId
  );
  const walletMessages: Record<string, ReactIntlMessage> = defineMessages({
    dialogTitle: {
      id: 'wallet.settings.walletPublicKey',
      defaultMessage: '!!!Wallet Public Key',
      description: 'Title for the "Wallet Public Key QR Code" dialog.',
    },
    copyPublicKeyLabel: {
      id: 'wallet.settings.copyPublicKey',
      defaultMessage: '!!!Copy public key',
      description: 'Copy public key label.',
    },
  });
  const icoMessages: Record<string, ReactIntlMessage> = defineMessages({
    dialogTitle: {
      id: 'wallet.settings.icoPublicKey',
      defaultMessage: '!!!ICO Public Key',
      description: 'Title for the "ICO Public Key QR Code" dialog.',
    },
    copyPublicKeyLabel: {
      id: 'wallet.settings.copyPublicKey',
      defaultMessage: '!!!Copy public key',
      description: 'Copy public key label.',
    },
  });
  return (
    <WalletSettings
      isLegacy={boolean('isLegacy', false)}
      isDialogOpen={(dialog) => {
        if (dialog === ChangeSpendingPasswordDialog) {
          return boolean(
            'Change Password - Show dialog',
            false,
            changePasswordId
          );
        }

        if (dialog === WalletSettingsRemoveConfirmationDialog) {
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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeField={null}
      isInvalid={false}
      isSubmitting={false}
      lastUpdatedField={null}
      nameValidator={() => true}
      onCancel={() => {}}
      onFieldValueChange={() => {}}
      onStartEditing={() => {}}
      onStopEditing={() => {}}
      openDialogAction={() => {}}
      walletId="walletid"
      walletName={text('Wallet Name', 'Wallet Name', basicSettingsId)}
      delegationStakePoolStatus={delegationStakePoolStatus}
      lastDelegationStakePoolStatus={delegationStakePoolStatus}
      isRestoring={false}
      isSyncing={false}
      walletPublicKey={walletPublicKeyId}
      icoPublicKey={icoPublicKeyId}
      spendingPasswordUpdateDate={moment().subtract(1, 'month').toDate()}
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
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
          currentLocale={'en-US'}
        />
      }
      walletPublicKeyDialogContainer={
        <WalletPublicKeyDialog
          onRevealPublicKey={action('onRevealPublicKey')}
          onClose={action('onCancel')}
          error={null}
          walletName={'Test Wallet'}
          hasReceivedWalletPublicKey
        />
      }
      icoPublicKeyDialogContainer={
        <ICOPublicKeyDialog
          onRevealPublicKey={action('onRevealICOPublicKey')}
          onClose={action('onCancel')}
          hasReceivedICOPublicKey
          error={null}
          walletName={'ICO Test Wallet'}
        />
      }
      walletPublicKeyQRCodeDialogContainer={
        <PublicKeyQRCodeDialog
          walletName={text(
            'PublicKeyQRCodeDialog: Wallet Name',
            'Wallet',
            walletPublicKeyId
          )}
          walletPublicKey={walletPublicKeyId}
          onCopyWalletPublicKey={action('Wallet Public Key QR Code - copy')}
          onClose={action('Wallet Public Key QR Code - onClose')}
          messages={walletMessages}
          derivationPath={WALLET_PUBLIC_KEY_DERIVATION_PATH}
        />
      }
      icoPublicKeyQRCodeDialogContainer={
        <PublicKeyQRCodeDialog
          walletName={text(
            'PublicKeyQRCodeDialog: Wallet Name',
            'Wallet',
            walletPublicKeyId
          )}
          walletPublicKey={icoPublicKeyId}
          onCopyWalletPublicKey={action('ICO Public Key QR Code - copy')}
          onClose={action('ICO Public Key QR Code - onClose')}
          messages={icoMessages}
          derivationPath={ICO_PUBLIC_KEY_DERIVATION_PATH}
        />
      }
      undelegateWalletDialogContainer={
        <UndelegateWalletConfirmationDialog
          selectedWallet={selectedWallet}
          stakePoolName={text(
            'UndelegateWalletConfirmationDialog: Stake Pool Name',
            'Stake Pool Name'
          )}
          stakePoolTicker={text(
            'UndelegateWalletConfirmationDialog: Stake Pool Ticker',
            'Stake Pool Ticker'
          )}
          onConfirm={action('Undelegate Wallet - onConfirm')}
          onCancel={action('Undelegate Wallet - onCancel')}
          onExternalLinkClick={action(
            'Undelegate Wallet - onExternalLinkClick'
          )}
          isSubmitting={boolean(
            'Undelegate Wallet - submitting',
            false,
            undelegateWalletId
          )}
          error={null}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          fees={new BigNumber(10)}
          hwDeviceStatus="ready"
          isTrezor={boolean('isTrezor', false)}
        />
      }
      deleteWalletDialogContainer={
        <WalletSettingsRemoveConfirmationDialog
          walletName={text(
            'WalletSettingsRemoveConfirmationDialog: Wallet Name',
            'Wallet To Delete',
            deleteWalletId
          )}
          hasWalletFunds={boolean('hasWalletFunds', false, basicSettingsId)}
          countdownFn={() =>
            // @ts-ignore ts-migrate(2559) FIXME: Type '"Delete Wallet"' has no properties in common... Remove this comment to see the full error message
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
      unpairWalletDialogContainer={
        <WalletSettingsRemoveConfirmationDialog
          walletName={text(
            'WalletSettingsRemoveConfirmationDialog: Wallet Name',
            'Wallet To Unpair',
            deleteWalletId
          )}
          hasWalletFunds={boolean('hasWalletFunds', false, basicSettingsId)}
          countdownFn={() =>
            // @ts-ignore ts-migrate(2559) FIXME: Type '"Delete Wallet"' has no properties in common... Remove this comment to see the full error message
            number('Unpair Wallet Countdown', 9, deleteWalletId)
          }
          isBackupNoticeAccepted={boolean(
            'isBackupNoticeAccepted',
            false,
            basicSettingsId
          )}
          onAcceptBackupNotice={action('Unpair Wallet - onAcceptBackupNotice')}
          onContinue={action('Unpair Wallet - onContinue')}
          onCancel={action('Unpair Wallet - onCancel')}
          confirmationValue={text(
            'Unpair Wallet Confirmation Value',
            'Wallet name',
            deleteWalletId
          )}
          onConfirmationValueChange={action(
            'Unpair Wallet - onConfirmationValueChange'
          )}
          isSubmitting={boolean(
            'Unpair Wallet - isSubmitting',
            false,
            deleteWalletId
          )}
        />
      }
      onVerifyRecoveryPhrase={action('onVerifyRecoveryPhrase')}
      onCopyWalletPublicKey={() => null}
      onCopyICOPublicKey={() => null}
      updateDataForActiveDialogAction={() => null}
      onDelegateClick={() => null}
      getWalletPublicKey={() => null}
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
      isHardwareWallet={false}
      isDelegating={false}
    />
  );
};
