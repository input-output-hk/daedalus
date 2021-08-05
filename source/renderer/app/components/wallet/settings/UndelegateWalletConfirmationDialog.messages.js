// @flow
import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const getUndelegateWalletConfirmationDialogMessages = (): {
  [string]: ReactIntlMessage,
} =>
  defineMessages({
    title: {
      id: 'wallet.settings.undelegate.dialog.title',
      defaultMessage: '!!!Undelegate',
      description: 'Title for the "Undelegate wallet" dialog.',
    },
    confirmButtonLabel: {
      id: 'wallet.settings.undelegate.dialog.confirmButtonLabel',
      defaultMessage: '!!!Undelegate',
      description:
        'Label for the "Undelegate" button in the undelegate wallet dialog.',
    },
    descriptionWithTicker: {
      id: 'wallet.settings.undelegate.dialog.descriptionWithTicker',
      defaultMessage:
        '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>[{stakePoolTicker}] {stakePoolName}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
      description:
        'Description of current delegation of wallet in the "Undelegate wallet" dialog.',
    },
    descriptionWithUnknownTicker: {
      id: 'wallet.settings.undelegate.dialog.descriptionWithUnknownTicker',
      defaultMessage:
        '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>{stakePoolTicker}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
      description:
        'Description of current delegation of wallet in the "Undelegate wallet" dialog.',
    },
    unknownStakePoolLabel: {
      id: 'wallet.settings.undelegate.dialog.unknownStakePoolLabel',
      defaultMessage: '!!!unknown',
      description:
        'unknown stake pool label in the "Undelegate wallet" dialog.',
    },
    confirmUnsupportNotice: {
      id: 'wallet.settings.undelegate.dialog.confirmUnsupportNotice',
      defaultMessage:
        '!!!I understand that I am not supporting the Cardano network when my stake is undelegated.',
      description:
        'Notice to confirm if the user understands unsupporting Cardano network after undelegation',
    },
    confirmIneligibleNotice: {
      id: 'wallet.settings.undelegate.dialog.confirmIneligibleNotice',
      defaultMessage:
        '!!!I understand that I will not be eligible to earn rewards when my stake is undelegated.',
      description:
        'Notice to confirm if the user understands non-earning rewards after undelegation',
    },
    feesLabel: {
      id: 'wallet.settings.undelegate.dialog.feesLabel',
      defaultMessage: '!!!Fees',
      description: 'Fees label in the "Undelegate wallet" dialog.',
    },
    depositLabel: {
      id: 'wallet.settings.undelegate.dialog.depositLabel',
      defaultMessage: '!!!Deposits reclaimed',
      description:
        'Deposits reclaimed label in the "Undelegate wallet" dialog.',
    },
    spendingPasswordLabel: {
      id: 'wallet.settings.undelegate.dialog.spendingPasswordLabel',
      defaultMessage: '!!!Spending password',
      description: 'Spending password label in the "Undelegate wallet" dialog.',
    },
    spendingPasswordPlaceholder: {
      id: 'wallet.settings.undelegate.dialog.spendingPasswordPlaceholder',
      defaultMessage: '!!!Type your spending password here',
      description:
        'Spending password placeholder in the "Undelegate wallet" dialog.',
    },

    passwordErrorMessage: {
      id: 'wallet.settings.undelegate.dialog.passwordError',
      defaultMessage: '!!!Incorrect spending password.',
      description:
        'Label for password error in the "Undelegate wallet" dialog.',
    },
    calculatingFees: {
      id: 'wallet.settings.undelegate.dialog.calculatingFees',
      defaultMessage: '!!!Calculating fees',
      description:
        '"Calculating fees" message in the "Undelegate wallet" dialog.',
    },
  });

export default getUndelegateWalletConfirmationDialogMessages;
