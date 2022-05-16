import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';

export const getMessages = () => {
  const messages: Record<string, ReactIntlMessage> = defineMessages({
    title: {
      id: 'staking.delegationSetup.confirmation.step.dialog.title',
      defaultMessage: '!!!Confirm Delegation',
      description:
        'Title "Confirm Delegation" on the delegation setup "confirmation" step dialog.',
    },
    stepIndicatorLabel: {
      id: 'staking.delegationSetup.confirmation.step.dialog.stepIndicatorLabel',
      defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
      description:
        'Step indicator label on the delegation setup "confirmation" step dialog.',
    },
    description: {
      id: 'staking.delegationSetup.confirmation.step.dialog.description',
      defaultMessage:
        '!!!Confirm your delegation choice to <span>[{selectedPoolTicker}]</span> stake pool for your <span>{selectedWalletName}</span> wallet.',
      description:
        'Description on the delegation setup "confirmation" step dialog.',
    },
    stakePoolIdLabel: {
      id: 'staking.delegationSetup.confirmation.step.dialog.stakePoolIdLabel',
      defaultMessage: '!!!Stake pool ID',
      description:
        'Stake pool ID label on the delegation setup "confirmation" step dialog.',
    },
    feesLabel: {
      id: 'staking.delegationSetup.confirmation.step.dialog.feesLabel',
      defaultMessage: '!!!Fees',
      description:
        'Fees label on the delegation setup "confirmation" step dialog.',
    },
    depositLabel: {
      id: 'staking.delegationSetup.confirmation.step.dialog.depositLabel',
      defaultMessage: '!!!Deposit',
      description:
        'Deposit label on the delegation setup "confirmation" step dialog.',
    },
    spendingPasswordPlaceholder: {
      id:
        'staking.delegationSetup.confirmation.step.dialog.spendingPasswordPlaceholder',
      defaultMessage: '!!!Spending password',
      description: 'Placeholder for "spending password"',
    },
    spendingPasswordLabel: {
      id:
        'staking.delegationSetup.confirmation.step.dialog.spendingPasswordLabel',
      defaultMessage: '!!!Spending password',
      description: 'Label for "spending password"',
    },
    confirmButtonLabel: {
      id: 'staking.delegationSetup.confirmation.step.dialog.confirmButtonLabel',
      defaultMessage: '!!!Confirm',
      description:
        'Label for continue button on the delegation setup "confirmation" step dialog.',
    },
    cancelButtonLabel: {
      id: 'staking.delegationSetup.confirmation.step.dialog.cancelButtonLabel',
      defaultMessage: '!!!Cancel',
      description:
        'Label for "Cancel" button on the delegation setup "confirmation" step dialog.',
    },
    calculatingFees: {
      id: 'staking.delegationSetup.confirmation.step.dialog.calculatingFees',
      defaultMessage: '!!!Calculating fees',
      description: '"Calculating fees" message in the "confirmation" dialog.',
    },
    calculatingDeposit: {
      id: 'staking.delegationSetup.confirmation.step.dialog.calculatingDeposit',
      defaultMessage: '!!!Calculating deposit',
      description:
        '"Calculating deposit" message in the "confirmation" dialog.',
    },
  });
  messages.fieldIsRequired = globalMessages.fieldIsRequired;
  return messages;
};
