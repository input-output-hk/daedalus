'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getMessages = void 0;
const react_intl_1 = require('react-intl');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const getMessages = () => {
  const messages = (0, react_intl_1.defineMessages)({
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
  messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
  return messages;
};
exports.getMessages = getMessages;
//# sourceMappingURL=DelegationStepsConfirmationDialog.messages.js.map
