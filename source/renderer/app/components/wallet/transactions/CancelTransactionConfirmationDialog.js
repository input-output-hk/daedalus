'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const CancelTransactionConfirmationDialog_scss_1 = __importDefault(
  require('./CancelTransactionConfirmationDialog.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'cancel.transaction.confirmation.dialog.headline',
    defaultMessage: '!!!Confirm transaction cancellation?',
    description:
      'Headline for the pending transaction cancellation confirmation dialog.',
  },
  content1: {
    id: 'cancel.transaction.confirmation.dialog.content1',
    defaultMessage:
      '!!!This transaction was submitted to the Cardano network some time ago, but has not been finalized yet. You can try to cancel the transaction now to release the pending funds, but there is a chance that the transaction will be finalized regardless. In this case, the transaction will reappear in your wallet as a completed transaction.',
    description:
      'Content for the pending transaction cancellation confirmation dialog.',
  },
  content2: {
    id: 'cancel.transaction.confirmation.dialog.content2',
    defaultMessage:
      '!!!To ensure that this transfer of funds is processed as soon as possible, we recommend that you cancel this transaction and submit a new one to the network.',
    description:
      'Content for the pending transaction cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id: 'cancel.transaction.confirmation.dialog.button.backLabel',
    defaultMessage: '!!!No, keep the transaction pending',
    description:
      '"Cancel" button label for the pending transaction cancellation confirmation dialog.',
  },
  confirmButtonLabel: {
    id: 'cancel.transaction.confirmation.dialog.button.confirmLabel',
    defaultMessage: '!!!Yes, cancel the transaction',
    description:
      '"Confirm" button label for the pending transaction cancellation confirmation dialog.',
  },
});
let CancelTransactionConfirmationDialog = class CancelTransactionConfirmationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { isSubmitting, onConfirm, onCancel } = this.props;
    const dialogClasses = (0, classnames_1.default)([
      CancelTransactionConfirmationDialog_scss_1.default.component,
      'ConfirmDialog',
    ]);
    const confirmButtonClasses = (0, classnames_1.default)([
      'confirmButton',
      'attention',
      isSubmitting
        ? CancelTransactionConfirmationDialog_scss_1.default.isSubmitting
        : null,
    ]);
    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        disabled: isSubmitting,
        onClick: onCancel,
      },
      {
        className: confirmButtonClasses,
        label: intl.formatMessage(messages.confirmButtonLabel),
        primary: true,
        disabled: isSubmitting,
        onClick: onConfirm,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.headline),
        actions: actions,
        closeOnOverlayClick: false,
        onClose: !isSubmitting && onCancel,
      },
      react_1.default.createElement(
        'p',
        null,
        intl.formatMessage(messages.content1)
      ),
      react_1.default.createElement(
        'p',
        null,
        react_1.default.createElement(
          'strong',
          null,
          intl.formatMessage(messages.content2)
        )
      )
    );
  }
};
CancelTransactionConfirmationDialog = __decorate(
  [mobx_react_1.observer],
  CancelTransactionConfirmationDialog
);
exports.default = CancelTransactionConfirmationDialog;
//# sourceMappingURL=CancelTransactionConfirmationDialog.js.map
