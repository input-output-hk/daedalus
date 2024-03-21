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
const Dialog_1 = __importDefault(require('../../../widgets/Dialog'));
const ConfirmationDialog_scss_1 = __importDefault(
  require('./ConfirmationDialog.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'wallet.restore.dialog.confirmation.headline',
    defaultMessage: '!!!Are you sure?',
    description:
      'Headline for the wallet restoration cancellation confirmation dialog.',
  },
  content: {
    id: 'wallet.restore.dialog.confirmation.content',
    defaultMessage:
      '!!!You haven’t submitted this information yet. If you close the window now, you will lose your progress and have to start again.',
    description:
      'Content for the wallet restoration cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id: 'wallet.restore.dialog.confirmation.button.cancelButtonLabel',
    defaultMessage: '!!!Back to wallet restoration',
    description:
      '"Cancel" button label for the wallet restoration cancellation confirmation dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.restore.dialog.confirmation.button.confirmButtonLabel',
    defaultMessage: '!!!Close window',
    description:
      '"Abort" button label for the wallet restoration cancellation confirmation dialog.',
  },
});
let ConfirmationDialog = class ConfirmationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onConfirm, onCancel } = this.props;
    const dialogClasses = (0, classnames_1.default)([
      ConfirmationDialog_scss_1.default.component,
      'ConfirmDialog',
    ]);
    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: onCancel,
      },
      {
        className: 'confirmButton',
        label: intl.formatMessage(messages.confirmButtonLabel),
        primary: true,
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
        onClose: onCancel,
      },
      react_1.default.createElement(
        'p',
        null,
        intl.formatMessage(messages.content)
      )
    );
  }
};
ConfirmationDialog = __decorate([mobx_react_1.observer], ConfirmationDialog);
exports.default = ConfirmationDialog;
//# sourceMappingURL=ConfirmationDialog.js.map
