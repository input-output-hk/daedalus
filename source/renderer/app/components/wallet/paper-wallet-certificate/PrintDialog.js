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
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const PrintDialog_scss_1 = __importDefault(require('./PrintDialog.scss'));
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'paper.wallet.create.certificate.print.dialog.headline',
    defaultMessage: '!!!Verify printed certificate',
    description:
      'Headline for the "Paper wallet create certificate print dialog".',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.print.dialog.subtitle',
    defaultMessage: `!!!Check your paper wallet certificate and make sure everything
      is readable and correctly printed. You can test this by scanning the QR code with
      a QR scanner application on your mobile phone.`,
    description: '"Paper wallet create certificate print dialog" subtitle.',
  },
  info: {
    id: 'paper.wallet.create.certificate.print.dialog.info',
    defaultMessage: `!!!Your certificate is not yet complete and does not contain all
      the data needed to restore your paper wallet. In the next step, you will need to
      write down an additional {paperWalletWrittenWordsCount} words to your paper wallet recovery phrase.`,
    description: '"Paper wallet create certificate print dialog" info.',
  },
  certificatePrintedConfirmationLabel: {
    id:
      'paper.wallet.create.certificate.print.dialog.certificatePrintedConfirmation',
    defaultMessage:
      '!!!Yes, the paper wallet certificate printed successfully.',
    description:
      '"Paper wallet create certificate print dialog" certificate printed confirmation.',
  },
  certificateReadableConfirmationLabel: {
    id:
      'paper.wallet.create.certificate.print.dialog.certificateReadableConfirmation',
    defaultMessage:
      '!!!Yes, first {paperWalletPrintedWordsCount} words of the paper wallet recovery phrase are readable.',
    description:
      '"Paper wallet create certificate print dialog" certificate readable confirmation.',
  },
  qrScannableConfirmationLabel: {
    id: 'paper.wallet.create.certificate.print.dialog.qrScannableConfirmation',
    defaultMessage: '!!!Yes, the QR code is scannable.',
    description:
      '"Paper wallet create certificate print dialog" QR scannable confirmation.',
  },
});
let PrintDialog = class PrintDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isPrintedCorrectly: false,
    isReadable: false,
    isScannable: false,
  };
  onConfirmCorrectPrinting = () => {
    this.setState((prevState) => ({
      isPrintedCorrectly: !prevState.isPrintedCorrectly,
    }));
  };
  onConfirmReadable = () => {
    this.setState((prevState) => ({
      isReadable: !prevState.isReadable,
    }));
  };
  onConfirmScannable = () => {
    this.setState((prevState) => ({
      isScannable: !prevState.isScannable,
    }));
  };
  render() {
    const { intl } = this.context;
    const { onContinue, onClose } = this.props;
    const { isPrintedCorrectly, isReadable, isScannable } = this.state;
    const certificatePrintedCheckboxClasses = (0, classnames_1.default)([
      'printedCheckbox',
      PrintDialog_scss_1.default.checkbox,
    ]);
    const certificateReadableCheckboxClasses = (0, classnames_1.default)([
      'readableCheckbox',
      PrintDialog_scss_1.default.checkbox,
    ]);
    const qrScannableCheckboxClasses = (0, classnames_1.default)([
      'scannableCheckbox',
      PrintDialog_scss_1.default.checkbox,
    ]);
    const canSubmit = isPrintedCorrectly && isReadable && isScannable;
    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(
          global_messages_1.default.dialogButtonContinueLabel
        ),
        primary: true,
        disabled: !canSubmit,
        onClick: onContinue,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: 'printDialog',
        title: intl.formatMessage(messages.headline),
        actions: actions,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        { className: PrintDialog_scss_1.default.printContentWrapper },
        react_1.default.createElement(
          'p',
          { className: PrintDialog_scss_1.default.subtitle },
          intl.formatMessage(messages.subtitle)
        ),
        react_1.default.createElement(
          'p',
          { className: PrintDialog_scss_1.default.info },
          intl.formatMessage(messages.info, {
            paperWalletWrittenWordsCount:
              cryptoConfig_1.PAPER_WALLET_WRITTEN_WORDS_COUNT,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: PrintDialog_scss_1.default.content },
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: certificatePrintedCheckboxClasses,
            label: intl.formatMessage(
              messages.certificatePrintedConfirmationLabel
            ),
            onChange: this.onConfirmCorrectPrinting,
            checked: isPrintedCorrectly,
            skin: CheckboxSkin_1.CheckboxSkin,
          }),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: certificateReadableCheckboxClasses,
            label: intl.formatMessage(
              messages.certificateReadableConfirmationLabel,
              {
                paperWalletPrintedWordsCount:
                  cryptoConfig_1.PAPER_WALLET_PRINTED_WORDS_COUNT,
              }
            ),
            onChange: this.onConfirmReadable,
            checked: isReadable,
            skin: CheckboxSkin_1.CheckboxSkin,
          }),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: qrScannableCheckboxClasses,
            label: intl.formatMessage(messages.qrScannableConfirmationLabel),
            onChange: this.onConfirmScannable,
            checked: isScannable,
            skin: CheckboxSkin_1.CheckboxSkin,
          })
        )
      )
    );
  }
};
PrintDialog = __decorate([mobx_react_1.observer], PrintDialog);
exports.default = PrintDialog;
//# sourceMappingURL=PrintDialog.js.map
