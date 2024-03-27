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
const react_intl_1 = require('react-intl');
const qrcode_react_1 = __importDefault(require('qrcode.react'));
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const VotingRegistrationDialog_1 = __importDefault(
  require('./widgets/VotingRegistrationDialog')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationStepsQrCod... Remove this comment to see the full error message
const VotingRegistrationStepsQrCode_scss_1 = __importDefault(
  require('./VotingRegistrationStepsQrCode.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  qrCodeTitle: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeTitle',
    defaultMessage: '!!!Please complete your registration now.',
    description: 'Qr code title on the voting registration "qr code" step.',
  },
  qrCodeDescription1: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeDescription1',
    defaultMessage:
      '!!!Open the Catalyst Voting app on your smartphone, scan the QR code, and enter your PIN to complete the voting registration process.',
    description:
      'Part 1 of Qr code description of use on the voting registration "qr code" step.',
  },
  qrCodeDescription2: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeDescription2',
    defaultMessage:
      '!!!Your registration remains valid across all Catalyst funding rounds. Ensure that you save your QR code and PIN so you can reconnect your wallet to the voting app if you are logged out, or if you want to connect a new device.',
    description:
      'Part 2 of Qr code description of use on the voting registration "qr code" step.',
  },
  qrCodeWarning: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeWarning',
    defaultMessage:
      '!!!<span>Warning:</span> After closing this window the QR code will no longer be available. If you do not keep a PDF copy of the QR code, you might not be able to participate in voting.',
    description: 'Qr code warning on the voting registration "qr code" step.',
  },
  checkbox1Label: {
    id: 'voting.votingRegistration.qrCode.step.checkbox1Label',
    defaultMessage:
      '!!!I understand that I will not be able to retrieve this QR code again after closing this window.',
    description:
      'First checkbox label on the voting registration "qr code" step.',
  },
  checkbox2Label: {
    id: 'voting.votingRegistration.qrCode.step.checkbox2Label',
    defaultMessage:
      '!!!I acknowledge that I must have the downloaded PDF with the QR code to vote.',
    description:
      'Second checkbox label on the voting registration "qr code" step.',
  },
  closeButtonLabel: {
    id: 'voting.votingRegistration.qrCode.step.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      '"Close" button label on the voting registration "qr code" step.',
  },
  saveAsPdfButtonLabel: {
    id: 'voting.votingRegistration.qrCode.step.saveAsPdfButtonLabel',
    defaultMessage: '!!!Save as PDF',
    description:
      '"Save as PDF" button label on the voting registration "qr code" step.',
  },
});
let VotingRegistrationStepsQrCode = class VotingRegistrationStepsQrCode extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isCheckbox1Accepted: false,
    isCheckbox2Accepted: false,
  };
  toggleAcceptance = (param) =>
    this.setState((currentState) =>
      (0, lodash_1.set)({}, param, !currentState[param])
    );
  handleClose = () => {
    const { isCheckbox1Accepted, isCheckbox2Accepted } = this.state;
    if (isCheckbox1Accepted && isCheckbox2Accepted) {
      this.props.onClose();
    }
  };
  render() {
    const { intl } = this.context;
    const { isCheckbox1Accepted, isCheckbox2Accepted } = this.state;
    const { stepsList, activeStep, qrCode, onDownloadPDF } = this.props;
    const qrCodeTitle = intl.formatMessage(messages.qrCodeTitle);
    const qrCodeDescription1 = intl.formatMessage(messages.qrCodeDescription1);
    const qrCodeDescription2 = intl.formatMessage(messages.qrCodeDescription2);
    const qrCodeWarning = react_1.default.createElement(
      react_intl_1.FormattedHTMLMessage,
      { ...messages.qrCodeWarning }
    );
    const checkbox1Label = intl.formatMessage(messages.checkbox1Label);
    const checkbox2Label = intl.formatMessage(messages.checkbox2Label);
    const closeButtonLabel = intl.formatMessage(messages.closeButtonLabel);
    const saveAsPdfButtonLabel = intl.formatMessage(
      messages.saveAsPdfButtonLabel
    );
    const areBothCheckboxesAccepted =
      isCheckbox1Accepted && isCheckbox2Accepted;
    // Get QRCode color value from active theme's CSS variable
    const qrCodeBackgroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-background-color'
        )
      : 'transparent';
    const qrCodeForegroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-foreground-color'
        )
      : '#000';
    const actions = [
      {
        label: closeButtonLabel,
        onClick: this.handleClose,
        disabled: !areBothCheckboxesAccepted,
      },
      {
        label: saveAsPdfButtonLabel,
        onClick: onDownloadPDF,
        primary: true,
      },
    ];
    return react_1.default.createElement(
      VotingRegistrationDialog_1.default,
      {
        onClose: this.handleClose,
        stepsList: stepsList,
        activeStep: activeStep,
        actions: actions,
        containerClassName:
          VotingRegistrationStepsQrCode_scss_1.default.component,
        hideCloseButton: !areBothCheckboxesAccepted,
      },
      react_1.default.createElement(
        'div',
        { className: VotingRegistrationStepsQrCode_scss_1.default.qrCode },
        qrCode &&
          react_1.default.createElement(qrcode_react_1.default, {
            value: qrCode,
            bgColor: qrCodeBackgroundColor,
            fgColor: qrCodeForegroundColor,
            size: 152,
          })
      ),
      react_1.default.createElement(
        'div',
        {
          className:
            VotingRegistrationStepsQrCode_scss_1.default.qrCodeDescription,
        },
        react_1.default.createElement(
          'p',
          { className: VotingRegistrationStepsQrCode_scss_1.default.boldText },
          qrCodeTitle
        ),
        react_1.default.createElement('p', null, qrCodeDescription1),
        react_1.default.createElement('p', null, qrCodeDescription2),
        react_1.default.createElement(
          'p',
          { className: VotingRegistrationStepsQrCode_scss_1.default.warning },
          qrCodeWarning
        )
      ),
      react_1.default.createElement('hr', {
        className: VotingRegistrationStepsQrCode_scss_1.default.separator,
      }),
      react_1.default.createElement(
        'div',
        { className: VotingRegistrationStepsQrCode_scss_1.default.checkboxes },
        react_1.default.createElement(Checkbox_1.Checkbox, {
          label: checkbox1Label,
          onChange: () => this.toggleAcceptance('isCheckbox1Accepted'),
          className: VotingRegistrationStepsQrCode_scss_1.default.checkbox,
          checked: isCheckbox1Accepted,
        }),
        react_1.default.createElement(Checkbox_1.Checkbox, {
          label: checkbox2Label,
          onChange: () => this.toggleAcceptance('isCheckbox2Accepted'),
          className: VotingRegistrationStepsQrCode_scss_1.default.checkbox,
          checked: isCheckbox2Accepted,
        })
      )
    );
  }
};
VotingRegistrationStepsQrCode = __decorate(
  [mobx_react_1.observer],
  VotingRegistrationStepsQrCode
);
exports.default = VotingRegistrationStepsQrCode;
//# sourceMappingURL=VotingRegistrationStepsQrCode.js.map
