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
const react_intl_1 = require('react-intl');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const ToggleRTSFlagsDialog_scss_1 = __importDefault(
  require('./ToggleRTSFlagsDialog.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  enableRTSFlagsModeHeadline: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.title',
    defaultMessage: '!!!Enable RTS flags (RAM management system)',
    description: 'Headline for the RTS flags dialog - when enabling',
  },
  enableRTSFlagsModeExplanation: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.explanation',
    defaultMessage:
      '!!!When enabled, the Cardano node will attempt to reduce its RAM usage. You will need to restart Daedalus for this change to take effect.',
    description: 'Main body of the dialog - when enabling',
  },
  enableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.enableRtsFlagsMode.actionButton',
    defaultMessage: '!!!Enable and quit',
    description: 'Enable RTS flags button label',
  },
  disableRTSFlagsModeHeadline: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.title',
    defaultMessage: '!!!Disable RTS flags (RAM management system)',
    description: 'Headline for the RTS flags dialog - when disabling',
  },
  disableRTSFlagsModeExplanation: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.explanation',
    defaultMessage:
      '!!!When disabled, the Cardano node will start in default mode. You will need to restart Daedalus for this change to take effect.',
    description: 'Main body of the dialog - when disabling',
  },
  disableRTSFlagsModeActionButton: {
    id: 'knownIssues.dialog.disableRtsFlagsMode.actionButton',
    defaultMessage: '!!!Disable and quit',
    description: 'Disable RTS flags button label',
  },
  manualRelaunchConfirmationCheckboxLabel: {
    id:
      'knownIssues.dialog.toggleRtsFlagsMode.manualRelaunchConfirmationCheckboxLabel',
    defaultMessage:
      '!!!I understand that I will need to launch Daedalus manually',
    description: 'Manual relaunch confirmation checkbox label',
  },
});
let ToggleRTSFlagsDialog = class ToggleRTSFlagsDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isConfirmationCheckboxChecked: false,
  };
  handleCheckboxToggle = () => {
    this.setState((prevState) => ({
      isConfirmationCheckboxChecked: !prevState.isConfirmationCheckboxChecked,
    }));
  };
  render() {
    const { intl } = this.context;
    const { isRTSFlagsModeEnabled, onClose, onConfirm } = this.props;
    const { isConfirmationCheckboxChecked } = this.state;
    const actions = [
      {
        label: intl.formatMessage(global_messages_1.default.cancel),
        onClick: onClose,
      },
      {
        label: intl.formatMessage(
          isRTSFlagsModeEnabled
            ? messages.disableRTSFlagsModeActionButton
            : messages.enableRTSFlagsModeActionButton
        ),
        primary: true,
        onClick: onConfirm,
        disabled: !isConfirmationCheckboxChecked,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: ToggleRTSFlagsDialog_scss_1.default.dialog,
        title: intl.formatMessage(
          isRTSFlagsModeEnabled
            ? messages.disableRTSFlagsModeHeadline
            : messages.enableRTSFlagsModeHeadline
        ),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
      },
      react_1.default.createElement(
        'p',
        null,
        intl.formatMessage(
          isRTSFlagsModeEnabled
            ? messages.disableRTSFlagsModeExplanation
            : messages.enableRTSFlagsModeExplanation
        )
      ),
      react_1.default.createElement(Checkbox_1.Checkbox, {
        label: intl.formatMessage(
          messages.manualRelaunchConfirmationCheckboxLabel
        ),
        onChange: this.handleCheckboxToggle,
        checked: isConfirmationCheckboxChecked,
      })
    );
  }
};
ToggleRTSFlagsDialog = __decorate(
  [mobx_react_1.observer],
  ToggleRTSFlagsDialog
);
exports.default = ToggleRTSFlagsDialog;
//# sourceMappingURL=ToggleRTSFlagsDialog.js.map
