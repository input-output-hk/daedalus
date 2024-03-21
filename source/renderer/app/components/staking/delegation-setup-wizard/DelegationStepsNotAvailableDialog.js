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
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const DelegationSteps_scss_1 = __importDefault(
  require('./DelegationSteps.scss')
);
const DelegationStepsNotAvailableDialog_scss_1 = __importDefault(
  require('./DelegationStepsNotAvailableDialog.scss')
);
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
const attention_dark_inline_svg_1 = __importDefault(
  require('../../../assets/images/attention-dark.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.delegationSetup.notAvailable.dialog.title',
    defaultMessage: '!!!Delegation is currently unavailable',
    description:
      'Title "Delegation Setup" on the delegation setup not available dialog.',
  },
  description: {
    id: 'staking.delegationSetup.notAvailable.dialog.description',
    defaultMessage:
      '!!!None of your Shelley wallets currently hold the <span>minimum amount of {minDelegationFunds} ADA</span> required for delegation.',
    description: 'Description on the delegation setup not available dialog.',
  },
  closeButtonLabel: {
    id: 'staking.delegationSetup.notAvailable.dialog.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      'Label for close button on the delegation setup not available dialog.',
  },
});
class DelegationStepsNotAvailableDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { minDelegationFunds, onClose } = this.props;
    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.closeButtonLabel),
        onClick: onClose,
        primary: true,
      },
    ];
    const dialogClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.delegationSteps,
      DelegationStepsNotAvailableDialog_scss_1.default
        .delegationStepsNotAvailableDialogWrapper,
    ]);
    const contentClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.content,
      DelegationStepsNotAvailableDialog_scss_1.default.content,
    ]);
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        className: dialogClassName,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
      },
      react_1.default.createElement(
        'div',
        { className: contentClassName },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: attention_dark_inline_svg_1.default,
          className: DelegationStepsNotAvailableDialog_scss_1.default.icon,
        }),
        react_1.default.createElement(
          'p',
          {
            className:
              DelegationStepsNotAvailableDialog_scss_1.default.description,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description,
            values: {
              minDelegationFunds,
            },
          })
        )
      )
    );
  }
}
exports.default = DelegationStepsNotAvailableDialog;
//# sourceMappingURL=DelegationStepsNotAvailableDialog.js.map
