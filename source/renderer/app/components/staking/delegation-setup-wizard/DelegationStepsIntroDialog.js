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
// import SVGInline from 'react-svg-inline';
const DelegationSteps_scss_1 = __importDefault(
  require('./DelegationSteps.scss')
);
const DelegationStepsIntroDialog_scss_1 = __importDefault(
  require('./DelegationStepsIntroDialog.scss')
);
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.delegationSetup.intro.step.dialog.title',
    defaultMessage: '!!!Delegate wallet',
    description:
      'Title "Delegation Setup" on the delegation setup "intro" dialog.',
  },
  description: {
    id: 'staking.delegationSetup.intro.step.dialog.description',
    defaultMessage:
      '!!!Follow these steps to configure delegation preferences for your wallet. Please be aware that the last step of delegation confirmation will incur transaction fees.',
    description: 'Description on the delegation setup "intro" dialog.',
  },
  learnMoreButtonLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.learnMore.buttonLabel',
    defaultMessage: '!!!Learn more',
    description:
      '"Learn more" button label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel1: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step1',
    defaultMessage: '!!!Wallet selection',
    description:
      'Steps explanation list item 1 label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel2: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step2',
    defaultMessage: '!!!Stake pool selection',
    description:
      'Steps explanation list item 2 label on the delegation setup "intro" dialog.',
  },
  stepsExplanationLabel3: {
    id: 'staking.delegationSetup.intro.step.dialog.stepsExplanation.step3',
    defaultMessage: '!!!Delegation confirmation',
    description:
      'Steps explanation list item 3 label on the delegation setup "intro" dialog.',
  },
  cancelButtonLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.cancelButtonLabel',
    defaultMessage: '!!!Cancel',
    description:
      'Label for close button on the delegation setup "intro" dialog.',
  },
  continueButtonLabel: {
    id: 'staking.delegationSetup.intro.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "intro" dialog.',
  },
});
class DelegationStepsIntroDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      onClose,
      onContinue, // onLearnMoreClick,
    } = this.props;
    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: onClose,
      },
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: onContinue,
        primary: true,
      },
    ];
    const dialogClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.delegationSteps,
      DelegationStepsIntroDialog_scss_1.default
        .delegationStepsIntroDialogWrapper,
    ]);
    const contentClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.content,
      DelegationStepsIntroDialog_scss_1.default.content,
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
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(messages.description)
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              DelegationStepsIntroDialog_scss_1.default.stepsExplanation,
          },
          react_1.default.createElement(
            'ol',
            null,
            react_1.default.createElement(
              'li',
              null,
              react_1.default.createElement('span', null, '1.'),
              ' ',
              intl.formatMessage(messages.stepsExplanationLabel1)
            ),
            react_1.default.createElement(
              'li',
              null,
              react_1.default.createElement('span', null, '2.'),
              ' ',
              intl.formatMessage(messages.stepsExplanationLabel2)
            ),
            react_1.default.createElement(
              'li',
              null,
              react_1.default.createElement('span', null, '3.'),
              ' ',
              intl.formatMessage(messages.stepsExplanationLabel3)
            )
          )
        )
      )
    );
  }
}
exports.default = DelegationStepsIntroDialog;
//# sourceMappingURL=DelegationStepsIntroDialog.js.map
