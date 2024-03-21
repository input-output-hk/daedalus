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
const Stepper_1 = require('@react-polymorph/components/Stepper');
const StepperSkin_1 = require('@react-polymorph/skins/simple/StepperSkin');
const react_intl_1 = require('react-intl');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationDialog.scs... Remove this comment to see the full error message
const VotingRegistrationDialog_scss_1 = __importDefault(
  require('./VotingRegistrationDialog.scss')
);
const Dialog_1 = __importDefault(require('../../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../../widgets/DialogCloseButton')
);
const DialogBackButton_1 = __importDefault(
  require('../../../widgets/DialogBackButton')
);
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'voting.votingRegistration.dialog.dialogTitle',
    defaultMessage: '!!!Register for voting',
    description: 'Tile "Register to vote" for voting registration',
  },
  subtitle: {
    id: 'voting.votingRegistration.dialog.subtitle',
    defaultMessage: '!!!Step {step} of {stepCount}',
    description: 'Sub title for voting registration',
  },
});
let VotingRegistrationDialog = class VotingRegistrationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    children: null,
  };
  render() {
    const { intl } = this.context;
    const {
      children,
      activeStep,
      stepsList,
      actions,
      onClose,
      onBack,
      containerClassName,
      contentClassName,
      hideCloseButton,
      hideSteps,
    } = this.props;
    const containerStyles = (0, classnames_1.default)([
      VotingRegistrationDialog_scss_1.default.container,
      containerClassName,
    ]);
    const contentStyles = (0, classnames_1.default)([
      VotingRegistrationDialog_scss_1.default.content,
      contentClassName,
    ]);
    const stepsIndicatorLabel = react_1.default.createElement(
      react_intl_1.FormattedMessage,
      {
        ...messages.subtitle,
        values: {
          step: activeStep,
          stepCount: stepsList.length,
        },
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: VotingRegistrationDialog_scss_1.default.component,
        title: intl.formatMessage(messages.dialogTitle),
        subtitle: !hideSteps && stepsIndicatorLabel,
        onClose: onClose,
        closeOnOverlayClick: false,
        closeButton: hideCloseButton
          ? null
          : react_1.default.createElement(DialogCloseButton_1.default, null),
        backButton:
          onBack &&
          react_1.default.createElement(DialogBackButton_1.default, {
            onBack: onBack,
          }),
        actions: actions,
      },
      !hideSteps &&
        react_1.default.createElement(
          'div',
          {
            className:
              VotingRegistrationDialog_scss_1.default
                .votingRegistrationStepsIndicatorWrapper,
          },
          react_1.default.createElement(Stepper_1.Stepper, {
            steps: stepsList,
            activeStep: activeStep,
            skin: StepperSkin_1.StepperSkin,
            labelDisabled: true,
          })
        ),
      react_1.default.createElement(
        'div',
        { className: containerStyles },
        react_1.default.createElement(
          'div',
          { className: contentStyles },
          children
        )
      )
    );
  }
};
VotingRegistrationDialog = __decorate(
  [mobx_react_1.observer],
  VotingRegistrationDialog
);
exports.default = VotingRegistrationDialog;
//# sourceMappingURL=VotingRegistrationDialog.js.map
