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
const mobx_react_1 = require('mobx-react');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/untada.... Remove this comment to see the full error message
const untada_inline_svg_1 = __importDefault(
  require('../../../assets/images/untada.inline.svg')
);
const ProgressBarLarge_1 = __importDefault(
  require('../../widgets/ProgressBarLarge')
);
const votingConfig_1 = require('../../../config/votingConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationStepsConfi... Remove this comment to see the full error message
const VotingRegistrationStepsConfirm_scss_1 = __importDefault(
  require('./VotingRegistrationStepsConfirm.scss')
);
const VotingRegistrationDialog_1 = __importDefault(
  require('./widgets/VotingRegistrationDialog')
);
const messages = (0, react_intl_1.defineMessages)({
  description: {
    id: 'voting.votingRegistration.confirm.step.description',
    defaultMessage:
      '!!!Confirmation of voting registration requires approximately 5 minutes. Please leave Daedalus running.',
    description: 'Description voting registration "confirm" step.',
  },
  descriptionRestart: {
    id: 'voting.votingRegistration.confirm.step.descriptionRestart',
    defaultMessage:
      '!!!Please restart the voting registration process by clicking <span>Restart voting registration</span>.',
    description:
      'Message for restart voting registration on the voting registration "confirm" step.',
  },
  errorMessage: {
    id: 'voting.votingRegistration.confirm.step.errorMessage',
    defaultMessage:
      '!!!The voting registration process was not completed correctly.',
    description: 'Error message on the voting registration "confirm" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting registration "confirm" step.',
  },
  restartButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.restartButtonLabel',
    defaultMessage: '!!!Restart voting registration',
    description:
      'Label for restart button on the voting registration "confirm" step.',
  },
  transactionPendingLabel: {
    id: 'voting.votingRegistration.confirm.step.transactionPendingLabel',
    defaultMessage: '!!!Transaction pending...',
    description:
      'Label for pending transaction state on the voting registration "confirm" step.',
  },
  transactionConfirmedLabel: {
    id: 'voting.votingRegistration.confirm.step.transactionConfirmedLabel',
    defaultMessage: '!!!Transaction confirmed',
    description:
      'Label for confirmed transaction state on the voting registration "confirm" step.',
  },
  waitingForConfirmationsLabel: {
    id: 'voting.votingRegistration.confirm.step.waitingForConfirmationsLabel',
    defaultMessage: '!!!Waiting for confirmation...',
    description:
      'Label for confirming transaction state on the voting registration "confirm" step.',
  },
  confirmationsCountLabel: {
    id: 'voting.votingRegistration.confirm.step.confirmationsCountLabel',
    defaultMessage: '!!!{currentCount} of {expectedCount}',
    description:
      'Label for number of confirmations on the voting registration "confirm" step.',
  },
});
let VotingRegistrationStepsConfirm = class VotingRegistrationStepsConfirm extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      stepsList,
      activeStep,
      onConfirm,
      onRestart,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      transactionError,
      onClose,
    } = this.props;
    const description = intl.formatMessage(messages.description);
    const descriptionRestart = react_1.default.createElement(
      react_intl_1.FormattedHTMLMessage,
      { ...messages.descriptionRestart }
    );
    const errorMessage = intl.formatMessage(messages.errorMessage);
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const restartButtonLabel = intl.formatMessage(messages.restartButtonLabel);
    const actions = [
      transactionError
        ? {
            className: 'attention',
            label: restartButtonLabel,
            onClick: onRestart,
          }
        : {
            className:
              VotingRegistrationStepsConfirm_scss_1.default.buttonConfirmStyles,
            label: buttonLabel,
            onClick: onConfirm,
            disabled: !isTransactionConfirmed,
            primary: true,
          },
    ];
    let progressBarLeftLabelMessage;
    if (isTransactionConfirmed)
      progressBarLeftLabelMessage = messages.transactionConfirmedLabel;
    else if (isTransactionPending)
      progressBarLeftLabelMessage = messages.transactionPendingLabel;
    else progressBarLeftLabelMessage = messages.waitingForConfirmationsLabel;
    const progressBarLeftLabel = intl.formatMessage(
      progressBarLeftLabelMessage
    );
    const progressBarRightLabel = isTransactionPending
      ? ''
      : intl.formatMessage(messages.confirmationsCountLabel, {
          currentCount: Math.min(
            transactionConfirmations,
            votingConfig_1.VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS
          ),
          expectedCount:
            votingConfig_1.VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
        });
    const progress =
      (transactionConfirmations * 100) /
      votingConfig_1.VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS;
    return react_1.default.createElement(
      VotingRegistrationDialog_1.default,
      {
        onClose: () => {
          onClose(!transactionError);
        },
        stepsList: stepsList,
        activeStep: activeStep,
        actions: actions,
        containerClassName:
          VotingRegistrationStepsConfirm_scss_1.default.component,
        hideSteps: !!transactionError,
      },
      transactionError
        ? react_1.default.createElement(
            react_1.Fragment,
            null,
            react_1.default.createElement(
              'div',
              {
                className:
                  VotingRegistrationStepsConfirm_scss_1.default
                    .sadLogoContainer,
              },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: untada_inline_svg_1.default,
                className:
                  VotingRegistrationStepsConfirm_scss_1.default.sadLogoIcon,
              })
            ),
            react_1.default.createElement(
              'div',
              {
                className:
                  VotingRegistrationStepsConfirm_scss_1.default.errorMessage,
              },
              react_1.default.createElement('p', null, errorMessage)
            ),
            react_1.default.createElement(
              'div',
              {
                className:
                  VotingRegistrationStepsConfirm_scss_1.default.description,
              },
              react_1.default.createElement('p', null, descriptionRestart)
            )
          )
        : react_1.default.createElement(
            react_1.Fragment,
            null,
            react_1.default.createElement(ProgressBarLarge_1.default, {
              leftLabel: progressBarLeftLabel,
              rightLabel1: progressBarRightLabel,
              loading: isTransactionPending,
              progress: Math.min(progress, 100),
            }),
            react_1.default.createElement(
              'div',
              {
                className:
                  VotingRegistrationStepsConfirm_scss_1.default.description,
              },
              react_1.default.createElement('p', null, description)
            )
          )
    );
  }
};
VotingRegistrationStepsConfirm = __decorate(
  [mobx_react_1.observer],
  VotingRegistrationStepsConfirm
);
exports.default = VotingRegistrationStepsConfirm;
//# sourceMappingURL=VotingRegistrationStepsConfirm.js.map
