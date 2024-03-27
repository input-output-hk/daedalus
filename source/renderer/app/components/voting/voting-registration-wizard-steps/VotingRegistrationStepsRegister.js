'use strict';
// @ts-nocheck
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
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const mobx_react_1 = require('mobx-react');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const form_1 = require('../../../utils/form');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const formatters_1 = require('../../../utils/formatters');
const timingConfig_1 = require('../../../config/timingConfig');
const VotingRegistrationStepsRegister_scss_1 = __importDefault(
  require('./VotingRegistrationStepsRegister.scss')
);
const VotingRegistrationDialog_1 = __importDefault(
  require('./widgets/VotingRegistrationDialog')
);
const Wallet_1 = require('../../../domains/Wallet');
const HardwareWalletStatus_1 = __importDefault(
  require('../../hardware-wallet/HardwareWalletStatus')
);
const messages = (0, react_intl_1.defineMessages)({
  description: {
    id: 'voting.votingRegistration.register.step.description',
    defaultMessage:
      '!!!Sign the voting registration transaction to link your wallet balance with your voting registration as proof of your voting power. Funds will not leave your wallet, but registration requires transaction fees.',
    description: 'Description on the voting registration "sign" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.register.step.continueButtonLabel',
    defaultMessage: '!!!Submit registration transaction',
    description:
      'Label for continue button on the voting registration "sign" step.',
  },
  feesLabel: {
    id: 'voting.votingRegistration.register.step.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label on the voting registration "sign" step.',
  },
  spendingPasswordPlaceholder: {
    id: 'voting.votingRegistration.register.step.spendingPasswordPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id: 'voting.votingRegistration.register.step.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  calculatingFees: {
    id: 'voting.votingRegistration.register.step.calculatingFees',
    defaultMessage: '!!!Calculating fees',
    description: '"Calculating fees" message in the "sign" step.',
  },
  learnMoreLink: {
    id: 'voting.votingRegistration.register.step.learnMoreLink',
    defaultMessage: '!!!Learn more',
    description: '"Learn more" link on the "sign" step.',
  },
  learntMoreLinkUrl: {
    id: 'voting.votingRegistration.register.step.learntMoreLinkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900006490763',
    description: 'Learn more" link URL on the "sign" step.',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let VotingRegistrationStepsRegister = class VotingRegistrationStepsRegister extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              const password = field.value;
              if (password === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    const { spendingPassword } = this.form.values();
    this.props.onConfirm(spendingPassword);
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      transactionFee,
      transactionFeeError,
      transactionError,
      isSubmitting,
      onExternalLinkClick,
      onClose,
      onBack,
      stepsList,
      activeStep,
      hwDeviceStatus,
      selectedWallet,
      isTrezor,
      isHardwareWallet,
    } = this.props;
    const spendingPasswordField = form.$('spendingPassword');
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const learnMoreLinkUrl = intl.formatMessage(messages.learntMoreLinkUrl);
    const selectedWalletName = (0, lodash_1.get)(selectedWallet, 'name', '');
    const actions = [
      {
        className: isSubmitting
          ? VotingRegistrationStepsRegister_scss_1.default.isSubmitting
          : null,
        label: buttonLabel,
        onClick: this.submit,
        disabled:
          (!isHardwareWallet && !spendingPasswordField.isValid) ||
          (isHardwareWallet &&
            hwDeviceStatus !==
              Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED) ||
          isSubmitting ||
          !transactionFee,
        primary: true,
      },
    ];
    return react_1.default.createElement(
      VotingRegistrationDialog_1.default,
      {
        onClose: !isSubmitting ? onClose : () => {},
        stepsList: stepsList,
        activeStep: activeStep,
        actions: actions,
        onBack: onBack,
        containerClassName:
          VotingRegistrationStepsRegister_scss_1.default.component,
      },
      react_1.default.createElement(
        'p',
        {
          className: VotingRegistrationStepsRegister_scss_1.default.description,
        },
        intl.formatMessage(messages.description)
      ),
      react_1.default.createElement(
        'div',
        {
          className:
            VotingRegistrationStepsRegister_scss_1.default.learnMoreWrapper,
        },
        react_1.default.createElement(Link_1.Link, {
          className:
            VotingRegistrationStepsRegister_scss_1.default.externalLink,
          onClick: (event) => onExternalLinkClick(learnMoreLinkUrl, event),
          label: intl.formatMessage(messages.learnMoreLink),
          skin: LinkSkin_1.LinkSkin,
        })
      ),
      react_1.default.createElement(
        'div',
        {
          className: VotingRegistrationStepsRegister_scss_1.default.feesWrapper,
        },
        react_1.default.createElement(
          'p',
          {
            className: VotingRegistrationStepsRegister_scss_1.default.feesLabel,
          },
          intl.formatMessage(messages.feesLabel)
        ),
        react_1.default.createElement(
          'p',
          {
            className:
              VotingRegistrationStepsRegister_scss_1.default.feesAmount,
          },
          !transactionFee
            ? react_1.default.createElement(
                'span',
                {
                  className:
                    VotingRegistrationStepsRegister_scss_1.default
                      .calculatingFeesLabel,
                },
                intl.formatMessage(messages.calculatingFees)
              )
            : react_1.default.createElement(
                react_1.default.Fragment,
                null,
                react_1.default.createElement(
                  'span',
                  null,
                  (0, formatters_1.formattedWalletAmount)(transactionFee, false)
                ),
                react_1.default.createElement(
                  'span',
                  {
                    className:
                      VotingRegistrationStepsRegister_scss_1.default
                        .feesAmountLabel,
                  },
                  '\u00A0',
                  intl.formatMessage(global_messages_1.default.adaUnit)
                )
              )
        )
      ),
      isHardwareWallet
        ? react_1.default.createElement(
            'div',
            {
              className:
                VotingRegistrationStepsRegister_scss_1.default
                  .hardwareWalletStatusWrapper,
            },
            react_1.default.createElement(HardwareWalletStatus_1.default, {
              hwDeviceStatus: hwDeviceStatus,
              walletName: selectedWalletName,
              isTrezor: isTrezor,
              onExternalLinkClick: onExternalLinkClick,
            })
          )
        : react_1.default.createElement(Input_1.Input, {
            ...spendingPasswordField.bind(),
            autoFocus: true,
            skin: InputSkin_1.InputSkin,
            error: spendingPasswordField.error,
            onKeyPress: this.handleSubmitOnEnter,
          }),
      transactionFeeError
        ? react_1.default.createElement(
            'div',
            {
              className:
                VotingRegistrationStepsRegister_scss_1.default.errorMessage,
            },
            react_1.default.createElement('p', null, transactionFeeError)
          )
        : null,
      transactionError
        ? react_1.default.createElement(
            'div',
            {
              className:
                VotingRegistrationStepsRegister_scss_1.default.errorMessage,
            },
            react_1.default.createElement(
              'p',
              null,
              intl.formatMessage(transactionError)
            )
          )
        : null
    );
  }
};
VotingRegistrationStepsRegister = __decorate(
  [mobx_react_1.observer],
  VotingRegistrationStepsRegister
);
exports.default = VotingRegistrationStepsRegister;
//# sourceMappingURL=VotingRegistrationStepsRegister.js.map
