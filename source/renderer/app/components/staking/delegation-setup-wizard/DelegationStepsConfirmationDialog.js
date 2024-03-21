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
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const Stepper_1 = require('@react-polymorph/components/Stepper');
const StepperSkin_1 = require('@react-polymorph/skins/simple/StepperSkin');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const DelegationSteps_scss_1 = __importDefault(
  require('./DelegationSteps.scss')
);
const DelegationStepsConfirmationDialog_scss_1 = __importDefault(
  require('./DelegationStepsConfirmationDialog.scss')
);
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const DialogBackButton_1 = __importDefault(
  require('../../widgets/DialogBackButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const formatters_1 = require('../../../utils/formatters');
const form_1 = require('../../../utils/form');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const timingConfig_1 = require('../../../config/timingConfig');
const Wallet_1 = require('../../../domains/Wallet');
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const HardwareWalletStatus_1 = __importDefault(
  require('../../hardware-wallet/HardwareWalletStatus')
);
const DelegationStepsConfirmationDialog_messages_1 = require('./DelegationStepsConfirmationDialog.messages');
const OversaturationText_1 = require('./OversaturationText');
const messages = {
  ...(0, DelegationStepsConfirmationDialog_messages_1.getMessages)(),
  fieldIsRequired: global_messages_1.default.fieldIsRequired,
};
let DelegationStepsConfirmationDialog = class DelegationStepsConfirmationDialog extends react_1.Component {
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
              const isHardwareWallet = (0, lodash_1.get)(
                this.props.selectedWallet,
                'isHardwareWallet'
              );
              if (isHardwareWallet) return [true];
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
    this.form.submit({
      onSuccess: (form) => {
        const { selectedWallet } = this.props;
        const isHardwareWallet = (0, lodash_1.get)(
          selectedWallet,
          'isHardwareWallet'
        );
        const { spendingPassword } = form.values();
        this.props.onConfirm(spendingPassword, isHardwareWallet);
      },
      onError: () => {},
    });
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      onBack,
      onClose,
      stepsList,
      transactionFee,
      selectedPool,
      selectedWallet,
      error,
      isSubmitting,
      hwDeviceStatus,
      onExternalLinkClick,
      isTrezor,
      oversaturationPercentage,
    } = this.props;
    const selectedWalletName = (0, lodash_1.get)(selectedWallet, 'name');
    const isHardwareWallet = (0, lodash_1.get)(
      selectedWallet,
      'isHardwareWallet'
    );
    const selectedPoolTicker = (0, lodash_1.get)(selectedPool, 'ticker');
    const selectedPoolId = (0, lodash_1.get)(selectedPool, 'id');
    const spendingPasswordField = form.$('spendingPassword');
    const buttonLabel = !isSubmitting
      ? intl.formatMessage(messages.confirmButtonLabel)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: !isSubmitting ? onClose : () => {},
        disabled: isSubmitting,
      },
      {
        className: 'confirmButton',
        label: buttonLabel,
        onClick: this.submit,
        primary: true,
        disabled:
          (!isHardwareWallet && !spendingPasswordField.isValid) ||
          (isHardwareWallet &&
            hwDeviceStatus !==
              Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED) ||
          isSubmitting ||
          !transactionFee,
      },
    ];
    const dialogClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.delegationSteps,
      DelegationStepsConfirmationDialog_scss_1.default
        .delegationStepsConfirmationDialogWrapper,
    ]);
    const contentClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.content,
      DelegationStepsConfirmationDialog_scss_1.default.content,
    ]);
    const stepsIndicatorLabel = react_1.default.createElement(
      react_intl_1.FormattedMessage,
      {
        ...messages.stepIndicatorLabel,
        values: {
          currentStep: 3,
          totalSteps: stepsList.length,
        },
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        subtitle: stepsIndicatorLabel,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: !isSubmitting ? onClose : () => {},
        className: dialogClassName,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
        backButton: react_1.default.createElement(DialogBackButton_1.default, {
          onBack: !isSubmitting ? onBack : () => {},
        }),
      },
      react_1.default.createElement(
        'div',
        {
          className:
            DelegationSteps_scss_1.default.delegationStepsIndicatorWrapper,
        },
        react_1.default.createElement(Stepper_1.Stepper, {
          steps: stepsList,
          activeStep: 3,
          skin: StepperSkin_1.StepperSkin,
          labelDisabled: true,
        })
      ),
      oversaturationPercentage > 0 &&
        react_1.default.createElement(OversaturationText_1.OversaturationText, {
          oversaturationPercentage: oversaturationPercentage.toFixed(2),
        }),
      react_1.default.createElement(
        'div',
        { className: contentClassName },
        react_1.default.createElement(
          'p',
          {
            className:
              DelegationStepsConfirmationDialog_scss_1.default.description,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description,
            values: {
              selectedWalletName,
              selectedPoolTicker,
            },
          })
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              DelegationStepsConfirmationDialog_scss_1.default
                .stakePoolIdWrapper,
          },
          react_1.default.createElement(
            'p',
            {
              className:
                DelegationStepsConfirmationDialog_scss_1.default
                  .stakePoolIdLabel,
            },
            intl.formatMessage(messages.stakePoolIdLabel)
          ),
          react_1.default.createElement(
            'p',
            {
              className:
                DelegationStepsConfirmationDialog_scss_1.default.stakePoolId,
            },
            selectedPoolId
          )
        ),
        react_1.default.createElement(
          'div',
          {
            className: DelegationStepsConfirmationDialog_scss_1.default.feesRow,
          },
          react_1.default.createElement(
            'div',
            {
              className:
                DelegationStepsConfirmationDialog_scss_1.default.feesWrapper,
            },
            react_1.default.createElement(
              'p',
              {
                className:
                  DelegationStepsConfirmationDialog_scss_1.default.feesLabel,
              },
              intl.formatMessage(messages.feesLabel)
            ),
            react_1.default.createElement(
              'p',
              {
                className:
                  DelegationStepsConfirmationDialog_scss_1.default.feesAmount,
              },
              !transactionFee
                ? react_1.default.createElement(
                    'span',
                    {
                      className:
                        DelegationStepsConfirmationDialog_scss_1.default
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
                      (0, formatters_1.formattedWalletAmount)(
                        transactionFee.fee,
                        false
                      )
                    ),
                    react_1.default.createElement(
                      'span',
                      {
                        className:
                          DelegationStepsConfirmationDialog_scss_1.default
                            .feesAmountLabel,
                      },
                      ` `,
                      intl.formatMessage(global_messages_1.default.adaUnit)
                    )
                  )
            )
          ),
          transactionFee &&
            transactionFee.deposits.isZero &&
            !transactionFee.deposits.isZero() &&
            react_1.default.createElement(
              'div',
              {
                className:
                  DelegationStepsConfirmationDialog_scss_1.default
                    .depositWrapper,
              },
              react_1.default.createElement(
                'p',
                {
                  className:
                    DelegationStepsConfirmationDialog_scss_1.default
                      .depositLabel,
                },
                intl.formatMessage(messages.depositLabel)
              ),
              react_1.default.createElement(
                'p',
                {
                  className:
                    DelegationStepsConfirmationDialog_scss_1.default
                      .depositAmount,
                },
                react_1.default.createElement(
                  'span',
                  null,
                  (0, formatters_1.formattedWalletAmount)(
                    transactionFee.deposits,
                    false
                  )
                ),
                react_1.default.createElement(
                  'span',
                  {
                    className:
                      DelegationStepsConfirmationDialog_scss_1.default
                        .depositAmountLabel,
                  },
                  ` `,
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
                  DelegationStepsConfirmationDialog_scss_1.default
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
              skin: InputSkin_1.InputSkin,
              error: spendingPasswordField.error,
              onKeyPress: this.handleSubmitOnEnter,
            })
      ),
      error
        ? react_1.default.createElement(
            'p',
            {
              className: DelegationStepsConfirmationDialog_scss_1.default.error,
            },
            intl.formatMessage(error)
          )
        : null
    );
  }
};
DelegationStepsConfirmationDialog = __decorate(
  [mobx_react_1.observer],
  DelegationStepsConfirmationDialog
);
exports.default = DelegationStepsConfirmationDialog;
//# sourceMappingURL=DelegationStepsConfirmationDialog.js.map
