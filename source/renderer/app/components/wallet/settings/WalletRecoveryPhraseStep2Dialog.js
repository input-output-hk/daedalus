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
exports.messages = void 0;
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const Autocomplete_1 = require('@react-polymorph/components/Autocomplete');
const AutocompleteSkin_1 = require('@react-polymorph/skins/simple/AutocompleteSkin');
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const valid_words_en_1 = __importDefault(
  require('../../../../../common/config/crypto/valid-words.en')
);
const decrypt_1 = require('../../../../../common/config/crypto/decrypt');
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const validations_1 = require('../../../utils/validations');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const WalletRecoveryPhraseStepDialogs_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseStepDialogs.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
exports.messages = (0, react_intl_1.defineMessages)({
  recoveryPhraseStep2Title: {
    id: 'wallet.settings.recoveryPhraseStep2Title',
    defaultMessage: '!!!Wallet recovery phrase verification',
    description: 'Label for the recoveryPhraseStep2Title on wallet settings.',
  },
  recoveryPhraseStep2Description: {
    id: 'wallet.settings.recoveryPhraseStep2Description',
    defaultMessage:
      '!!!Please enter your wallet recovery phrase. Make sure you enter the words in the correct order.',
    description:
      'Label for the recoveryPhraseStep2Description on wallet settings.',
  },
  recoveryPhraseStep2Subtitle: {
    id: 'wallet.settings.recoveryPhraseStep2Subtitle',
    defaultMessage: '!!!Recovery phrase',
    description:
      'Label for the recoveryPhraseStep2Subtitle on wallet settings.',
  },
  recoveryPhraseStep2Button: {
    id: 'wallet.settings.recoveryPhraseStep2Button',
    defaultMessage: '!!!Verify',
    description: 'Label for the recoveryPhraseStep2Button on wallet settings.',
  },
  recoveryPhraseInputPlaceholder: {
    id: 'wallet.settings.recoveryPhraseInputPlaceholder',
    defaultMessage: '!!!Enter word #{wordNumber}',
    description:
      'Placeholder "Enter word #{wordNumber}" for the recovery phrase input on the verification dialog.',
  },
  recoveryPhraseNoResults: {
    id: 'wallet.settings.recoveryPhraseInputNoResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  recoveryPhraseStep2InvalidMnemonics: {
    id: 'wallet.settings.recoveryPhraseStep2InvalidMnemonics',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
});
let WalletRecoveryPhraseStep2Dialog = class WalletRecoveryPhraseStep2Dialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isVerifying: false,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validators: ({ field }) =>
            (0, validations_1.validateMnemonics)({
              requiredWords: this.props.expectedWordCount,
              providedWords: field.value,
              validator: (enteredWords) => [
                (0, decrypt_1.isValidMnemonic)(
                  enteredWords.join(' '),
                  enteredWords.length
                ),
                this.context.intl.formatMessage(
                  exports.messages.recoveryPhraseStep2InvalidMnemonics
                ),
              ],
            }),
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        showErrorsOnChange: false,
        validateOnChange: true,
      },
    }
  );
  handleSubmit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { recoveryPhrase } = form.values();
        this.setState({ isVerifying: true });
        this.props.onContinue({ recoveryPhrase });
      },
    });
  };
  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onClose, expectedWordCount, walletName } = this.props;
    const { isVerifying } = this.state;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const { length: enteredWordCount } = recoveryPhraseField.value;
    const canSubmit =
      !recoveryPhraseField.error &&
      !isVerifying &&
      (Array.isArray(expectedWordCount)
        ? expectedWordCount.includes(enteredWordCount)
        : enteredWordCount === expectedWordCount);
    // const { reset, ...mnemonicInputProps } = recoveryPhraseField.bind();
    const actions = [
      {
        className: isVerifying
          ? WalletRecoveryPhraseStepDialogs_scss_1.default.isVerifying
          : null,
        label: intl.formatMessage(exports.messages.recoveryPhraseStep2Button),
        primary: true,
        onClick: this.handleSubmit,
        disabled: !canSubmit,
      },
    ];
    const maxSelections = Array.isArray(expectedWordCount)
      ? Math.max(...expectedWordCount)
      : expectedWordCount;
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: WalletRecoveryPhraseStepDialogs_scss_1.default.dialog,
        title: intl.formatMessage(exports.messages.recoveryPhraseStep2Title),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: false,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        { className: WalletRecoveryPhraseStepDialogs_scss_1.default.subtitle },
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(exports.messages.recoveryPhraseStep2Description)
        )
      ),
      react_1.default.createElement(Autocomplete_1.Autocomplete, {
        ...recoveryPhraseField.bind(),
        label: intl.formatMessage(exports.messages.recoveryPhraseStep2Subtitle),
        placeholder: intl.formatMessage(
          exports.messages.recoveryPhraseInputPlaceholder,
          {
            wordNumber: enteredWordCount + 1,
          }
        ),
        options: valid_words_en_1.default,
        requiredSelections: Array.isArray(expectedWordCount)
          ? expectedWordCount
          : [expectedWordCount],
        requiredSelectionsInfo: (required, actual) =>
          Array.isArray(expectedWordCount)
            ? intl.formatMessage(
                global_messages_1.default.unknownMnemonicWordCount,
                {
                  actual,
                }
              )
            : intl.formatMessage(
                global_messages_1.default.knownMnemonicWordCount,
                {
                  actual,
                  required,
                }
              ),
        maxSelections: maxSelections,
        error:
          recoveryPhraseField.error !==
            validations_1.INCOMPLETE_MNEMONIC_MARKER &&
          recoveryPhraseField.error,
        maxVisibleOptions: 5,
        noResultsMessage: intl.formatMessage(
          exports.messages.recoveryPhraseNoResults
        ),
        skin: AutocompleteSkin_1.AutocompleteSkin,
        optionHeight: 50,
      })
    );
  }
};
WalletRecoveryPhraseStep2Dialog = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseStep2Dialog
);
exports.default = WalletRecoveryPhraseStep2Dialog;
//# sourceMappingURL=WalletRecoveryPhraseStep2Dialog.js.map
