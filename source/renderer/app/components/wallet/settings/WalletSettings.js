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
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const moment_1 = __importDefault(require('moment'));
const walletsConfig_1 = require('../../../config/walletsConfig');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const InlineEditingInput_1 = __importDefault(
  require('../../widgets/forms/InlineEditingInput')
);
const ReadOnlyInput_1 = __importDefault(
  require('../../widgets/forms/ReadOnlyInput')
);
const UndelegateWalletButton_1 = __importDefault(
  require('./UndelegateWalletButton')
);
const DelegateWalletButton_1 = __importDefault(
  require('./DelegateWalletButton')
);
const UndelegateWalletConfirmationDialog_1 = __importDefault(
  require('./UndelegateWalletConfirmationDialog')
);
const WalletSettingsRemoveConfirmationDialog_1 = __importDefault(
  require('./WalletSettingsRemoveConfirmationDialog')
);
const UnpairWallet_1 = __importDefault(require('./UnpairWallet'));
const DeleteWallet_1 = __importDefault(require('./DeleteWallet'));
const ChangeSpendingPasswordDialog_1 = __importDefault(
  require('./ChangeSpendingPasswordDialog')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletSettings_scss_1 = __importDefault(require('./WalletSettings.scss'));
const WalletRecoveryPhraseVerificationWidget_1 = __importDefault(
  require('./WalletRecoveryPhraseVerificationWidget')
);
const locales_types_1 = require('../../../../../common/types/locales.types');
const ICOPublicKeyBox_1 = __importDefault(require('./ICOPublicKeyBox'));
const WalletPublicKeyBox_1 = __importDefault(require('./WalletPublicKeyBox'));
const ICOPublicKeyDialog_1 = __importDefault(require('./ICOPublicKeyDialog'));
const ICOPublicKeyQRCodeDialog_1 = __importDefault(
  require('./ICOPublicKeyQRCodeDialog')
);
const WalletPublicKeyDialog_1 = __importDefault(
  require('./WalletPublicKeyDialog')
);
const WalletPublicKeyQRCodeDialog_1 = __importDefault(
  require('./WalletPublicKeyQRCodeDialog')
);
exports.messages = (0, react_intl_1.defineMessages)({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description:
      'Label for the "Transaction assurance security level" dropdown.',
  },
  undelegateWalletHeader: {
    id: 'wallet.settings.undelegateWallet.header',
    defaultMessage: '!!!Undelegating your wallet',
    description: 'Undelegate wallet header on the wallet settings page.',
  },
  undelegateWalletWarning: {
    id: 'wallet.settings.undelegateWallet.warning',
    defaultMessage:
      '!!!If you are planning to stop using this wallet and remove all funds, you should first undelegate it to recover your 2 ada deposit. You will continue getting delegation rewards during the three Cardano epochs after undelegating your wallet.',
    description: 'Undelegate wallet warning explaining the consequences.',
  },
  undelegateWalletDisabledWarning: {
    id: 'wallet.settings.undelegateWallet.disabledWarning',
    defaultMessage:
      "!!!This wallet is synchronizing with the blockchain, so this wallet's delegation status is currently unknown, and undelegation is not possible.",
    description:
      'Undelegate wallet disabled warning explaining why it is disabled.',
  },
  delegateWalletHeader: {
    id: 'wallet.settings.delegateWallet.header',
    defaultMessage: '!!!Delegate your wallet',
    description: 'Delegate wallet header on the wallet settings page.',
  },
  delegateWalletWarning: {
    id: 'wallet.settings.delegateWallet.warning',
    defaultMessage:
      "!!!This wallet is not delegated. Please, delegate the stake from this wallet to earn rewards and support the Cardano network's security.",
    description: 'Delegate wallet warning.',
  },
  delegateWalletDisabledWarning: {
    id: 'wallet.settings.delegateWallet.disabledWarning',
    defaultMessage:
      "!!!This wallet is synchronizing with the blockchain, so this wallet's delegation status is currently unknown, and delegation is not possible.",
    description:
      'Delegate wallet disabled warning explaining why it is disabled.',
  },
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
  },
  passwordLabel: {
    id: 'wallet.settings.password',
    defaultMessage: '!!!Password',
    description: 'Label for the "Password" field.',
  },
  passwordLastUpdated: {
    id: 'wallet.settings.passwordLastUpdated',
    defaultMessage: '!!!Last updated',
    description: 'Last updated X time ago message.',
  },
  passwordNotSet: {
    id: 'wallet.settings.passwordNotSet',
    defaultMessage: "!!!You still don't have password",
    description: "You still don't have password set message.",
  },
});
let WalletSettings = class WalletSettings extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isFormBlocked: false,
  };
  componentDidUpdate() {
    const { isDialogOpen } = this.props;
    const { isFormBlocked } = this.state;
    // Set "name" input to active and "unblock form" on Dialog close
    if (
      !isDialogOpen(WalletSettingsRemoveConfirmationDialog_1.default) &&
      !isDialogOpen(ChangeSpendingPasswordDialog_1.default) &&
      isFormBlocked
    ) {
      this.unblockForm();
    }
  }
  componentWillUnmount() {
    // This call is used to prevent display of old successfully-updated messages
    this.props.onCancel();
  }
  onBlockForm = () => {
    this.setState({
      isFormBlocked: true,
    });
  };
  unblockForm = () => {
    this.setState({
      isFormBlocked: false,
    });
  };
  onUndelegateWalletClick = async () => {
    const {
      walletId,
      openDialogAction,
      updateDataForActiveDialogAction,
    } = this.props;
    this.onBlockForm();
    openDialogAction({
      dialog: UndelegateWalletConfirmationDialog_1.default,
    });
    updateDataForActiveDialogAction({
      data: {
        walletId,
      },
    });
  };
  renderUndelegateWalletBox = () => {
    const { intl } = this.context;
    const {
      isDelegating,
      isRestoring,
      isSyncing,
      isLegacy,
      isDialogOpen,
      onDelegateClick,
      undelegateWalletDialogContainer,
    } = this.props;
    /// @TODO: Once undelegation for rewarded wallet works fine with api, remove reward checking and config
    if (!walletsConfig_1.IS_WALLET_UNDELEGATION_ENABLED || isLegacy) {
      return null;
    }
    let headerMessage;
    let warningMessage;
    if (isDelegating) {
      headerMessage = intl.formatMessage(
        exports.messages.undelegateWalletHeader
      );
      warningMessage =
        isRestoring || isSyncing
          ? intl.formatMessage(exports.messages.undelegateWalletDisabledWarning)
          : intl.formatMessage(exports.messages.undelegateWalletWarning);
    } else {
      headerMessage = intl.formatMessage(exports.messages.delegateWalletHeader);
      warningMessage =
        isRestoring || isSyncing
          ? intl.formatMessage(exports.messages.delegateWalletDisabledWarning)
          : intl.formatMessage(exports.messages.delegateWalletWarning);
    }
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        BorderedBox_1.default,
        { className: WalletSettings_scss_1.default.undelegateWalletBox },
        react_1.default.createElement('span', null, headerMessage),
        react_1.default.createElement(
          'div',
          { className: WalletSettings_scss_1.default.contentBox },
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement('p', null, warningMessage)
          ),
          isDelegating
            ? react_1.default.createElement(UndelegateWalletButton_1.default, {
                disabled: isRestoring || isSyncing,
                onUndelegate: this.onUndelegateWalletClick,
              })
            : react_1.default.createElement(DelegateWalletButton_1.default, {
                disabled: isRestoring || isSyncing,
                onDelegate: onDelegateClick,
              })
        )
      ),
      isDialogOpen(UndelegateWalletConfirmationDialog_1.default)
        ? undelegateWalletDialogContainer
        : false
    );
  };
  render() {
    const { intl } = this.context;
    const {
      walletName,
      creationDate,
      spendingPasswordUpdateDate,
      error,
      isDialogOpen,
      openDialogAction,
      onFieldValueChange,
      onStartEditing,
      onStopEditing,
      onCancel,
      onVerifyRecoveryPhrase,
      nameValidator,
      isLegacy,
      changeSpendingPasswordDialog,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
      locale,
      isSpendingPasswordSet,
      isHardwareWallet,
      shouldDisplayRecoveryPhrase,
      wordCount,
      walletPublicKeyDialogContainer,
      walletPublicKeyQRCodeDialogContainer,
      icoPublicKeyDialogContainer,
      icoPublicKeyQRCodeDialogContainer,
      deleteWalletDialogContainer,
      unpairWalletDialogContainer,
    } = this.props;
    const { isFormBlocked } = this.state;
    // Set Japanese locale to moment. Default is en-US
    moment_1.default.locale(locales_types_1.momentLocales[locale]);
    const passwordMessage = isSpendingPasswordSet
      ? intl.formatMessage(exports.messages.passwordLastUpdated, {
          lastUpdated: (0, moment_1.default)(spendingPasswordUpdateDate)
            .locale(this.context.intl.locale)
            .fromNow(),
        })
      : intl.formatMessage(exports.messages.passwordNotSet);
    return react_1.default.createElement(
      'div',
      { className: WalletSettings_scss_1.default.root },
      react_1.default.createElement(
        BorderedBox_1.default,
        null,
        react_1.default.createElement(InlineEditingInput_1.default, {
          className: 'walletName',
          label: intl.formatMessage(exports.messages.name),
          value: walletName,
          maxLength: 40,
          onFocus: () => onStartEditing('name'),
          onBlur: onStopEditing,
          onCancel: onCancel,
          onSubmit: (value) => onFieldValueChange('name', value),
          isValid: nameValidator,
          valueErrorMessage: intl.formatMessage(
            global_messages_1.default.invalidWalletName
          ),
          readOnly: isFormBlocked,
        }),
        !isHardwareWallet &&
          react_1.default.createElement(ReadOnlyInput_1.default, {
            label: intl.formatMessage(exports.messages.passwordLabel),
            value: passwordMessage,
            isSet: isSpendingPasswordSet,
            withButton: true,
            onClick: () => {
              this.onBlockForm();
              openDialogAction({
                dialog: ChangeSpendingPasswordDialog_1.default,
              });
            },
          }),
        shouldDisplayRecoveryPhrase &&
          react_1.default.createElement(
            WalletRecoveryPhraseVerificationWidget_1.default,
            {
              onVerify: onVerifyRecoveryPhrase,
              recoveryPhraseVerificationDate: recoveryPhraseVerificationDate,
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              recoveryPhraseVerificationStatus: recoveryPhraseVerificationStatus,
              recoveryPhraseVerificationStatusType: recoveryPhraseVerificationStatusType,
              creationDate: creationDate,
              locale: locale,
              wordCount: wordCount,
              isLegacy: isLegacy,
            }
          ),
        error &&
          react_1.default.createElement(
            'p',
            { className: WalletSettings_scss_1.default.error },
            intl.formatMessage(error)
          )
      ),
      isDialogOpen(ChangeSpendingPasswordDialog_1.default)
        ? changeSpendingPasswordDialog
        : false,
      walletsConfig_1.IS_WALLET_PUBLIC_KEY_SHARING_ENABLED &&
        !isLegacy &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(WalletPublicKeyBox_1.default, {
            publicKey: this.props.walletPublicKey,
            locale: this.props.locale,
            onCopyWalletPublicKey: this.props.onCopyWalletPublicKey,
            openDialogAction: this.props.openDialogAction,
          }),
          isDialogOpen(WalletPublicKeyDialog_1.default) &&
            walletPublicKeyDialogContainer,
          isDialogOpen(WalletPublicKeyQRCodeDialog_1.default) &&
            walletPublicKeyQRCodeDialogContainer
        ),
      walletsConfig_1.IS_ICO_PUBLIC_KEY_SHARING_ENABLED &&
        !isLegacy &&
        !isHardwareWallet &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(ICOPublicKeyBox_1.default, {
            publicKey: this.props.icoPublicKey,
            locale: this.props.locale,
            onCopyICOPublicKey: this.props.onCopyICOPublicKey,
            openDialogAction: this.props.openDialogAction,
          }),
          isDialogOpen(ICOPublicKeyDialog_1.default) &&
            icoPublicKeyDialogContainer,
          isDialogOpen(ICOPublicKeyQRCodeDialog_1.default) &&
            icoPublicKeyQRCodeDialogContainer
        ),
      this.renderUndelegateWalletBox(),
      isHardwareWallet
        ? react_1.default.createElement(UnpairWallet_1.default, {
            openDialogAction: openDialogAction,
            isDialogOpen: isDialogOpen,
            unpairWalletDialogContainer: unpairWalletDialogContainer,
            onBlockForm: this.onBlockForm,
          })
        : react_1.default.createElement(DeleteWallet_1.default, {
            openDialogAction: openDialogAction,
            isDialogOpen: isDialogOpen,
            deleteWalletDialogContainer: deleteWalletDialogContainer,
            onBlockForm: this.onBlockForm,
          })
    );
  }
};
WalletSettings = __decorate([mobx_react_1.observer], WalletSettings);
exports.default = WalletSettings;
//# sourceMappingURL=WalletSettings.js.map
