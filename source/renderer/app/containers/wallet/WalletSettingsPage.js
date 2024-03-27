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
const WalletSettings_1 = __importDefault(
  require('../../components/wallet/settings/WalletSettings')
);
const validations_1 = require('../../utils/validations');
const strings_1 = require('../../utils/strings');
const ChangeSpendingPasswordDialogContainer_1 = __importDefault(
  require('./dialogs/settings/ChangeSpendingPasswordDialogContainer')
);
const WalletRecoveryPhraseContainer_1 = __importDefault(
  require('./dialogs/settings/WalletRecoveryPhraseContainer')
);
const PublicKeyDialogContainer_1 = __importDefault(
  require('./dialogs/settings/PublicKeyDialogContainer')
);
const PublicKeyQRCodeDialogContainer_1 = __importDefault(
  require('./dialogs/settings/PublicKeyQRCodeDialogContainer')
);
const UndelegateWalletDialogContainer_1 = __importDefault(
  require('./dialogs/settings/UndelegateWalletDialogContainer')
);
const DeleteWalletDialogContainer_1 = __importDefault(
  require('./dialogs/settings/DeleteWalletDialogContainer')
);
const UnpairWalletDialogContainer_1 = __importDefault(
  require('./dialogs/settings/UnpairWalletDialogContainer')
);
const ExportWalletToFileDialogContainer_1 = __importDefault(
  require('./dialogs/settings/ExportWalletToFileDialogContainer')
);
const cryptoConfig_1 = require('../../config/cryptoConfig');
const routes_config_1 = require('../../routes-config');
const walletsConfig_1 = require('../../config/walletsConfig');
const VerticalFlexContainer_1 = __importDefault(
  require('../../components/layout/VerticalFlexContainer')
);
let WalletSettingsPage = class WalletSettingsPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleCopyWalletPublicKey = (walletPublicKey) => {
    const { wallets } = this.props.actions;
    const publicKey = (0, strings_1.ellipsis)(
      walletPublicKey,
      walletsConfig_1.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      walletsConfig_1.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );
    wallets.copyWalletPublicKey.trigger({
      publicKey,
    });
  };
  handleCopyICOPublicKey = (icoPublicKey) => {
    const { wallets } = this.props.actions;
    const publicKey = (0, strings_1.ellipsis)(
      icoPublicKey,
      walletsConfig_1.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      walletsConfig_1.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );
    wallets.copyICOPublicKey.trigger({
      publicKey,
    });
  };
  handleDelegateClick = () => {
    const { goToRoute } = this.props.actions.router;
    goToRoute.trigger({
      route: routes_config_1.ROUTES.STAKING.DELEGATION_CENTER,
    });
  };
  render() {
    const {
      uiDialogs,
      walletSettings,
      app,
      wallets,
      profile,
      hardwareWallets,
    } = this.props.stores;
    const { checkIsTrezorByWalletId } = hardwareWallets;
    const {
      active: activeWallet,
      activePublicKey: activeWalletPublicKey,
      icoPublicKey,
    } = wallets;
    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSettingsPage.');
    const { isLegacy, isHardwareWallet } = activeWallet;
    const isTrezor = checkIsTrezorByWalletId(activeWallet.id);
    const { actions } = this.props;
    const {
      environment: { isProduction },
    } = app;
    const {
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
      getWalletsRecoveryPhraseVerificationData,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
      recoveryPhraseVerificationContinue,
    } = actions.walletSettings;
    const {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = getWalletsRecoveryPhraseVerificationData(activeWallet.id);
    const locale = profile.currentLocale;
    const shouldDisplayRecoveryPhrase = !isHardwareWallet;
    const wordCount = activeWallet.isRandom
      ? cryptoConfig_1.LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT
      : cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT;
    return react_1.default.createElement(
      VerticalFlexContainer_1.default,
      null,
      react_1.default.createElement(WalletSettings_1.default, {
        error: updateWalletRequest.error,
        openDialogAction: actions.dialogs.open.trigger,
        isHardwareWallet: isHardwareWallet,
        isSpendingPasswordSet: activeWallet.hasPassword,
        isDelegating: activeWallet.isDelegating,
        spendingPasswordUpdateDate: activeWallet.passwordUpdateDate,
        recoveryPhraseVerificationDate: recoveryPhraseVerificationDate,
        recoveryPhraseVerificationStatus: recoveryPhraseVerificationStatus,
        recoveryPhraseVerificationStatusType: recoveryPhraseVerificationStatusType,
        isDialogOpen: uiDialogs.isOpen,
        isLegacy: isLegacy,
        walletId: activeWallet.id,
        walletName: activeWallet.name,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        delegationStakePoolStatus: activeWallet.delegationStakePoolStatus,
        lastDelegationStakePoolStatus:
          activeWallet.lastDelegationStakePoolStatus,
        isRestoring: activeWallet.isRestoring,
        isSyncing: activeWallet.isSyncing,
        walletPublicKey: activeWalletPublicKey,
        icoPublicKey: icoPublicKey,
        creationDate: creationDate,
        isSubmitting: updateWalletRequest.isExecuting,
        isInvalid:
          updateWalletRequest.wasExecuted &&
          updateWalletRequest.result === false,
        showExportLink: !isProduction,
        lastUpdatedField: lastUpdatedWalletField,
        onFieldValueChange: (field, value) =>
          updateWalletField.trigger({
            field,
            value,
          }),
        onStartEditing: (field) =>
          startEditingWalletField.trigger({
            field,
          }),
        onStopEditing: stopEditingWalletField.trigger,
        onCancel: cancelEditingWalletField.trigger,
        onVerifyRecoveryPhrase: recoveryPhraseVerificationContinue.trigger,
        onCopyWalletPublicKey: this.handleCopyWalletPublicKey,
        onCopyICOPublicKey: this.handleCopyICOPublicKey,
        updateDataForActiveDialogAction:
          actions.dialogs.updateDataForActiveDialog.trigger,
        onDelegateClick: this.handleDelegateClick,
        activeField: walletFieldBeingEdited,
        nameValidator: (name) => (0, validations_1.isValidWalletName)(name),
        changeSpendingPasswordDialog: react_1.default.createElement(
          ChangeSpendingPasswordDialogContainer_1.default,
          null
        ),
        walletPublicKeyDialogContainer: react_1.default.createElement(
          PublicKeyDialogContainer_1.default,
          null
        ),
        icoPublicKeyDialogContainer: react_1.default.createElement(
          PublicKeyDialogContainer_1.default,
          { isICO: true }
        ),
        walletPublicKeyQRCodeDialogContainer: react_1.default.createElement(
          PublicKeyQRCodeDialogContainer_1.default,
          null
        ),
        icoPublicKeyQRCodeDialogContainer: react_1.default.createElement(
          PublicKeyQRCodeDialogContainer_1.default,
          { isICO: true }
        ),
        undelegateWalletDialogContainer: react_1.default.createElement(
          UndelegateWalletDialogContainer_1.default,
          {
            onExternalLinkClick: app.openExternalLink,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            isTrezor: isHardwareWallet && isTrezor,
          }
        ),
        deleteWalletDialogContainer: react_1.default.createElement(
          DeleteWalletDialogContainer_1.default,
          null
        ),
        unpairWalletDialogContainer: react_1.default.createElement(
          UnpairWalletDialogContainer_1.default,
          null
        ),
        exportWalletDialogContainer: react_1.default.createElement(
          ExportWalletToFileDialogContainer_1.default,
          null
        ),
        locale: locale,
        wordCount: wordCount,
        shouldDisplayRecoveryPhrase: shouldDisplayRecoveryPhrase,
      }),
      shouldDisplayRecoveryPhrase &&
        react_1.default.createElement(
          WalletRecoveryPhraseContainer_1.default,
          null
        )
    );
  }
};
WalletSettingsPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletSettingsPage
);
exports.default = WalletSettingsPage;
//# sourceMappingURL=WalletSettingsPage.js.map
