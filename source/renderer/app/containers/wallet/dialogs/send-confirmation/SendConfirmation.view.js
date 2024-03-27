'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletSendConfirmationDialogView = void 0;
// @ts-nocheck
const react_1 = __importDefault(require('react'));
const compose_1 = __importDefault(require('lodash/fp/compose'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const DialogContentWithAssets_1 = require('./DialogContentWithAssets');
const DialogContentWithoutAssets_1 = require('./DialogContentWithoutAssets');
const Error_1 = require('./Error');
const PasswordInput_1 = require('./PasswordInput');
const messages_1 = require('./messages');
const Dialog_1 = __importDefault(
  require('../../../../components/widgets/Dialog')
);
const DialogCloseButton_1 = __importDefault(
  require('../../../../components/widgets/DialogCloseButton')
);
const walletUtils_1 = require('../../../../utils/walletUtils');
const assets_1 = require('../../../../utils/assets');
const LoadingSpinner_1 = __importDefault(
  require('../../../../components/widgets/LoadingSpinner')
);
const hooks_1 = require('./hooks');
const helpers_1 = require('./helpers');
const styles_scss_1 = __importDefault(require('./styles.scss'));
function View({
  intl,
  amount,
  areTermsAccepted,
  selectedAssets,
  assetsAmounts,
  receiver,
  totalAmount,
  transactionFee,
  hwDeviceStatus,
  formattedTotalAmount,
  wallet,
  assetTokens,
  error,
  isFlight,
  isTrezor,
  isSubmitting,
  isHardwareWallet,
  onExternalLinkClick,
  onCancel,
  onSubmitCb,
  onTermsCheckboxClick,
  onCopyAssetParam,
}) {
  const {
    passphraseField,
    flightCandidateCheckboxField,
    onSubmit,
    handleSubmitOnEnter,
  } = (0, hooks_1.useForm)({
    intl,
    error,
    amount,
    assetTokens,
    assetsAmounts,
    receiver,
    selectedAssets,
    isHardwareWallet,
    onSubmitCb,
  });
  const isSendingAssets = !!selectedAssets.length;
  const buttonLabel = !isSubmitting
    ? intl.formatMessage(messages_1.messages.sendButtonLabel)
    : react_1.default.createElement(LoadingSpinner_1.default, null);
  const dialogActions = [
    {
      label: intl.formatMessage(messages_1.messages.backButtonLabel),
      onClick: !isSubmitting ? onCancel : () => {},
    },
    {
      label: buttonLabel,
      onClick: onSubmit,
      primary: true,
      className: 'confirmButton',
      disabled:
        (0, helpers_1.isNotEnoughFundsForTokenError)(error?.id) ||
        (0, helpers_1.didHWTxVerificationFailed)({
          isHardwareWallet,
          hwDeviceStatus,
        }) ||
        (0, helpers_1.doTermsNeedAcceptance)({ areTermsAccepted, isFlight }) ||
        !(0, helpers_1.isPasswordValid)({
          isHardwareWallet,
          isValid: passphraseField.isValid,
        }),
    },
  ];
  return react_1.default.createElement(
    Dialog_1.default,
    {
      title: intl.formatMessage(messages_1.messages.dialogTitle),
      subtitle: wallet.name,
      actions: dialogActions,
      closeOnOverlayClick: true,
      primaryButtonAutoFocus: true,
      onClose: !isSubmitting ? onCancel : () => {},
      className: styles_scss_1.default.root,
      closeButton: react_1.default.createElement(
        DialogCloseButton_1.default,
        null
      ),
    },
    (0, walletUtils_1.shouldShowEmptyWalletWarning)(
      totalAmount,
      wallet,
      !!assetTokens?.length &&
        assetTokens.length > 0 &&
        (0, assets_1.hasTokensLeftAfterTransaction)(
          assetTokens,
          selectedAssets,
          assetsAmounts
        )
    ) &&
      react_1.default.createElement(
        'div',
        { className: styles_scss_1.default.warning },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages_1.messages.emptyingWarning,
          tagName: 'p',
        })
      ),
    isSendingAssets
      ? react_1.default.createElement(
          DialogContentWithAssets_1.DialogContentWithAssets,
          {
            amount: amount,
            formattedTotalAmount: formattedTotalAmount,
            receiver: receiver,
            transactionFee: transactionFee,
            selectedAssets: selectedAssets,
            assetsAmounts: assetsAmounts,
            isHardwareWallet: isHardwareWallet,
            onCopyAssetParam: onCopyAssetParam,
          }
        )
      : react_1.default.createElement(
          DialogContentWithoutAssets_1.DialogContentWithoutAssets,
          {
            amount: amount,
            receiver: receiver,
            transactionFee: transactionFee,
            formattedTotalAmount: formattedTotalAmount,
          }
        ),
    isFlight &&
      react_1.default.createElement(
        'div',
        { className: styles_scss_1.default.flightCandidateWarning },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages_1.messages.flightCandidateWarning,
          tagName: 'p',
        }),
        react_1.default.createElement(Checkbox_1.Checkbox, {
          ...flightCandidateCheckboxField.bind(),
          error: flightCandidateCheckboxField.error,
          skin: CheckboxSkin_1.CheckboxSkin,
          disabled: areTermsAccepted,
          onChange: onTermsCheckboxClick,
          checked: areTermsAccepted,
        })
      ),
    react_1.default.createElement(PasswordInput_1.PasswordInput, {
      isFlight: isFlight,
      isTrezor: isTrezor,
      isHardwareWallet: isHardwareWallet,
      areTermsAccepted: areTermsAccepted,
      hwDeviceStatus: hwDeviceStatus,
      handleSubmitOnEnter: handleSubmitOnEnter,
      onExternalLinkClick: onExternalLinkClick,
      walletName: wallet.name,
      passphraseField: passphraseField,
    }),
    react_1.default.createElement(Error_1.ConfirmationError, {
      error: error,
      onExternalLinkClick: onExternalLinkClick,
    })
  );
}
exports.WalletSendConfirmationDialogView = (0, compose_1.default)(
  react_intl_1.injectIntl,
  mobx_react_1.observer
)(View);
//# sourceMappingURL=SendConfirmation.view.js.map
