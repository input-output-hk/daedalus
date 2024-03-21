'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.createForm = exports.getFormattedAssetAmount = exports.hasAssetsAfterTransaction = exports.isSendingAllFromSelected = exports.isPasswordValid = exports.isNotEnoughFundsForTokenError = exports.didHWTxVerificationFailed = exports.doTermsNeedAcceptance = void 0;
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const global_messages_1 = __importDefault(
  require('../../../../i18n/global-messages')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../../utils/ReactToolboxMobxForm')
);
const messages_1 = require('./messages');
const Wallet_1 = require('../../../../domains/Wallet');
const timingConfig_1 = require('../../../../config/timingConfig');
const formatters_1 = require('../../../../utils/formatters');
const doTermsNeedAcceptance = ({ isFlight, areTermsAccepted }) =>
  !areTermsAccepted && isFlight;
exports.doTermsNeedAcceptance = doTermsNeedAcceptance;
const didHWTxVerificationFailed = ({ isHardwareWallet, hwDeviceStatus }) =>
  isHardwareWallet &&
  hwDeviceStatus !== Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
exports.didHWTxVerificationFailed = didHWTxVerificationFailed;
const isNotEnoughFundsForTokenError = (errorId) =>
  errorId === 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens';
exports.isNotEnoughFundsForTokenError = isNotEnoughFundsForTokenError;
const isPasswordValid = ({ isHardwareWallet, isValid }) =>
  isHardwareWallet || isValid;
exports.isPasswordValid = isPasswordValid;
const isSendingAllFromSelected = ({
  selectedAssets = [],
  assetsAmounts = [],
}) =>
  Boolean(selectedAssets.length) &&
  selectedAssets.every(({ quantity }, i) =>
    quantity.isEqualTo(assetsAmounts?.[i])
  );
exports.isSendingAllFromSelected = isSendingAllFromSelected;
const hasAssetsAfterTransaction = ({
  selectedAssets = [],
  assetTokens = [],
  assetsAmounts = [],
}) => {
  const sendingTokens = Boolean(selectedAssets.length);
  const hasTokens = Boolean(assetTokens.length);
  const sendingAllTokenTypes = selectedAssets.length === assetTokens.length;
  const sendingAllFromSelected = (0, exports.isSendingAllFromSelected)({
    assetsAmounts,
    selectedAssets,
  });
  return sendingTokens
    ? !(sendingAllTokenTypes && sendingAllFromSelected)
    : hasTokens;
};
exports.hasAssetsAfterTransaction = hasAssetsAfterTransaction;
const getFormattedAssetAmount = ({ metadata, decimals }, assetAmount = 0) => {
  return (0, formatters_1.formattedTokenWalletAmount)(
    new bignumber_js_1.default(assetAmount),
    metadata,
    decimals
  );
};
exports.getFormattedAssetAmount = getFormattedAssetAmount;
const createForm = ({ intl, isHardwareWallet }) => {
  return new ReactToolboxMobxForm_1.default(
    {
      fields: {
        passphrase: {
          type: 'password',
          label: intl.formatMessage(messages_1.messages.passphraseLabel),
          placeholder: intl.formatMessage(
            messages_1.messages.passphraseFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (isHardwareWallet) return [true];
              if (field.value === '') {
                return [
                  false,
                  intl.formatMessage(global_messages_1.default.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        flightCandidateCheckbox: {
          type: 'checkbox',
          label: intl.formatMessage(
            messages_1.messages.flightCandidateCheckboxLabel
          ),
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
};
exports.createForm = createForm;
//# sourceMappingURL=helpers.js.map
