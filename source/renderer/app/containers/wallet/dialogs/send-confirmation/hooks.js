'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.useForm = void 0;
const react_1 = require('react');
const helpers_1 = require('./helpers');
const formatters_1 = require('../../../../utils/formatters');
const form_1 = require('../../../../utils/form');
const useForm = ({
  intl,
  error,
  amount,
  receiver,
  assetTokens,
  selectedAssets,
  assetsAmounts,
  isHardwareWallet,
  onSubmitCb,
}) => {
  const form = (0, react_1.useMemo)(
    () => (0, helpers_1.createForm)({ intl, isHardwareWallet }),
    [intl, isHardwareWallet]
  );
  const passphraseField = form.$('passphrase');
  const flightCandidateCheckboxField = form.$('flightCandidateCheckbox');
  const onSubmit = (0, react_1.useCallback)(
    () =>
      form.submit({
        onSuccess: (form) => {
          const { passphrase } = form.values();
          const transactionData = {
            receiver,
            amount: (0, formatters_1.formattedAmountToNaturalUnits)(amount),
            passphrase,
            isHardwareWallet,
            hasAssetsRemainingAfterTransaction: (0,
            helpers_1.hasAssetsAfterTransaction)({
              assetTokens,
              selectedAssets,
              assetsAmounts,
            }),
            ...(selectedAssets.length
              ? { assets: selectedAssets, assetsAmounts }
              : {}),
          };
          onSubmitCb(transactionData);
        },
        onError: () => {},
      }),
    [
      form,
      amount,
      receiver,
      selectedAssets,
      assetsAmounts,
      isHardwareWallet,
      onSubmitCb,
    ]
  );
  const handleSubmitOnEnter = (0, react_1.useCallback)(
    (event) => {
      if (
        (0, helpers_1.isPasswordValid)({
          isHardwareWallet,
          isValid: passphraseField.isValid,
        }) &&
        !(0, helpers_1.isNotEnoughFundsForTokenError)(error?.id)
      ) {
        (0, form_1.submitOnEnter)(onSubmit, event);
      }
    },
    [error, passphraseField, isHardwareWallet, onSubmit]
  );
  return {
    passphraseField,
    flightCandidateCheckboxField,
    handleSubmitOnEnter,
    onSubmit,
  };
};
exports.useForm = useForm;
//# sourceMappingURL=hooks.js.map
