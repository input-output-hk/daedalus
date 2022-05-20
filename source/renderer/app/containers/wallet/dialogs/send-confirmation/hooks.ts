import { useCallback, useMemo } from 'react';
import {
  createForm,
  hasAssetsAfterTransaction,
  isNotEnoughFundsForTokenError,
  isPasswordValid,
} from './helpers';
import { formattedAmountToNaturalUnits } from '../../../../utils/formatters';
import { UseForm } from './types';
import { submitOnEnter } from '../../../../utils/form';

export const useForm = ({
  intl,
  error,
  amount,
  receiver,
  assetTokens,
  selectedAssets,
  assetsAmounts,
  isHardwareWallet,
  onSubmitCb,
}: UseForm) => {
  const form = useMemo(() => createForm({ intl, isHardwareWallet }), [
    intl,
    isHardwareWallet,
  ]);
  const passphraseField = form.$('passphrase');
  const flightCandidateCheckboxField = form.$('flightCandidateCheckbox');
  const onSubmit = useCallback(
    () =>
      form.submit({
        onSuccess: (form) => {
          const { passphrase } = form.values();
          const transactionData = {
            receiver,
            amount: formattedAmountToNaturalUnits(amount),
            passphrase,
            isHardwareWallet,
            hasAssetsRemainingAfterTransaction: hasAssetsAfterTransaction({
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
  const handleSubmitOnEnter = useCallback(
    (event: KeyboardEvent) => {
      if (
        isPasswordValid({
          isHardwareWallet,
          isValid: passphraseField.isValid,
        }) &&
        !isNotEnoughFundsForTokenError(error?.id)
      ) {
        submitOnEnter(onSubmit, event);
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
