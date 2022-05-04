import BigNumber from 'bignumber.js';
import vjf from 'mobx-react-form/lib/validators/VJF';
import globalMessages from '../../../../i18n/global-messages';
import ReactToolboxMobxForm from '../../../../utils/ReactToolboxMobxForm';
import { messages } from './messages';
import { AssetToken } from '../../../../api/assets/types';
import { HwDeviceStatuses } from '../../../../domains/Wallet';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../../config/timingConfig';
import { formattedTokenWalletAmount } from '../../../../utils/formatters';
import { CreateForm, FormFields, HasAssetsAfterTransaction } from './types';

export const doTermsNeedAcceptance = ({ isFlight, areTermsAccepted }) =>
  !areTermsAccepted && isFlight;

export const didHWTxVerificationFailed = ({
  isHardwareWallet,
  hwDeviceStatus,
}) =>
  isHardwareWallet &&
  hwDeviceStatus !== HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;

export const isNotEnoughFundsForTokenError = (errorId) =>
  errorId === 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens';

export const isPasswordValid = ({ isHardwareWallet, isValid }) =>
  isHardwareWallet || isValid;

export const hasAssetsAfterTransaction = ({
  selectedAssets = [],
  assetTokens = [],
}: HasAssetsAfterTransaction) =>
  selectedAssets.length
    ? selectedAssets.length < assetTokens.length
    : assetTokens.length > 0;

export const getFormattedAssetAmount = (
  { metadata, decimals }: AssetToken,
  assetAmount = 0
) => {
  return formattedTokenWalletAmount(
    new BigNumber(assetAmount),
    metadata,
    decimals
  );
};

export const createForm = ({ intl, isHardwareWallet }: CreateForm) => {
  return new ReactToolboxMobxForm<FormFields>(
    {
      fields: {
        passphrase: {
          type: 'password',
          label: intl.formatMessage(messages.passphraseLabel),
          placeholder: intl.formatMessage(messages.passphraseFieldPlaceholder),
          value: '',
          validators: [
            ({ field }) => {
              if (isHardwareWallet) return [true];

              if (field.value === '') {
                return [
                  false,
                  intl.formatMessage(globalMessages.fieldIsRequired),
                ];
              }

              return [true];
            },
          ],
        },
        flightCandidateCheckbox: {
          type: 'checkbox',
          label: intl.formatMessage(messages.flightCandidateCheckboxLabel),
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
};
