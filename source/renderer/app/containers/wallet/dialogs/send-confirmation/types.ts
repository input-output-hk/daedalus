import BigNumber from 'bignumber.js';
import { Field } from 'mobx-react-form';
import { Intl } from '../../../../types/i18nTypes';
import { StoresMap } from '../../../../stores/index';
import { ActionsMap } from '../../../../actions/index';
import Wallet, { HwDeviceStatus } from '../../../../domains/Wallet';
import { AssetToken } from '../../../../api/assets/types';
import LocalizableError from '../../../../i18n/LocalizableError';

type CommonProps = {
  amount: string;
  isHardwareWallet: boolean;
  selectedAssets: Array<AssetToken>;
  assetsAmounts: Array<string>;
  formattedTotalAmount: string;
  receiver: string;
  hwDeviceStatus: HwDeviceStatus;
  totalAmount: BigNumber;
  transactionFee: string | null | undefined;
  onExternalLinkClick: (...args: Array<any>) => any;
};

export type ContainerProps = CommonProps & {
  stores: StoresMap;
  actions: ActionsMap;
};

export type SubmitPayload = {
  passphrase: string;
  assets?: Array<AssetToken>;
  assetsAmounts?: Array<string>;
  hasAssetsRemainingAfterTransaction: boolean;
} & Pick<CommonProps, 'amount' | 'receiver' | 'isHardwareWallet'>;

export type ViewProps = CommonProps & {
  intl: Intl;
  assetTokens: Array<AssetToken>;
  areTermsAccepted: boolean;
  wallet: Wallet;
  error: LocalizableError | null | undefined;
  isTrezor: boolean;
  isFlight: boolean;
  isSubmitting: boolean;
  onTermsCheckboxClick: (areTermsAccepted: boolean) => void;
  onCancel: () => void;
  onSubmitCb: (values: SubmitPayload) => void;
  onCopyAssetParam: (...args: Array<any>) => any;
};

export type DialogContentWithoutAssets = Pick<
  ViewProps,
  'intl' | 'amount' | 'receiver' | 'transactionFee' | 'formattedTotalAmount'
>;

export type DialogContentWithAssets = Pick<
  ViewProps,
  'isHardwareWallet' | 'selectedAssets' | 'assetsAmounts' | 'onCopyAssetParam'
> &
  DialogContentWithoutAssets;

export type UseForm = Pick<
  ViewProps,
  | 'intl'
  | 'error'
  | 'amount'
  | 'receiver'
  | 'assetTokens'
  | 'selectedAssets'
  | 'assetsAmounts'
  | 'isHardwareWallet'
  | 'onSubmitCb'
>;

export type PasswordInputProps = {
  walletName: string;
  passphraseField: Field;
  handleSubmitOnEnter: (event: KeyboardEvent) => void;
} & Pick<
  ViewProps,
  | 'isHardwareWallet'
  | 'isFlight'
  | 'isTrezor'
  | 'areTermsAccepted'
  | 'hwDeviceStatus'
  | 'onExternalLinkClick'
>;

export interface FormFields {
  flightCandidateCheckbox: string;
  passphrase: string;
}

export type CreateForm = Pick<ViewProps, 'intl' | 'isHardwareWallet'>;

export type HasAssetsAfterTransaction = Pick<
  ViewProps,
  'assetTokens' | 'selectedAssets' | 'assetsAmounts'
>;

export type IsSendingAllFromSelected = Pick<
  ViewProps,
  'selectedAssets' | 'assetsAmounts'
>;
