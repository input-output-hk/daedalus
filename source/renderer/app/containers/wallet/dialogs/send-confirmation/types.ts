import BigNumber from 'bignumber.js';
import { Field } from 'mobx-react-form';
import { Intl } from '../../../../types/i18nTypes';
import { StoresMap } from '../../../../stores/index';
import { ActionsMap } from '../../../../actions/index';
import Wallet, { HwDeviceStatus } from '../../../../domains/Wallet';
import { AssetToken } from '../../../../api/assets/types';
import LocalizableError from '../../../../i18n/LocalizableError';

export interface FormFields {
  flightCandidateCheckbox: string;
  passphrase: string;
}

export type CreateForm = {
  intl: Intl;
  isHardwareWallet: boolean;
};

export type HasAssetsAfterTransaction = {
  selectedAssets: AssetToken[];
  assetTokens: AssetToken[];
};

export type SubmitPayload = {
  receiver: string;
  amount: string;
  passphrase: string;
  isHardwareWallet: boolean;
  assets?: Array<AssetToken>;
  assetsAmounts?: Array<string>;
  hasAssetsRemainingAfterTransaction?: boolean;
};

export type PasswordInputProps = {
  isHardwareWallet: boolean;
  passphraseField: Field;
  isFlight: boolean;
  isTrezor: boolean;
  areTermsAccepted: boolean;
  walletName: string;
  hwDeviceStatus: HwDeviceStatus;
  onExternalLinkClick: (...args: Array<any>) => any;
  handleSubmitOnEnter: (event: KeyboardEvent) => void;
};

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

export type DialogContentWithoutAssets = {
  intl: Intl;
  amount: string;
  receiver: string;
  transactionFee: string | null | undefined;
  formattedTotalAmount: string;
};

export type DialogContentWithAssets = {
  isHardwareWallet: boolean;
  selectedAssets: Array<AssetToken>;
  assetsAmounts: Array<string>;
  onCopyAssetParam: (...args: Array<any>) => any;
} & DialogContentWithoutAssets;
