import BigNumber from 'bignumber.js';
import { Field } from 'mobx-react-form';
import { Intl } from '../../../../types/i18nTypes';
import { StoresMap } from '../../../../stores/index';
import { ActionsMap } from '../../../../actions/index';
import Wallet, { HwDeviceStatus } from '../../../../domains/Wallet';
import { AssetToken } from '../../../../api/assets/types';
import type { WithdrawalDecision } from '../../../../utils/governance/rewardWithdrawal';
import LocalizableError from '../../../../i18n/LocalizableError';

type CommonProps = {
  amount: string;
  isHardwareWallet: boolean;
  isCollateralPreparation?: boolean;
  spendsPreferredCollateral?: boolean;
  selectedAssets: Array<AssetToken>;
  assetsAmounts: Array<string>;
  formattedTotalAmount: string;
  receiver: string;
  hwDeviceStatus: HwDeviceStatus;
  totalAmount: BigNumber;
  transactionFee: string | null | undefined;
  withdrawal: 'self' | null;
  withdrawalDecision?: WithdrawalDecision;
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
} & Pick<
  CommonProps,
  'amount' | 'receiver' | 'isHardwareWallet' | 'withdrawal'
>;

export type ViewProps = Omit<CommonProps, 'withdrawalDecision'> & {
  intl: Intl;
  // The reward balance this payment leaves in place, or null when it does not.
  heldReward: BigNumber | null;
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
  | 'isCollateralPreparation'
  | 'withdrawal'
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
