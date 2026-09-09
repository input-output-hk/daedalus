import type { Cip103BatchItemReview } from '../../../../common/cip30/cip103Review';
import type { Cip30TransactionReview } from '../../../../common/cip30/review';
import type {
  NativeTransactionAcknowledgement,
  TransactionAuthorization,
  WalletApprovalResult,
} from '../../../../common/ipc/api';
import type { TransactionReviewDisplay } from '../../../../common/transactions/reviewDisplay';
import type { Asset } from '../../api/assets/types';
import type { WalletTransaction } from '../../domains/WalletTransaction';

export type TransactionApprovalItem = Readonly<{
  index: number;
  display: TransactionReviewDisplay;
  evidence:
    | Readonly<{ kind: 'exact-cbor'; review: Cip30TransactionReview }>
    | Readonly<{
        kind: 'native-plan';
        planCbor: string;
        planDigest: string;
        finalBodyPending: boolean;
      }>;
  effects: readonly Readonly<{ index: number; kind: string; value: string }>[];
  dependencies: Cip103BatchItemReview['dependencies'];
  conflicts: Cip103BatchItemReview['conflicts'];
  approvable: boolean;
  refusalReasons: readonly string[];
}>;

export type TransactionApprovalRequest = Readonly<{
  requestId: string;
  requester:
    | Readonly<{ kind: 'dapp'; origin: string }>
    | Readonly<{ kind: 'wallet'; action: string }>;
  walletName: string;
  networkName: string;
  operation: 'sign' | 'submit' | 'sign-and-submit';
  authorization: TransactionAuthorization;
  items: readonly TransactionApprovalItem[];
  collection: 'single' | 'ordered-sign' | 'attempt-all-submit' | 'migration';
  acknowledgements: readonly NativeTransactionAcknowledgement[];
}>;

export type TransactionReceiptDetails = Readonly<
  Pick<WalletTransaction, 'id' | 'state'> &
    Partial<
      Pick<
        WalletTransaction,
        | 'amount'
        | 'fee'
        | 'confirmations'
        | 'transferAmount'
        | 'isSelfTransfer'
        | 'amountIsKnown'
      >
    >
>;

export type TransactionApprovalDialogProps = Readonly<{
  request: TransactionApprovalRequest;
  assetDetails: Readonly<Record<string, Asset>>;
  deciding: boolean;
  phase: 'ready' | 'signing' | 'waiting-for-device' | 'submitting';
  activeItemIndex?: number;
  canCancel: boolean;
  cancelling: boolean;
  errorCode?: string;
  result?: WalletApprovalResult;
  receipts?: readonly TransactionReceiptDetails[];
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
  onCancel: () => void;
  onDismiss?: () => void;
  onViewTransaction?: (transactionId: string) => void;
}>;
