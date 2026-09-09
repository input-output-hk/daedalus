import React from 'react';
import type {
  WalletApprovalPresentation,
  WalletApprovalResult,
} from '../../../../common/ipc/api';
import { CIP30_REVIEW_EFFECTS } from '../../../../common/cip30/review';
import type { Asset } from '../../api/assets/types';
import DappConsentDialog from '../../components/dapp-consent/DappConsentDialog';
import DappDataSignApproval from '../../components/dapp-consent/DappDataSignApproval';
import TransactionApprovalDialog from '../../components/transactions/TransactionApprovalDialog';
import type {
  TransactionApprovalItem,
  TransactionApprovalRequest,
  TransactionReceiptDetails,
} from '../../components/transactions/TransactionApprovalDialog.types';

const approvable = (
  review: Extract<
    WalletApprovalPresentation,
    { kind: 'transaction-sign' | 'transaction-submit' }
  >['review']
) =>
  review.approvable &&
  review.commitmentsVerified &&
  review.refusalReasons.length === 0 &&
  review.effects.every(({ kind }) =>
    (CIP30_REVIEW_EFFECTS as readonly string[]).includes(kind)
  ) &&
  !review.effects.some(
    ({ kind }) => kind === 'maximum-collateral-loss-unresolved'
  );

const item = (
  index: number,
  review: Extract<
    WalletApprovalPresentation,
    { kind: 'transaction-sign' | 'transaction-submit' }
  >['review'],
  dependencies: TransactionApprovalItem['dependencies'] = [],
  conflicts: TransactionApprovalItem['conflicts'] = []
): TransactionApprovalItem => ({
  index,
  display: review.display,
  evidence: { kind: 'exact-cbor', review },
  effects: review.effects,
  dependencies,
  conflicts,
  approvable: approvable(review),
  refusalReasons: review.refusalReasons,
});

const transactionRequest = (
  request: Extract<
    WalletApprovalPresentation,
    {
      kind:
        | 'transaction-sign'
        | 'transaction-submit'
        | 'batch-sign'
        | 'batch-submit';
    }
  >
): TransactionApprovalRequest => {
  const signing =
    request.kind === 'transaction-sign' || request.kind === 'batch-sign';
  let items: readonly TransactionApprovalItem[];
  let collection: TransactionApprovalRequest['collection'];
  if (request.kind === 'batch-sign' || request.kind === 'batch-submit') {
    items = request.review.items.map((value) =>
      item(value.index, value.transaction, value.dependencies, value.conflicts)
    );
    collection =
      request.kind === 'batch-sign' ? 'ordered-sign' : 'attempt-all-submit';
  } else {
    items = [
      item(
        0,
        (request as Extract<
          WalletApprovalPresentation,
          { kind: 'transaction-sign' | 'transaction-submit' }
        >).review
      ),
    ];
    collection = 'single';
  }
  return {
    requestId: request.requestId,
    requester: { kind: 'dapp', origin: request.origin },
    walletName: request.walletName,
    networkName: request.networkName,
    operation: signing ? 'sign' : 'submit',
    authorization: request.authorization,
    items,
    collection,
    acknowledgements: [],
  };
};
const nativeTransactionRequest = (
  request: Extract<WalletApprovalPresentation, { kind: 'native-transaction' }>
): TransactionApprovalRequest => ({
  requestId: request.requestId,
  requester: { kind: 'wallet', action: request.action },
  walletName: request.walletName,
  networkName: request.networkName,
  operation: 'sign-and-submit',
  authorization: request.authorization,
  items: request.items.map((value, index) =>
    value.kind === 'exact-cbor'
      ? item(index, value.review)
      : {
          index,
          display: value.display,
          evidence: {
            kind: 'native-plan',
            planCbor: value.planCbor,
            planDigest: value.planDigest,
            finalBodyPending: true,
          },
          effects: [],
          dependencies: [],
          conflicts: [],
          approvable: true,
          refusalReasons: [],
        }
  ),
  collection: request.collection,
  acknowledgements: request.acknowledgements,
});

type Props = {
  request: WalletApprovalPresentation;
  assetDetails: Readonly<Record<string, Asset>>;
  deciding: boolean;
  phase: 'ready' | 'signing' | 'waiting-for-device' | 'submitting';
  activeItemIndex?: number;
  submissionAuthorized: boolean;
  cancelling?: boolean;
  result?: WalletApprovalResult;
  receipts?: readonly TransactionReceiptDetails[];
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
  onCancel?: () => void;
  onDismiss?: () => void;
  onViewTransaction?: (transactionId: string) => void;
};

export default function WalletApprovalContainer(props: Props) {
  if (props.request.kind === 'native-transaction')
    return (
      <TransactionApprovalDialog
        request={nativeTransactionRequest(props.request)}
        assetDetails={props.assetDetails}
        deciding={props.deciding}
        phase={props.phase}
        activeItemIndex={props.activeItemIndex}
        canCancel={props.deciding && !props.submissionAuthorized}
        cancelling={props.cancelling === true}
        result={props.result}
        receipts={props.receipts}
        onApprove={props.onApprove}
        onReject={props.onReject}
        onCancel={props.onCancel || props.onReject}
        onDismiss={props.onDismiss}
        onViewTransaction={props.onViewTransaction}
      />
    );
  if (props.request.kind === 'data-sign')
    return <DappDataSignApproval {...props} request={props.request} />;
  if (
    props.request.kind === 'transaction-sign' ||
    props.request.kind === 'transaction-submit' ||
    props.request.kind === 'batch-sign' ||
    props.request.kind === 'batch-submit'
  )
    return (
      <TransactionApprovalDialog
        request={transactionRequest(props.request)}
        assetDetails={props.assetDetails}
        deciding={props.deciding}
        phase={props.phase}
        activeItemIndex={props.activeItemIndex}
        canCancel={false}
        cancelling={false}
        result={props.result}
        receipts={props.receipts}
        onDismiss={props.onDismiss}
        onViewTransaction={props.onViewTransaction}
        onApprove={props.onApprove}
        onReject={props.onReject}
        onCancel={props.onReject}
      />
    );
  return <DappConsentDialog {...props} request={props.request} />;
}
