import React from 'react';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import DappConsentDialog from '../../components/dapp-consent/DappConsentDialog';
import Cip30TransactionApproval from '../../components/dapp/Cip30TransactionApproval';
import DappBatchReviewDialog from '../../components/dapp/DappBatchReviewDialog';
import DappDataSignApproval from '../../components/dapp-consent/DappDataSignApproval';

type Props = {
  request: DappConsentPresentation;
  deciding: boolean;
  onApprove: (passphrase?: string) => void;
  onReject: () => void;
};

export default function DappApprovalContainer(props: Props) {
  if (props.request.kind === 'data-sign')
    return <DappDataSignApproval {...props} request={props.request} />;
  if (
    props.request.kind === 'batch-sign' ||
    props.request.kind === 'batch-submit'
  )
    return <DappBatchReviewDialog {...props} request={props.request} />;
  return props.request.kind === 'transaction-sign' ||
    props.request.kind === 'transaction-submit' ? (
    <Cip30TransactionApproval {...props} request={props.request} />
  ) : (
    <DappConsentDialog {...props} request={props.request} />
  );
}
