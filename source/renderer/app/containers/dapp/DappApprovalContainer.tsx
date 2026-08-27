import React from 'react';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import DappConsentDialog from '../../components/dapp-consent/DappConsentDialog';
import Cip30TransactionApproval from '../../components/dapp/Cip30TransactionApproval';

type Props = {
  request: DappConsentPresentation;
  deciding: boolean;
  onApprove: () => void;
  onReject: () => void;
};

export default function DappApprovalContainer(props: Props) {
  return props.request.kind === 'transaction-sign' ||
    props.request.kind === 'transaction-submit' ? (
    <Cip30TransactionApproval {...props} />
  ) : (
    <DappConsentDialog {...props} />
  );
}
