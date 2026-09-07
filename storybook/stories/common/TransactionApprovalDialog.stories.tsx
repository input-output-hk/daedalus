import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import TransactionApprovalDialog from '../../../source/renderer/app/components/transactions/TransactionApprovalDialog';

const display = {
  entries: [
    {
      effectIndex: 0,
      role: 'input',
      position: 0,
      outpoint: { transactionId: '11'.repeat(32), index: '0' },
      address: `addr_test1${'q'.repeat(40)}`,
      control: 'key',
      ownership: 'wallet',
      value: { coin: '110000000', assets: [] },
      hasDatum: false,
      hasReferenceScript: false,
    },
    {
      effectIndex: 1,
      role: 'output',
      position: 0,
      outpoint: null,
      address: `addr_test1${'p'.repeat(40)}`,
      control: 'key',
      ownership: 'other',
      value: { coin: '100000000', assets: [] },
      hasDatum: false,
      hasReferenceScript: false,
    },
    {
      effectIndex: 2,
      role: 'output',
      position: 1,
      outpoint: null,
      address: `addr_test1${'r'.repeat(40)}`,
      control: 'key',
      ownership: 'wallet',
      value: { coin: '9700000', assets: [] },
      hasDatum: false,
      hasReferenceScript: false,
    },
  ],
  walletInputs: { coin: '110000000', assets: [] },
  walletOutputs: { coin: '9700000', assets: [] },
  walletChange: { coin: '-100300000', assets: [] },
  fee: '300000',
  deposits: null,
  refunds: null,
  maximumCollateralLoss: null,
  mint: [],
  withdrawals: [],
  certificates: [],
  votes: [],
  proposalCount: 0,
  donation: null,
};

const request = {
  requestId: 'storybook-payment',
  requester: { kind: 'wallet', action: 'payment' },
  walletName: 'Savings',
  networkName: 'Preview',
  operation: 'sign-and-submit',
  authorization: { kind: 'software' },
  collection: 'single',
  acknowledgements: [],
  items: [
    {
      index: 0,
      display,
      evidence: {
        kind: 'native-plan',
        planCbor: '8a7664616564616c75732d6e61746976652d72657669657701',
        planDigest: '22'.repeat(32),
        finalBodyPending: true,
      },
      effects: [
        { index: 0, kind: 'input', value: '110.000000 ADA from this wallet' },
        { index: 1, kind: 'output', value: '100.000000 ADA to recipient' },
        { index: 2, kind: 'output', value: '9.700000 ADA to this wallet' },
      ],
      dependencies: [],
      conflicts: [],
      approvable: true,
      refusalReasons: [],
    },
  ],
} as any;

const commonProps = {
  assetDetails: {},
  deciding: false,
  phase: 'ready',
  canCancel: false,
  cancelling: false,
  onApprove: action('approve'),
  onReject: action('reject'),
  onCancel: action('cancel'),
} as const;

storiesOf('Common / Transaction approval', module)
  .add('Software payment', () => (
    <TransactionApprovalDialog {...commonProps} request={request} />
  ))
  .add('Hardware signing', () => (
    <TransactionApprovalDialog
      {...commonProps}
      request={{
        ...request,
        requestId: 'storybook-hardware',
        authorization: { kind: 'hardware', vendor: 'ledger' },
      }}
      deciding
      phase="waiting-for-device"
      canCancel
    />
  ))
  .add('Submission error', () => (
    <TransactionApprovalDialog
      {...commonProps}
      request={{ ...request, requestId: 'storybook-error' }}
      errorCode="transaction_plan_changed"
    />
  ));
