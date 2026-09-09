import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { select, withKnobs } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import type { Cip30TransactionReview } from '../../../source/common/cip30/review';
import type { TransactionReviewDisplay } from '../../../source/common/transactions/reviewDisplay';
import type { Asset } from '../../../source/renderer/app/api/assets/types';
import type { TransactionApprovalRequest } from '../../../source/renderer/app/components/transactions/TransactionApprovalDialog.types';
import TransactionApprovalDialog from '../../../source/renderer/app/components/transactions/TransactionApprovalDialog';
import StoryDecorator from '../_support/StoryDecorator';

const display: TransactionReviewDisplay = {
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

const request: TransactionApprovalRequest = {
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
};

const usdmPolicy = 'aa'.repeat(28);
const qusdmPolicy = 'bb'.repeat(28);
const markerPolicy = 'cc'.repeat(28);
const longNamePolicy = 'dd'.repeat(28);
const blankNamePolicy = 'ee'.repeat(28);
const usdm = {
  policyId: usdmPolicy,
  assetName: '5553444d',
  fingerprint: 'asset1usdm8x8v6fq5r3w0v8rxmv2s9',
};
const qusdm = {
  policyId: qusdmPolicy,
  assetName: '715553444d',
  fingerprint: 'asset1qusdm6x8v6fq5r3w0v8rxmv2s',
};
const marker = {
  policyId: markerPolicy,
  assetName: '4c69717769644d61726b6572',
  fingerprint: 'asset1liqwidmarker8v6fq5r3w0v8rx',
};
const longName = {
  policyId: longNamePolicy,
  assetName: '4c6971776964207769746864726177616c207265636569707420746f6b656e21',
  fingerprint: 'asset1longname8v6fq5r3w0v8rxmv2s9',
};
const blankName = {
  policyId: blankNamePolicy,
  assetName: '',
  fingerprint: 'asset1blank8x8v6fq5r3w0v8rxmv2s9',
};
const walletInputAssets = [
  { ...usdm, quantity: '107452317294' },
  { ...qusdm, quantity: '3012277781903' },
  { ...longName, quantity: '1' },
  { ...blankName, quantity: '42' },
];
const walletOutputAssets = [
  { ...usdm, quantity: '182534584841' },
  { ...qusdm, quantity: '17' },
  { ...longName, quantity: '1' },
  { ...blankName, quantity: '42' },
];
const liqwidDisplay: TransactionReviewDisplay = {
  entries: [
    {
      effectIndex: 0,
      role: 'input',
      position: 0,
      outpoint: { transactionId: '10'.repeat(32), index: '0' },
      address: `addr1q${'w'.repeat(98)}`,
      control: 'key',
      ownership: 'wallet',
      value: { coin: '3828970000', assets: walletInputAssets },
      hasDatum: false,
      hasReferenceScript: false,
    },
    {
      effectIndex: 1,
      role: 'input',
      position: 1,
      outpoint: { transactionId: '11'.repeat(32), index: '1' },
      address: `addr1w${'s'.repeat(54)}`,
      control: 'script',
      ownership: 'other',
      value: {
        coin: '3000000',
        assets: [
          { ...usdm, quantity: '76032664693' },
          { ...marker, quantity: '1' },
        ],
      },
      hasDatum: true,
      hasReferenceScript: false,
    },
    {
      effectIndex: 2,
      role: 'output',
      position: 0,
      outpoint: null,
      address: `addr1q${'w'.repeat(98)}`,
      control: 'key',
      ownership: 'wallet',
      value: { coin: '3828523417', assets: walletOutputAssets },
      hasDatum: false,
      hasReferenceScript: false,
    },
    {
      effectIndex: 3,
      role: 'output',
      position: 1,
      outpoint: null,
      address: `addr1w${'s'.repeat(54)}`,
      control: 'script',
      ownership: 'other',
      value: {
        coin: '3000000',
        assets: [
          { ...usdm, quantity: '950397146' },
          { ...marker, quantity: '1' },
        ],
      },
      hasDatum: true,
      hasReferenceScript: true,
    },
    ...['20', '21', '22', '23'].map((prefix, position) => ({
      effectIndex: 4 + position,
      role: 'reference-input' as const,
      position,
      outpoint: { transactionId: prefix.repeat(32), index: String(position) },
      address: `addr1w${prefix.repeat(27)}`,
      control: 'script' as const,
      ownership: 'other' as const,
      value: {
        coin: ['3000000', '3000000', '22890000', '14780000'][position],
        assets: [],
      },
      hasDatum: true,
      hasReferenceScript: position === 2,
    })),
    {
      effectIndex: 8,
      role: 'collateral-input' as const,
      position: 0,
      outpoint: { transactionId: '30'.repeat(32), index: '0' },
      address: `addr1q${'w'.repeat(98)}`,
      control: 'key' as const,
      ownership: 'wallet' as const,
      value: { coin: '3828970000', assets: walletInputAssets },
      hasDatum: false,
      hasReferenceScript: false,
    },
    {
      effectIndex: 9,
      role: 'collateral-return' as const,
      position: 0,
      outpoint: null,
      address: `addr1q${'w'.repeat(98)}`,
      control: 'key' as const,
      ownership: 'wallet' as const,
      value: { coin: '3823970000', assets: walletInputAssets },
      hasDatum: false,
      hasReferenceScript: false,
    },
  ],
  walletInputs: { coin: '3828970000', assets: walletInputAssets },
  walletOutputs: { coin: '3828523417', assets: walletOutputAssets },
  walletChange: {
    coin: '-446583',
    assets: [
      { ...usdm, quantity: '75082267547' },
      { ...qusdm, quantity: '-3012277781886' },
    ],
  },
  fee: '446583',
  deposits: null,
  refunds: null,
  maximumCollateralLoss: { coin: '5000000', assets: [] },
  mint: [{ ...qusdm, quantity: '-3012277781886' }],
  withdrawals: [],
  certificates: [],
  votes: [],
  proposalCount: 0,
  donation: null,
};
const liqwidReview: Cip30TransactionReview = {
  mode: 'sign',
  transactionId: '99'.repeat(32),
  bodyCbor: 'a0',
  fullCbor: '84a0a0f5f6',
  fullCborDigest: '98'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  display: liqwidDisplay,
  effects: [],
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};
const liqwidRequest: TransactionApprovalRequest = {
  requestId: 'storybook-liqwid-withdrawal',
  requester: { kind: 'dapp', origin: 'https://app.liqwid.finance' },
  walletName: 'Ledger Flex X2',
  networkName: 'Mainnet',
  operation: 'sign',
  authorization: { kind: 'hardware', vendor: 'ledger' },
  collection: 'single',
  acknowledgements: [],
  items: [
    {
      index: 0,
      display: liqwidDisplay,
      evidence: { kind: 'exact-cbor', review: liqwidReview },
      effects: [],
      dependencies: [],
      conflicts: [],
      approvable: true,
      refusalReasons: [],
    },
  ],
};
const liqwidAssetDetails: Readonly<Record<string, Asset>> = {
  [`${usdmPolicy}${usdm.assetName}`]: {
    ...usdm,
    uniqueId: `${usdmPolicy}${usdm.assetName}`,
    decimals: 6,
    metadata: { name: 'USDM', ticker: 'USDM', description: 'Synthetic USDM' },
  },
  [`${qusdmPolicy}${qusdm.assetName}`]: {
    ...qusdm,
    uniqueId: `${qusdmPolicy}${qusdm.assetName}`,
    decimals: 6,
    metadata: {
      name: 'qUSDM',
      ticker: 'qUSDM',
      description: 'Synthetic qUSDM',
    },
  },
  [`${markerPolicy}${marker.assetName}`]: {
    ...marker,
    uniqueId: `${markerPolicy}${marker.assetName}`,
    decimals: 0,
    metadata: {
      name: 'Liqwid marker',
      ticker: 'MARKER',
      description: 'Synthetic marker',
    },
  },
  [`${longNamePolicy}${longName.assetName}`]: {
    ...longName,
    uniqueId: `${longNamePolicy}${longName.assetName}`,
    decimals: 0,
    metadata: {
      name: 'Liqwid withdrawal receipt token with an intentionally long name',
      description: 'Synthetic long-name token',
    },
  },
  [`${blankNamePolicy}${blankName.assetName}`]: {
    ...blankName,
    uniqueId: `${blankNamePolicy}${blankName.assetName}`,
    decimals: 0,
  },
};
const unknownAssetDetails: Readonly<Record<string, Asset>> = {
  [`${usdmPolicy}${usdm.assetName}`]: liqwidAssetDetails[
    `${usdmPolicy}${usdm.assetName}`
  ],
  [`${markerPolicy}${marker.assetName}`]: liqwidAssetDetails[
    `${markerPolicy}${marker.assetName}`
  ],
  [`${longNamePolicy}${longName.assetName}`]: liqwidAssetDetails[
    `${longNamePolicy}${longName.assetName}`
  ],
};
const unavailableRequest: TransactionApprovalRequest = {
  ...liqwidRequest,
  requestId: 'storybook-liqwid-unavailable',
  items: [
    {
      ...liqwidRequest.items[0],
      display: {
        ...liqwidDisplay,
        walletChange: null,
        maximumCollateralLoss: null,
        entries: liqwidDisplay.entries.map((entry) => ({
          ...entry,
          value: null,
        })),
      },
      approvable: false,
      refusalReasons: ['maximum-collateral-loss-unresolved'],
    },
  ],
};

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

const receiptTransactionId = '44'.repeat(32);
const selfTransferDisplay: TransactionReviewDisplay = {
  ...display,
  walletInputs: { coin: '10000000', assets: [] },
  walletOutputs: { coin: '9700000', assets: [] },
  walletChange: { coin: '-300000', assets: [] },
  entries: [
    { ...display.entries[0], value: { coin: '10000000', assets: [] } },
    {
      ...display.entries[1],
      ownership: 'wallet',
      address: display.entries[0].address,
      value: { coin: '5000000', assets: [] },
    },
    { ...display.entries[2], value: { coin: '4700000', assets: [] } },
  ],
};
const selfTransferRequest: TransactionApprovalRequest = {
  ...request,
  authorization: { kind: 'hardware', vendor: 'ledger' },
  items: [
    {
      ...request.items[0],
      display: selfTransferDisplay,
      evidence: {
        kind: 'exact-cbor',
        review: {
          ...liqwidReview,
          transactionId: receiptTransactionId,
          display: selfTransferDisplay,
        },
      },
      effects: [],
    },
  ],
};
const selfTransferReceipt = {
  id: receiptTransactionId,
  fee: new BigNumber('0.3'),
  amount: new BigNumber('-0.3'),
  transferAmount: new BigNumber(5),
  isSelfTransfer: true,
  amountIsKnown: true,
};

storiesOf('Common / Transaction approval', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  .add('Live submission receipt', () => {
    const state = select(
      'Backend state',
      {
        'Submitted — awaiting confirmation': 'pending',
        Confirmed: 'in_ledger',
        Expired: 'expired',
        Failed: 'failed',
        'Submission status unknown': 'submission-unknown',
      } as const,
      'pending'
    );
    return (
      <TransactionApprovalDialog
        {...commonProps}
        request={selfTransferRequest}
        result={{
          status:
            state === 'submission-unknown' ? 'submission-unknown' : 'submitted',
          transactionIds: [receiptTransactionId],
        }}
        receipts={[
          {
            ...selfTransferReceipt,
            state,
            ...(state === 'in_ledger' ? { confirmations: 3 } : {}),
          },
        ]}
        onDismiss={action('dismiss receipt')}
        onViewTransaction={action('view transaction in wallet history')}
      />
    );
  })
  .add('Signed only receipt', () => (
    <TransactionApprovalDialog
      {...commonProps}
      request={{ ...liqwidRequest, operation: 'sign' }}
      result={{
        status: 'signed',
        transactionIds: [liqwidReview.transactionId],
      }}
      onDismiss={action('dismiss signed-only receipt')}
    />
  ))
  .add('Partial submission receipt', () => (
    <TransactionApprovalDialog
      {...commonProps}
      request={{
        ...selfTransferRequest,
        collection: 'migration',
        items: [
          selfTransferRequest.items[0],
          { ...selfTransferRequest.items[0], index: 1 },
        ],
      }}
      result={{
        status: 'partial',
        transactionIds: [receiptTransactionId],
        failedIndex: 1,
        errorCode: 'TxSignError.UserDeclined',
      }}
      receipts={[
        { ...selfTransferReceipt, state: 'in_ledger', confirmations: 2 },
      ]}
      onDismiss={action('dismiss partial receipt')}
      onViewTransaction={action('view submitted transaction')}
    />
  ))
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
  .add('Liqwid withdrawal reference', () => (
    <TransactionApprovalDialog
      {...commonProps}
      assetDetails={liqwidAssetDetails}
      request={liqwidRequest}
    />
  ))
  .add('Unknown token metadata', () => (
    <TransactionApprovalDialog
      {...commonProps}
      assetDetails={unknownAssetDetails}
      request={{ ...liqwidRequest, requestId: 'storybook-liqwid-unknown' }}
    />
  ))
  .add('Unavailable review', () => (
    <TransactionApprovalDialog
      {...commonProps}
      assetDetails={liqwidAssetDetails}
      request={unavailableRequest}
    />
  ))
  .add('Submission error', () => (
    <TransactionApprovalDialog
      {...commonProps}
      request={{ ...request, requestId: 'storybook-error' }}
      errorCode="transaction_plan_changed"
    />
  ));
