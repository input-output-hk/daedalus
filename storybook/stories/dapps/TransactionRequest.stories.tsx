import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import Notification from '../../../source/renderer/app/components/notifications/Notification';
import Cip30TransactionApproval from '../../../source/renderer/app/components/dapp/Cip30TransactionApproval';
import DappBatchReviewDialog from '../../../source/renderer/app/components/dapp/DappBatchReviewDialog';
import type { DappConsentPresentation } from '../../../source/common/ipc/api';
import { CIP30_REVIEW_EFFECTS } from '../../../source/common/cip30/review';

const review = {
  mode: 'sign' as const,
  transactionId: '11'.repeat(32),
  bodyCbor:
    'a30080018182581d601111111111111111111111111111111111111111111111111111111a1021a000f4240',
  fullCbor:
    '84a30080018182581d601111111111111111111111111111111111111111111111111111111a1021a000f4240a0f5f6',
  fullCborDigest: '22'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  effects: CIP30_REVIEW_EFFECTS.filter(
    (kind) => kind !== 'maximum-collateral-loss-unresolved'
  ).map((kind, index) => ({
    index,
    kind,
    value: JSON.stringify({ kind, example: true }, null, 2),
  })),
  maximumCollateralLoss: JSON.stringify(
    { coin: '5000000', assets: [] },
    null,
    2
  ),
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};

const request = (
  kind: 'transaction-sign' | 'transaction-submit',
  overrides = {}
) => ({
  requestId: 'storybook-review',
  kind,
  origin: 'https://example.test',
  walletName: 'Storybook wallet',
  networkName: 'Preview',
  scopes: [
    kind === 'transaction-sign'
      ? 'transaction-signing'
      : 'transaction-submission',
  ],
  extensions: [],
  review: {
    ...review,
    mode: kind === 'transaction-sign' ? ('sign' as const) : ('submit' as const),
    ...overrides,
  },
});

type BatchPresentation = Extract<
  DappConsentPresentation,
  { kind: 'batch-sign' | 'batch-submit' }
>;

const batchRequest = (
  kind: 'batch-sign' | 'batch-submit',
  blocked = false
): BatchPresentation => ({
  requestId: 'storybook-batch-review',
  kind,
  origin: 'https://example.test',
  walletName: 'Storybook wallet',
  networkName: 'Preview',
  scopes: [],
  extensions: [103],
  review: {
    mode: kind === 'batch-sign' ? 'sign' : 'submit',
    approvable: !blocked,
    ...(blocked ? { refusalIndex: 1 } : {}),
    items: [0, 1, 2].map((index) => ({
      index,
      dependencies:
        index === 0
          ? []
          : [
              {
                source: 'current-batch' as const,
                inputRole:
                  index === 2 ? ('reference' as const) : ('normal' as const),
                outpoint: {
                  transactionId: String(index).repeat(64),
                  index: 0,
                },
                sourceTransactionIndex: index - 1,
              },
            ],
      conflicts:
        index === 2
          ? [
              {
                inputRole: 'collateral' as const,
                outpoint: { transactionId: '11'.repeat(32), index: 0 },
                earlierTransactionIndex: 0,
              },
            ]
          : [],
      transaction: {
        ...review,
        mode: kind === 'batch-sign' ? ('sign' as const) : ('submit' as const),
        transactionId: String(index + 1).padStart(64, '0'),
        fullCborDigest: String(index + 11).padStart(64, '0'),
        effects: [
          {
            index: 0,
            kind: 'input',
            value: JSON.stringify({ item: index + 1 }),
          },
          {
            index: 1,
            kind: 'output',
            value: JSON.stringify({ item: index + 1 }),
          },
        ],
        ...(blocked && index === 1
          ? {
              commitmentsVerified: false,
              approvable: false,
              refusalReasons: ['unsupported-effect:future-effect'],
              effects: [{ index: 0, kind: 'future-effect', value: '{}' }],
            }
          : {}),
      },
    })),
  },
});

storiesOf('dApps / TransactionRequest', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('Signing review', () => (
    <Cip30TransactionApproval
      request={request('transaction-sign')}
      deciding={false}
      onApprove={action('sign')}
      onReject={action('reject')}
    />
  ))
  .add('Governance signing review', () => (
    <Cip30TransactionApproval
      request={request('transaction-sign', {
        effects: [
          {
            index: 0,
            kind: 'certificate',
            value: JSON.stringify(
              {
                kind: 16,
                credentialIdentities: [`key:${'33'.repeat(28)}`],
                targetCredentialIdentities: [],
              },
              null,
              2
            ),
          },
          {
            index: 1,
            kind: 'vote',
            value: JSON.stringify(
              { voter: `drep-key:${'33'.repeat(28)}`, vote: 'yes' },
              null,
              2
            ),
          },
        ],
      })}
      deciding={false}
      onApprove={action('sign governance transaction')}
      onReject={action('reject')}
    />
  ))
  .add('Submission review', () => (
    <Cip30TransactionApproval
      request={request('transaction-submit', {
        isValid: false,
        witnessSetCbor: 'a10081825820',
        auxiliaryDataCbor: 'a10101',
      })}
      deciding={false}
      onApprove={action('submit')}
      onReject={action('reject')}
    />
  ))
  .add('Incomplete review', () => (
    <Cip30TransactionApproval
      request={request('transaction-sign', {
        effects: [
          { index: 0, kind: 'maximum-collateral-loss-unresolved', value: '{}' },
          { index: 1, kind: 'future-effect', value: '{}' },
        ],
        commitmentsVerified: false,
        approvable: false,
        refusalReasons: ['datum:missing:unavailable'],
      })}
      deciding={false}
      onApprove={action('blocked')}
      onReject={action('reject')}
    />
  ))
  .add('Ordered batch signing review', () => (
    <DappBatchReviewDialog
      request={batchRequest('batch-sign')}
      deciding={false}
      onApprove={action('sign batch')}
      onReject={action('reject')}
    />
  ))
  .add('Blocked batch submission review', () => (
    <DappBatchReviewDialog
      request={batchRequest('batch-submit', true)}
      deciding={false}
      onApprove={action('blocked')}
      onReject={action('reject')}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('Notifications', ({ locale }: { locale: string }) => {
    let text1 = 'Opening transaction received via link...';
    let text2 = 'Transaction received via link';
    let actionBtn1 = 'Reject';
    let actionBtn2 = 'View';

    if (locale !== 'en-US') {
      text1 = 'リンク経由で受信したトランザクションを開いています...';
      text2 = 'リンク経由で受信したトランザクション';
      actionBtn1 = '拒否する';
      actionBtn2 = '表示する';
    }

    const actions = [
      {
        label: actionBtn1,
      },
      {
        label: actionBtn2,
        primary: true,
      },
    ];
    return (
      <div
        style={{
          overflow: 'hidden',
          height: 200,
        }}
      >
        <div>
          <Notification isVisible clickToClose={false} hasCloseButton={false}>
            {text1}
          </Notification>
        </div>
        <div
          style={{
            position: 'relative',
            marginTop: 72,
          }}
        >
          <Notification isVisible onClose={action('onClose')} actions={actions}>
            {text2}
          </Notification>
        </div>
      </div>
    );
  });
