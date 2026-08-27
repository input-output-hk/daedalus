import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import Notification from '../../../source/renderer/app/components/notifications/Notification';
import Cip30TransactionApproval from '../../../source/renderer/app/components/dapp/Cip30TransactionApproval';
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
