import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import type { Cip30TransactionReview } from '../../../../common/cip30/review';
import en from '../../i18n/locales/en-US.json';
import ja from '../../i18n/locales/ja-JP.json';
import DappBatchReviewDialog from './DappBatchReviewDialog';

type BatchPresentation = Extract<
  DappConsentPresentation,
  { kind: 'batch-sign' | 'batch-submit' }
>;

const transaction = (
  index: number,
  overrides: Partial<Cip30TransactionReview> = {}
): Cip30TransactionReview => ({
  mode: 'sign',
  transactionId: String(index + 1).padStart(64, '0'),
  bodyCbor: 'a0',
  fullCbor: '84a0a0f5f6',
  fullCborDigest: String(index + 11).padStart(64, '0'),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  effects: [
    { index: 0, kind: 'input', value: `{"item":${index + 1}}` },
    { index: 1, kind: 'output', value: `{"item":${index + 1}}` },
  ],
  maximumCollateralLoss: `{"coin":"${index + 1}000000"}`,
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
  ...overrides,
});

const presentation = (
  kind: 'batch-sign' | 'batch-submit',
  blocked = false
): BatchPresentation => ({
  requestId: 'batch-review',
  kind,
  origin: 'https://dapp.test',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: [],
  extensions: [103],
  review: {
    mode: kind === 'batch-sign' ? 'sign' : 'submit',
    approvable: !blocked,
    ...(blocked ? { refusalIndex: 1 } : {}),
    items: [
      {
        index: 0,
        dependencies: [],
        conflicts: [],
        transaction: transaction(0, {
          mode: kind === 'batch-sign' ? 'sign' : 'submit',
        }),
      },
      {
        index: 1,
        dependencies: [
          {
            source: 'current-batch',
            inputRole: 'normal',
            outpoint: { transactionId: '11'.repeat(32), index: 0 },
            sourceTransactionIndex: 0,
          },
          {
            source: 'pending-submission',
            inputRole: 'reference',
            outpoint: { transactionId: '22'.repeat(32), index: 1 },
          },
        ],
        conflicts: [
          {
            inputRole: 'collateral',
            outpoint: { transactionId: '11'.repeat(32), index: 0 },
            earlierTransactionIndex: 0,
          },
        ],
        transaction: transaction(1, {
          mode: kind === 'batch-sign' ? 'sign' : 'submit',
          ...(blocked
            ? {
                approvable: false,
                commitmentsVerified: false,
                refusalReasons: ['unsupported-effect:future-effect'],
                effects: [{ index: 0, kind: 'future-effect', value: '{}' }],
              }
            : {}),
        }),
      },
    ],
  },
});

const renderDialog = (
  request: BatchPresentation,
  locale: 'en-US' | 'ja-JP' = 'en-US',
  onApprove = jest.fn(),
  deciding = false
) => {
  const result = render(
    <StoryDecorator>
      <IntlProvider locale={locale} messages={locale === 'en-US' ? en : ja}>
        <DappBatchReviewDialog
          request={request}
          deciding={deciding}
          onApprove={onApprove}
          onReject={jest.fn()}
        />
      </IntlProvider>
    </StoryDecorator>
  );
  return { ...result, onApprove };
};

describe('DappBatchReviewDialog', () => {
  afterEach(cleanup);

  it('keeps ordered conflicts, dependencies, effects, collateral, and focus per item', () => {
    const { onApprove } = renderDialog(presentation('batch-sign'));

    expect(
      screen
        .getAllByRole('article')
        .map((item) => item.getAttribute('aria-labelledby'))
    ).toEqual(['cip103-review-item-0', 'cip103-review-item-1']);
    expect(screen.getByRole('heading', { name: 'Item 1 of 2' })).toBeVisible();
    expect(screen.getByRole('heading', { name: 'Item 2 of 2' })).toBeVisible();
    expect(screen.getByText('normal input depends on item 1')).toBeVisible();
    expect(
      screen.getByText('reference input depends on a pending wallet submission')
    ).toBeVisible();
    expect(
      screen.getByText('collateral input is already claimed by item 1')
    ).toBeVisible();
    expect(screen.getAllByText('Effects for this item only')).toHaveLength(2);
    expect(screen.getAllByText('Maximum collateral loss')).toHaveLength(2);
    expect(screen.queryByText(/batch total:/u)).not.toBeInTheDocument();

    const password = screen.getByLabelText('Wallet spending password');
    expect(screen.getByLabelText('Modal Dialog')).toHaveFocus();
    const approve = screen.getByRole('button', {
      name: 'Sign all transactions',
    });
    expect(approve).toBeDisabled();
    fireEvent.change(password, { target: { value: 'secret' } });
    fireEvent.click(approve);
    expect(onApprove).toHaveBeenCalledWith('secret');
  });

  it('keeps submission review and recovery separate without a password', () => {
    renderDialog(presentation('batch-submit'));

    expect(screen.getByText(/every item is attempted in order/u)).toBeVisible();
    expect(screen.getByText(/pending wallet history remains/iu)).toBeVisible();
    expect(
      screen.queryByLabelText('Wallet spending password')
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Submit all transactions' })
    ).toBeEnabled();
  });

  it('keeps device-progress controls disabled while the approved batch executes', () => {
    renderDialog(presentation('batch-sign'), 'en-US', jest.fn(), true);

    expect(screen.getByText(/one at a time in this order/u)).toBeVisible();
    expect(screen.getByLabelText('Wallet spending password')).toBeDisabled();
    expect(
      screen.getByRole('button', { name: 'Sign all transactions' })
    ).toBeDisabled();
    expect(screen.getByRole('button', { name: 'Reject' })).toBeDisabled();
  });

  it('blocks the whole review before confirmation when one item is unsupported', () => {
    const { onApprove } = renderDialog(presentation('batch-sign', true));

    expect(
      screen.getByText(
        'Item 2 cannot be approved. No host or hardware confirmation will start.'
      )
    ).toBeVisible();
    expect(screen.getByText('unsupported-effect:future-effect')).toBeVisible();
    const approve = screen.getByRole('button', {
      name: 'Sign all transactions',
    });
    expect(approve).toBeDisabled();
    fireEvent.click(approve);
    expect(onApprove).not.toHaveBeenCalled();
  });

  it('renders polished Japanese batch and device guidance', () => {
    renderDialog(presentation('batch-sign'), 'ja-JP');

    expect(screen.getByRole('heading', { name: '2件中1件目' })).toBeVisible();
    expect(screen.getByText(/ハードウェアウォレット/u)).toBeVisible();
    expect(
      screen.getByRole('button', { name: 'すべてのトランザクションに署名' })
    ).toBeDisabled();
  });
});
