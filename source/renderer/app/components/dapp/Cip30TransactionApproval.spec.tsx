import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import {
  CIP30_REVIEW_EFFECTS,
  Cip30TransactionReview,
} from '../../../../common/cip30/review';
import type { DappConsentPresentation } from '../../../../common/ipc/api';
import Cip30TransactionApproval from './Cip30TransactionApproval';

const review: Cip30TransactionReview = {
  mode: 'sign',
  transactionId: '11'.repeat(32),
  bodyCbor: 'a10001',
  fullCbor: '84a10001a0f5f6',
  fullCborDigest: '22'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  effects: CIP30_REVIEW_EFFECTS.filter(
    (kind) => kind !== 'maximum-collateral-loss-unresolved'
  ).map((kind, index) => ({ index, kind, value: JSON.stringify({ kind }) })),
  maximumCollateralLoss: JSON.stringify({ coin: '5000000', assets: [] }),
  existingVkeyWitnesses: ['820102'],
  existingBootstrapWitnesses: [],
  auxiliaryDataHash: '33'.repeat(32),
  scriptDataHash: '44'.repeat(32),
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};
const presentation = (
  kind: 'transaction-sign' | 'transaction-submit',
  overrides: Partial<Cip30TransactionReview> = {}
): Extract<
  DappConsentPresentation,
  { kind: 'transaction-sign' | 'transaction-submit' }
> => ({
  requestId: 'request',
  kind,
  origin: 'https://dapp.test/<script>',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['transaction-signing'],
  extensions: [],
  review: {
    ...review,
    mode: kind === 'transaction-sign' ? 'sign' : 'submit',
    ...overrides,
  },
});

const renderApproval = (
  request: ReturnType<typeof presentation>,
  onApprove = jest.fn()
) => {
  const result = render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <Cip30TransactionApproval
          request={request}
          deciding={false}
          onApprove={onApprove}
          onReject={jest.fn()}
        />
      </IntlProvider>
    </StoryDecorator>
  );
  return { ...result, onApprove };
};

describe('Cip30TransactionApproval', () => {
  afterEach(cleanup);

  it('renders every approvable effect in order and the signing collateral bound', () => {
    const { container, onApprove } = renderApproval(
      presentation('transaction-sign')
    );
    const headers = [...document.querySelectorAll('h2')]
      .map((heading) => heading.textContent?.replace(/View$/u, ''))
      .filter((text) => text?.match(/^\d+\./u));
    expect(headers).toEqual(
      CIP30_REVIEW_EFFECTS.filter(
        (kind) => kind !== 'maximum-collateral-loss-unresolved'
      ).map((kind, index) => `${index + 1}. ${kind}`)
    );
    expect(screen.getByText('Signing body hash')).toBeVisible();
    expect(screen.getByText('Maximum collateral loss')).toBeVisible();
    expect(screen.getByText(/isValid flag is not signed/u)).toBeVisible();
    expect(container.querySelector('script')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Sign transaction' }));
    expect(onApprove).toHaveBeenCalledTimes(1);
  });

  it('shows exact outer submission identity separately from the body', () => {
    renderApproval(
      presentation('transaction-submit', {
        fullCbor: '84a10001a10081820102f4a10101',
        witnessSetCbor: 'a10081820102',
        auxiliaryDataCbor: 'a10101',
        isValid: false,
      })
    );
    expect(screen.getByText('Submitted isValid: false')).toBeVisible();
    expect(screen.getByText('Submitted envelope digest')).toBeVisible();
    expect(screen.getByText('Exact witness set CBOR')).toBeVisible();
    expect(screen.getByText('Exact auxiliary data CBOR')).toBeVisible();
    expect(
      screen.getByRole('button', { name: 'Submit transaction' })
    ).toBeEnabled();
  });

  it('fails closed for incomplete, unknown, or unresolved collateral review', () => {
    const request = presentation('transaction-sign', {
      approvable: false,
      commitmentsVerified: false,
      refusalReasons: ['datum:missing'],
      effects: [
        { index: 0, kind: 'maximum-collateral-loss-unresolved', value: '{}' },
        { index: 1, kind: 'future-effect', value: '{}' },
      ],
    });
    const { onApprove } = renderApproval(request);
    const approve = screen.getByRole('button', { name: 'Sign transaction' });
    expect(screen.getByRole('alert')).toBeVisible();
    expect(
      screen.getByText('1. maximum-collateral-loss-unresolved')
    ).toBeVisible();
    expect(approve).toBeDisabled();
    fireEvent.click(approve);
    expect(onApprove).not.toHaveBeenCalled();
  });
});
