import React from 'react';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import WalletApprovalContainer from './WalletApprovalContainer';

jest.mock(
  '../../components/dapp-consent/DappConsentDialog',
  () =>
    function ConnectionConsent() {
      return <div data-testid="connection-consent" />;
    }
);
jest.mock(
  '../../components/transactions/TransactionApprovalDialog',
  () =>
    function TransactionConsent({ request }: any) {
      return (
        <div
          data-testid={
            request.collection === 'single'
              ? 'transaction-consent'
              : 'batch-consent'
          }
        />
      );
    }
);
jest.mock(
  '../../components/dapp-consent/DappDataSignApproval',
  () =>
    function DataSignConsent() {
      return <div data-testid="data-sign-consent" />;
    }
);

const identity = {
  requestId: 'request',
  walletId: 'aa'.repeat(20),
  origin: 'https://dapp.test',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: [],
  extensions: [],
};
const shared = {
  assetDetails: {},
  deciding: false,
  phase: 'ready' as const,
  submissionAuthorized: false,
  onApprove: jest.fn(),
  onReject: jest.fn(),
};

describe('WalletApprovalContainer', () => {
  afterEach(cleanup);

  it('routes connection and transaction presentations through one global mount', () => {
    const { rerender } = render(
      <WalletApprovalContainer
        {...shared}
        request={{ ...identity, kind: 'connection' }}
        deciding={false}
        onApprove={jest.fn()}
        onReject={jest.fn()}
      />
    );
    expect(screen.getByTestId('connection-consent')).toBeVisible();

    rerender(
      <WalletApprovalContainer
        {...shared}
        request={{
          ...identity,
          kind: 'data-sign',
          review: {
            address: `60${'11'.repeat(28)}`,
            credentialKind: 'payment',
            payload: '00',
            utf8Preview: null,
          },
        }}
        deciding={false}
        onApprove={jest.fn()}
        onReject={jest.fn()}
      />
    );
    expect(screen.getByTestId('data-sign-consent')).toBeVisible();

    rerender(
      <WalletApprovalContainer
        {...shared}
        request={{
          ...identity,
          kind: 'transaction-sign',
          authorization: { kind: 'software' },
          review: {
            mode: 'sign',
            transactionId: '11'.repeat(32),
            bodyCbor: 'a0',
            fullCbor: '84a0a0f5f6',
            fullCborDigest: '22'.repeat(32),
            witnessSetCbor: 'a0',
            auxiliaryDataCbor: 'f6',
            isValid: true,
            display: {
              entries: [],
              walletInputs: null,
              walletOutputs: null,
              walletChange: null,
              fee: '0',
              deposits: null,
              refunds: null,
              maximumCollateralLoss: null,
              mint: [],
              withdrawals: [],
              certificates: [],
              votes: [],
              proposalCount: 0,
              donation: null,
            },
            effects: [],
            existingVkeyWitnesses: [],
            existingBootstrapWitnesses: [],
            commitmentsVerified: true,
            approvable: true,
            refusalReasons: [],
          },
        }}
        deciding={false}
        onApprove={jest.fn()}
        onReject={jest.fn()}
      />
    );
    expect(screen.getByTestId('transaction-consent')).toBeVisible();

    rerender(
      <WalletApprovalContainer
        {...shared}
        request={{
          ...identity,
          kind: 'batch-sign',
          authorization: { kind: 'software' },
          review: {
            mode: 'sign',
            approvable: false,
            refusalIndex: 0,
            items: [],
          },
        }}
        deciding={false}
        onApprove={jest.fn()}
        onReject={jest.fn()}
      />
    );
    expect(screen.getByTestId('batch-consent')).toBeVisible();
  });
});
