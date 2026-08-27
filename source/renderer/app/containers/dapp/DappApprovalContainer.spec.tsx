import React from 'react';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import DappApprovalContainer from './DappApprovalContainer';

jest.mock(
  '../../components/dapp-consent/DappConsentDialog',
  () =>
    function ConnectionConsent() {
      return <div data-testid="connection-consent" />;
    }
);
jest.mock(
  '../../components/dapp/Cip30TransactionApproval',
  () =>
    function TransactionConsent() {
      return <div data-testid="transaction-consent" />;
    }
);

const identity = {
  requestId: 'request',
  origin: 'https://dapp.test',
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: [],
  extensions: [],
};

describe('DappApprovalContainer', () => {
  afterEach(cleanup);

  it('routes connection and transaction presentations through one global mount', () => {
    const { rerender } = render(
      <DappApprovalContainer
        request={{ ...identity, kind: 'connection' }}
        deciding={false}
        onApprove={jest.fn()}
        onReject={jest.fn()}
      />
    );
    expect(screen.getByTestId('connection-consent')).toBeVisible();

    rerender(
      <DappApprovalContainer
        request={{
          ...identity,
          kind: 'transaction-sign',
          review: {
            mode: 'sign',
            transactionId: '11'.repeat(32),
            bodyCbor: 'a0',
            fullCbor: '84a0a0f5f6',
            fullCborDigest: '22'.repeat(32),
            witnessSetCbor: 'a0',
            auxiliaryDataCbor: 'f6',
            isValid: true,
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
  });
});
