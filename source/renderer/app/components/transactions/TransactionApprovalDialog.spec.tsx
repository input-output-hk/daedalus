import React from 'react';
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import { IntlProvider } from 'react-intl';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../i18n/locales/en-US.json';
import { TransactionApprovalDialog } from './TransactionApprovalDialog';
import type { TransactionApprovalDialogProps } from './TransactionApprovalDialog.types';

const display = {
  entries: [
    {
      effectIndex: 0,
      role: 'input' as const,
      position: 0,
      outpoint: { transactionId: '11'.repeat(32), index: '0' },
      address: `addr_test1${'q'.repeat(40)}`,
      control: 'key' as const,
      ownership: 'wallet' as const,
      value: { coin: '10000000', assets: [] },
      hasDatum: false,
      hasReferenceScript: false,
    },
    {
      effectIndex: 1,
      role: 'output' as const,
      position: 0,
      outpoint: { transactionId: '22'.repeat(32), index: '0' },
      address: `addr_test1${'p'.repeat(40)}`,
      control: 'key' as const,
      ownership: 'other' as const,
      value: { coin: '7000000', assets: [] },
      hasDatum: false,
      hasReferenceScript: false,
    },
  ],
  walletInputs: { coin: '10000000', assets: [] },
  walletOutputs: { coin: '0', assets: [] },
  walletChange: { coin: '-10000000', assets: [] },
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
const review = {
  mode: 'sign' as const,
  transactionId: '22'.repeat(32),
  bodyCbor: 'a0',
  fullCbor: '84a0a0f5f6',
  fullCborDigest: '33'.repeat(32),
  witnessSetCbor: 'a0',
  auxiliaryDataCbor: 'f6',
  isValid: true,
  display,
  effects: [
    { index: 0, kind: 'input', value: '{}' },
    { index: 1, kind: 'output', value: '{}' },
  ],
  existingVkeyWitnesses: [],
  existingBootstrapWitnesses: [],
  commitmentsVerified: true,
  approvable: true,
  refusalReasons: [],
};
const request = {
  requestId: 'review',
  requester: { kind: 'dapp' as const, origin: 'https://example.test' },
  walletName: 'Savings',
  networkName: 'Preview',
  operation: 'sign' as const,
  authorization: { kind: 'software' as const },
  collection: 'single' as const,
  acknowledgements: [],
  items: [
    {
      index: 0,
      display,
      evidence: { kind: 'exact-cbor' as const, review },
      effects: review.effects,
      dependencies: [],
      conflicts: [],
      approvable: true,
      refusalReasons: [],
    },
  ],
};
const props: TransactionApprovalDialogProps = {
  request,
  assetDetails: {},
  deciding: false,
  phase: 'ready',
  canCancel: false,
  cancelling: false,
  onApprove: jest.fn(),
  onReject: jest.fn(),
  onCancel: jest.fn(),
};
const renderDialog = (
  overrides: Partial<TransactionApprovalDialogProps> = {}
) =>
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <TransactionApprovalDialog
          intl={
            {
              formatMessage: ({ defaultMessage }: any, values: any = {}) =>
                String(defaultMessage).replace(
                  /\{(\w+)\}/g,
                  (_: string, key: string) => values[key]
                ),
            } as any
          }
          {...props}
          {...overrides}
        />
      </IntlProvider>
    </StoryDecorator>
  );

describe('TransactionApprovalDialog', () => {
  afterEach(cleanup);

  it('shows wallet impact, fee, readable entries, and collapsed exact bytes', async () => {
    renderDialog();
    await waitFor(() =>
      expect(screen.getByText('!!!Review transaction')).toHaveFocus()
    );
    expect(screen.getByText('−10.000000 ADA')).toBeVisible();
    expect(screen.getByText('0.300000 ADA')).toBeVisible();
    expect(screen.getByText('!!!This wallet')).toBeVisible();
    expect(screen.queryByText(review.bodyCbor)).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: /Technical details/ }));
    expect(screen.getByText(review.bodyCbor)).toBeVisible();
  });

  it('requires a software password and clears it before approval', () => {
    const onApprove = jest.fn();
    renderDialog({ onApprove });
    const approve = screen.getByRole('button', { name: '!!!Sign transaction' });
    expect(approve).toBeDisabled();
    fireEvent.change(screen.getByLabelText('!!!Wallet spending password'), {
      target: { value: 'secret' },
    });
    expect(approve).toBeEnabled();
    fireEvent.click(approve);
    expect(onApprove).toHaveBeenCalledWith('secret');
    expect(screen.getByLabelText('!!!Wallet spending password')).toHaveValue(
      ''
    );
  });

  it('requires action-specific consent before hardware authorization', () => {
    renderDialog({
      request: {
        ...request,
        authorization: { kind: 'hardware', vendor: 'ledger' },
        acknowledgements: ['flight-mainnet-funds'],
      },
    });
    const approve = screen.getByRole('button', {
      name: '!!!Continue on device',
    });
    expect(approve).toBeDisabled();
    fireEvent.click(
      screen.getByLabelText(
        '!!!I understand that this Flight build uses real mainnet funds and the transaction is irreversible.'
      )
    );
    expect(approve).toBeEnabled();
  });
});
