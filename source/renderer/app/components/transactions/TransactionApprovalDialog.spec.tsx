import React from 'react';
import BigNumber from 'bignumber.js';
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
  within,
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
const dialogElement = (
  overrides: Partial<TransactionApprovalDialogProps> = {}
) => (
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

const renderDialog = (
  overrides: Partial<TransactionApprovalDialogProps> = {}
) => render(dialogElement(overrides));

describe('TransactionApprovalDialog', () => {
  afterEach(cleanup);

  it('updates an open submission receipt without offering another send or losing its transaction', async () => {
    const result = {
      status: 'submitted' as const,
      transactionIds: [review.transactionId],
    };
    const onViewTransaction = jest.fn();
    const completedProps: Partial<TransactionApprovalDialogProps> = {
      request: { ...request, operation: 'sign-and-submit' },
      result,
      receipts: [
        {
          id: review.transactionId,
          state: 'pending',
          amount: new BigNumber('-0.3'),
          fee: new BigNumber('0.3'),
          transferAmount: new BigNumber(5),
          isSelfTransfer: true,
          amountIsKnown: true,
        },
      ],
      onDismiss: jest.fn(),
      onViewTransaction,
    };
    const view = renderDialog(completedProps);
    const pending = screen.getByRole('status');
    expect(within(pending).getByText('5.000000 ADA')).toBeVisible();
    expect(within(pending).getByText('0.300000 ADA')).toBeVisible();
    expect(within(pending).getByText('-0.300000 ADA')).toBeVisible();
    expect(
      screen.queryByRole('button', { name: /Sign and send/ })
    ).not.toBeInTheDocument();
    await waitFor(() =>
      expect(within(pending).getByRole('heading')).toHaveFocus()
    );

    view.rerender(
      dialogElement({
        ...completedProps,
        receipts: [
          {
            ...completedProps.receipts![0],
            state: 'in_ledger',
            confirmations: 4,
          },
        ],
      })
    );
    expect(screen.getByRole('status')).toHaveTextContent(
      translations['transaction.approval.result.confirmedMessage']
    );
    fireEvent.click(screen.getByRole('button', { name: /View transaction/ }));
    expect(onViewTransaction).toHaveBeenCalledWith(review.transactionId);
  });

  it('shows wallet impact, fee, readable entries, and collapsed exact bytes', async () => {
    renderDialog();
    await waitFor(() =>
      expect(screen.getByText('!!!Review transaction')).toHaveFocus()
    );
    expect(screen.getAllByText('−10.000000 ADA').length).toBeGreaterThan(0);
    expect(screen.getByText('−0.300000 ADA')).toBeVisible();
    expect(screen.getByText('!!!This wallet')).toBeVisible();
    expect(screen.queryByText(review.bodyCbor)).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: /Technical details/ }));
    expect(screen.getByText(review.bodyCbor)).toBeVisible();
  });

  it('keeps net changes visible while gross assets disclose exact identities', () => {
    const usdm = {
      policyId: 'aa'.repeat(28),
      assetName: '5553444d',
      fingerprint: 'asset1usdm8x8v6fq5r3w0v8rxmv2s9',
    };
    const qusdm = {
      policyId: 'bb'.repeat(28),
      assetName: '715553444d',
      fingerprint: 'asset1qusdm6x8v6fq5r3w0v8rxmv2s',
    };
    const longName = {
      policyId: 'cc'.repeat(28),
      assetName:
        '4c6971776964207769746864726177616c207265636569707420746f6b656e21',
      fingerprint: 'asset1longname8v6fq5r3w0v8rxmv2s9',
    };
    const unknown = {
      policyId: 'dd'.repeat(28),
      assetName: '',
      fingerprint: 'asset1unknown8v6fq5r3w0v8rxmv2s9',
    };
    const richDisplay = {
      ...display,
      entries: [
        {
          ...display.entries[0],
          value: {
            coin: '10000000',
            assets: [
              { ...usdm, quantity: '107452317294' },
              { ...qusdm, quantity: '3012277781903' },
              { ...longName, quantity: '1' },
              { ...unknown, quantity: '9' },
            ],
          },
        },
        display.entries[1],
      ],
      walletChange: {
        coin: '-10000000',
        assets: [
          { ...usdm, quantity: '75082267547' },
          { ...qusdm, quantity: '-3012277781886' },
          { ...unknown, quantity: '-9' },
        ],
      },
    };
    const onApprove = jest.fn();
    renderDialog({
      onApprove,
      assetDetails: {
        [`${usdm.policyId}${usdm.assetName}`]: {
          ...usdm,
          uniqueId: `${usdm.policyId}${usdm.assetName}`,
          decimals: 6,
          metadata: {
            name: 'USDM',
            ticker: 'USDM',
            description: 'Synthetic USDM',
          },
        },
        [`${qusdm.policyId}${qusdm.assetName}`]: {
          ...qusdm,
          uniqueId: `${qusdm.policyId}${qusdm.assetName}`,
          decimals: 6,
          metadata: {
            name: 'qUSDM',
            ticker: 'qUSDM',
            description: 'Synthetic qUSDM',
          },
        },
        [`${longName.policyId}${longName.assetName}`]: {
          ...longName,
          uniqueId: `${longName.policyId}${longName.assetName}`,
          decimals: 0,
          metadata: {
            name: 'Liqwid withdrawal receipt token',
            description: 'Synthetic receipt',
          },
        },
      },
      request: {
        ...request,
        authorization: { kind: 'hardware', vendor: 'ledger' },
        items: [
          {
            ...request.items[0],
            display: richDisplay,
            approvable: false,
            refusalReasons: ['maximum-collateral-loss-unresolved'],
          },
        ],
      },
    });

    expect(screen.getByText('+75,082.267547')).toBeVisible();
    expect(screen.getByText('−3,012,277.781886')).toBeVisible();
    expect(
      within(
        screen.getByText('!!!Leaving your wallet').parentElement as HTMLElement
      ).getByText('−!!!9 base units — decimals unknown')
    ).toBeVisible();
    const more = screen.getByText('!!!+2 more assets');
    const moreDisclosure = more.closest('details') as HTMLDetailsElement;
    expect(moreDisclosure.open).toBe(false);
    fireEvent.click(more);
    expect(moreDisclosure.open).toBe(true);
    expect(
      screen.getAllByText('Liqwid withdrawal receipt token').length
    ).toBeGreaterThan(0);

    const [asset] = screen.getAllByLabelText(
      `!!!Asset details: ${usdm.fingerprint}`
    );
    fireEvent.click(asset);
    expect(screen.getAllByText(usdm.fingerprint).length).toBeGreaterThan(0);

    const approve = screen.getByRole('button', {
      name: '!!!Continue on device',
    });
    expect(approve).toBeDisabled();
    expect(onApprove).not.toHaveBeenCalled();
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
