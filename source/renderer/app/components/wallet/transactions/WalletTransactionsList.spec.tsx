import React from 'react';
import BigNumber from 'bignumber.js';
import {
  cleanup,
  fireEvent,
  render,
  waitFor,
  within,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import { IntlProvider } from 'react-intl';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import {
  TransactionStates,
  TransactionTypes,
  WalletTransaction,
} from '../../../domains/WalletTransaction';
import { noopAnalyticsTracker } from '../../../analytics';
import {
  BrowserLocalStorageBridge,
  DiscreetModeFeatureProvider,
} from '../../../features';
import WalletTransactionsList from './WalletTransactionsList';
import translations from '../../../i18n/locales/en-US.json';

const transaction = (
  id: string,
  overrides: Partial<WalletTransaction> = {}
): WalletTransaction =>
  new WalletTransaction({
    id,
    type: TransactionTypes.EXPEND,
    title: '',
    amount: new BigNumber('-0.17'),
    fee: new BigNumber('0.17'),
    deposit: new BigNumber(0),
    assets: [],
    date: new Date('2026-09-09T12:00:00.000Z'),
    description: '',
    addresses: {
      from: ['addr_test1sender'],
      to: ['addr_test1receiver'],
      withdrawals: [],
    },
    state: TransactionStates.PENDING,
    confirmations: 0,
    slotNumber: null,
    epochNumber: null,
    metadata: null,
    isSelfTransfer: false,
    amountIsKnown: true,
    hasCertificates: false,
    ...overrides,
  });

const list = (
  transactions: WalletTransaction[],
  selectedTransactionId?: string,
  deletePendingTransaction = jest.fn()
) => (
  <StoryDecorator>
    <IntlProvider locale="en-US" messages={translations}>
      <BrowserLocalStorageBridge>
        <DiscreetModeFeatureProvider>
          <WalletTransactionsList
            transactions={transactions}
            selectedTransactionId={selectedTransactionId}
            deletePendingTransaction={deletePendingTransaction}
            formattedWalletAmount={(amount: BigNumber) => amount.toString()}
            hasMoreToLoad={false}
            isLoadingTransactions={false}
            isRestoreActive={false}
            isRenderingAsVirtualList={false}
            onOpenExternalLink={jest.fn()}
            getUrlByType={jest.fn()}
            walletId="wallet-1"
            isDeletingTransaction={false}
            currentDateFormat="YYYY-MM-DD"
            currentTimeFormat="HH:mm:ss"
            hasAssetsEnabled
            getAsset={() => ({
              fingerprint: 'asset1token',
              metadata: null,
              decimals: 0,
              recommendedDecimals: null,
              uniqueId: 'policytoken',
            })}
            isInternalAddress={(address: string) => address.includes('self')}
            analyticsTracker={noopAnalyticsTracker}
            onCopyAssetParam={jest.fn()}
          />
        </DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </IntlProvider>
  </StoryDecorator>
);

const toggleFor = (id: string): HTMLElement => {
  const row = document.getElementById(`tx-${id}`);
  if (!row) throw new Error(`Missing transaction row ${id}`);
  return within(row).getByRole('button', { name: /ADA sent/ });
};

describe('WalletTransactionsList feedback', () => {
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });
  afterEach(cleanup);

  it('reveals the selected transaction and preserves other manual expansion across refreshes', async () => {
    const first = transaction('first');
    const selected = transaction('selected');
    const view = render(list([first, selected], 'selected'));

    await waitFor(() =>
      expect(toggleFor('selected')).toHaveAttribute('aria-expanded', 'true')
    );
    fireEvent.click(toggleFor('first'));
    expect(toggleFor('first')).toHaveAttribute('aria-expanded', 'true');

    view.rerender(
      list([transaction('first'), transaction('selected')], 'selected')
    );

    expect(toggleFor('selected')).toHaveAttribute('aria-expanded', 'true');
    expect(toggleFor('first')).toHaveAttribute('aria-expanded', 'true');
  });

  it('separates payment principal from net change and keeps unknown amounts unavailable', async () => {
    const ada = transaction('ada', {
      transferAmount: new BigNumber(12),
      isSelfTransfer: true,
      addresses: {
        from: ['addr_test1selfsender'],
        to: ['addr_test1selfreceiver'],
        withdrawals: [],
      },
    });
    const unknown = transaction('unknown', {
      state: TransactionStates.SUBMISSION_UNKNOWN,
      amount: new BigNumber(0),
      amountIsKnown: false,
    });
    render(list([ada, unknown]));

    await waitFor(() => expect(document.getElementById('tx-ada')).toBeTruthy());
    const adaRow = within(document.getElementById('tx-ada')!);
    const transferredLabel = adaRow.getByText(
      translations['wallet.transaction.amountTransferred']
    );
    expect(transferredLabel).toBeVisible();
    expect(transferredLabel.parentElement).toHaveTextContent('12');
    expect(transferredLabel.parentElement).not.toHaveTextContent('0.17');
    expect(
      within(document.getElementById('tx-unknown')!).getByText(
        translations['wallet.transaction.amountUnavailable']
      )
    ).toBeVisible();
  });

  it('ignores receipt selection and transfer overlays outside the pilot while retaining expired removal', async () => {
    global.cip30Enabled = false;
    const expired = transaction('expired', {
      state: TransactionStates.EXPIRED,
      transferAmount: new BigNumber(99),
      isSelfTransfer: true,
      amountIsKnown: false,
    });
    const remove = jest.fn().mockResolvedValue(undefined);
    render(list([expired], 'expired', remove));
    const toggle = await waitFor(() => toggleFor('expired'));
    expect(toggle).toHaveAttribute('aria-expanded', 'false');
    const row = within(document.getElementById('tx-expired')!);
    expect(
      row.queryByText(translations['wallet.transaction.amountTransferred'])
    ).not.toBeInTheDocument();
    expect(
      row.queryByText(translations['wallet.transaction.amountUnavailable'])
    ).not.toBeInTheDocument();
    fireEvent.click(toggle);
    fireEvent.click(
      row.getByRole('button', { name: /Remove failed transaction/ })
    );
    await waitFor(() =>
      expect(remove).toHaveBeenCalledWith({
        walletId: 'wallet-1',
        transactionId: 'expired',
      })
    );
  });

  it('requires confirmation to cancel upstream pending history and leaves confirmed history uncancellable', async () => {
    global.cip30Enabled = false;
    const cancel = jest.fn().mockResolvedValue(undefined);
    const view = render(
      list(
        [
          transaction('pending'),
          transaction('confirmed', { state: TransactionStates.OK }),
        ],
        undefined,
        cancel
      )
    );
    await waitFor(() => toggleFor('pending'));
    fireEvent.click(toggleFor('confirmed'));
    expect(
      within(document.getElementById('tx-confirmed')!).queryByRole('button', {
        name: /Cancel pending transaction|Remove failed transaction/,
      })
    ).not.toBeInTheDocument();
    fireEvent.click(toggleFor('pending'));
    const row = within(document.getElementById('tx-pending')!);
    fireEvent.click(
      row.getByRole('button', { name: /Cancel pending transaction/ })
    );
    expect(cancel).not.toHaveBeenCalled();
    fireEvent.click(
      view.getByRole('button', {
        name: translations[
          'cancel.transaction.confirmation.dialog.button.backLabel'
        ],
      })
    );
    expect(cancel).not.toHaveBeenCalled();
    fireEvent.click(
      row.getByRole('button', { name: /Cancel pending transaction/ })
    );
    fireEvent.click(
      view.getByRole('button', {
        name: translations[
          'cancel.transaction.confirmation.dialog.button.confirmLabel'
        ],
      })
    );
    await waitFor(() =>
      expect(cancel).toHaveBeenCalledWith({
        walletId: 'wallet-1',
        transactionId: 'pending',
      })
    );
  });

  it('does not offer backend cancellation for rejected or unknown pilot receipts', async () => {
    render(
      list([
        transaction('rejected', { state: TransactionStates.FAILED }),
        transaction('unknown', { state: TransactionStates.SUBMISSION_UNKNOWN }),
      ])
    );
    await waitFor(() => toggleFor('rejected'));
    for (const id of ['rejected', 'unknown']) {
      fireEvent.click(toggleFor(id));
      expect(
        within(document.getElementById(`tx-${id}`)!).queryByRole('button', {
          name: /Cancel pending transaction|Remove expired transaction/,
        })
      ).not.toBeInTheDocument();
    }
  });
});
