// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';

// Assets and helpers
import { generateWallet } from '../../_support/utils';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import WalletsWrapper from '../_utils/WalletsWrapper';
import WalletsTransactionsWrapper from '../_utils/WalletsTransactionsWrapper';
import {
  DATE_ENGLISH_OPTIONS,
  // LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';
import { WalletTransaction } from '../../../../source/renderer/app/domains/WalletTransaction';
import type { TransactionFilterOptionsType } from '../../../../source/renderer/app/stores/TransactionsStore';

// Screens
import WalletTransactions from '../../../../source/renderer/app/components/wallet/transactions/WalletTransactions';
import { WALLET_NATIVE_TOKENS_ENABLED } from '../../../../source/renderer/app/config/walletsConfig';

type Props = {
  defaultFilterOptions: TransactionFilterOptionsType,
  filterOptions: TransactionFilterOptionsType,
  locale: string,
  onFilter: Function,
  populatedFilterOptions: TransactionFilterOptionsType,
  transactions: Array<WalletTransaction>,
  totalAvailable: number,
};

/* eslint-disable consistent-return */
storiesOf('Wallets|Transactions', module)
  .addDecorator(withKnobs)
  .addDecorator((getStory, props) => {
    const transactionsOption = select(
      'Transactions',
      {
        'Grouped by days': 'groupedByDays',
        'Confirmed and pending transactions': 'confirmedAndPendingTransactions',
        'Rendering many transactions': 'renderingManyTransactions',
        'Unresolved income addresses': 'unresolvedIncomeAddresses',
        'Without income addresses': 'withoutIncomeAddresses',
        'With withdrawal addresses': 'withWithdrawalAddresses',
      },
      'groupedByDays'
    );
    return (
      <WalletsTransactionsWrapper
        {...props}
        transactionsOption={transactionsOption}
        getStory={getStory}
      />
    );
  })
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Transactions List', (props: Props) => {
    const {
      defaultFilterOptions,
      filterOptions,
      locale,
      onFilter,
      populatedFilterOptions,
      transactions,
      totalAvailable,
    } = props;
    return (
      <WalletTransactions
        activeWallet={generateWallet('Wallet name', '45119903750165')}
        currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentLocale={locale}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
        defaultFilterOptions={defaultFilterOptions}
        filterOptions={filterOptions}
        deletePendingTransaction={action('deletePendingTransaction')}
        formattedWalletAmount={formattedWalletAmount}
        getUrlByType={action('getUrlByType')}
        hasMoreToLoad={false}
        isDeletingTransaction={false}
        isLoadingTransactions={false}
        onFilter={onFilter}
        onLoadMore={action('onLoadMore')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onRequestCSVFile={action('onRequestCSVFile')}
        populatedFilterOptions={populatedFilterOptions}
        totalAvailable={totalAvailable}
        transactions={transactions}
      />
    );
  })
  .add('Tokens Transactions List', (props: Props) => {
    const {
      defaultFilterOptions,
      filterOptions,
      locale,
      onFilter,
      populatedFilterOptions,
      transactions,
      totalAvailable,
    } = props;
    const hasNativeTokens = WALLET_NATIVE_TOKENS_ENABLED;
    return (
      <WalletTransactions
        activeWallet={generateWallet('Wallet name', '45119903750165')}
        currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentLocale={locale}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
        defaultFilterOptions={defaultFilterOptions}
        filterOptions={filterOptions}
        deletePendingTransaction={action('deletePendingTransaction')}
        formattedWalletAmount={formattedWalletAmount}
        getUrlByType={action('getUrlByType')}
        hasMoreToLoad={false}
        isDeletingTransaction={false}
        isLoadingTransactions={false}
        onFilter={onFilter}
        onLoadMore={action('onLoadMore')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onRequestCSVFile={action('onRequestCSVFile')}
        populatedFilterOptions={populatedFilterOptions}
        totalAvailable={totalAvailable}
        transactions={transactions}
        hasNativeTokens={hasNativeTokens}
      />
    );
  });
