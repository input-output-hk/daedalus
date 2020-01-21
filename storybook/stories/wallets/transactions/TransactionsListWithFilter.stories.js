// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import moment from 'moment';

// Assets and helpers
import { generateTransaction } from '../../_support/utils';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import { TransactionTypes } from '../../../../source/renderer/app/domains/WalletTransaction';
import WalletsWrapper from '../_utils/WalletsWrapper';
import {
  DATE_ENGLISH_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';
import { DateRangeTypes } from '../../../../source/renderer/app/stores/TransactionsStore';

// Screens
import WalletTransactionsList from '../../../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import FilterButton from '../../../../source/renderer/app/components/wallet/transactions/FilterButton';
import FilterDialog from '../../../../source/renderer/app/components/wallet/transactions/FilterDialog';

/* eslint-disable consistent-return */
storiesOf('Wallets|Transactions', module)
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add(
    'Transactions - With filter dialog open',
    ({ locale }: { locale: string }) => (
      <div>
        <FilterButton
          numberOfFilterDimensionsApplied={1}
          faded={false}
          onClick={() => null}
        />
        <FilterDialog
          dateFormat="MM/DD/YYYY"
          defaultFilterOptions={{}}
          populatedFilterOptions={{ dateRange: DateRangeTypes.ALL }}
          onFilter={() => null}
          onClose={() => null}
        />
        <WalletTransactionsList
          onOpenExternalLink={action('onOpenExternalLink')}
          getUrlByType={action('getUrlByType')}
          currentLocale={locale}
          transactions={[
            generateTransaction(
              TransactionTypes.INCOME,
              new Date(),
              new BigNumber(1)
            ),
            generateTransaction(
              TransactionTypes.INCOME,
              moment()
                .subtract(1, 'days')
                .toDate(),
              new BigNumber(1)
            ),
            generateTransaction(
              TransactionTypes.INCOME,
              new Date(),
              new BigNumber(1)
            ),
            generateTransaction(
              TransactionTypes.INCOME,
              moment()
                .subtract(2, 'days')
                .toDate(),
              new BigNumber(1)
            ),
            generateTransaction(
              TransactionTypes.INCOME,
              moment()
                .subtract(1, 'days')
                .toDate(),
              new BigNumber(1)
            ),
          ]}
          deletePendingTransaction={() => {}}
          isRestoreActive={false}
          isLoadingTransactions={false}
          hasMoreToLoad={false}
          walletId="test-wallet"
          isDeletingTransaction={false}
          formattedWalletAmount={formattedWalletAmount}
          totalAvailable={5}
          currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
          currentTimeFormat={TIME_OPTIONS[0].value}
        />
      </div>
    )
  );
