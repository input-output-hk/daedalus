// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import FilterDialog from './FilterDialog';
import FilterButton from './FilterButton';
import FilterResultInfo from './FilterResultInfo';
import styles from './WalletTransactionsHeader.scss';
import TinyButton from '../../widgets/forms/TinyButton';
import downloadIcon from '../../../assets/images/download-icon.inline.svg';
import type { TransactionFilterOptionsType } from '../../../stores/TransactionsStore';

export const messages = defineMessages({
  transactions: {
    id: 'wallet.transactions.header.transactions',
    defaultMessage: '!!!Transactions',
    description: 'Label for the "Transactions" header.',
  },
  exportCSVButtonLabel: {
    id: 'wallet.transactions.header.exportCSV.button.label',
    defaultMessage: '!!!Export CSV',
    description: 'Label for the "Export CSV" button.',
  },
});

const fileContent = [
  ['id', 'amount'],
  ['1', '1.000000'],
  ['2', '2.000000'],
  ['3', '3.000000'],
  ['4', '4.000000'],
];

type Props = {
  transactions: Array<WalletTransaction>,
  filterOptions: TransactionFilterOptionsType,
  currentLocale: string,
  currentDateFormat: string,
  currentNumberFormat: string,
  defaultFilterOptions: TransactionFilterOptionsType,
  populatedFilterOptions: TransactionFilterOptionsType,
  onFilter: Function,
  onClose: Function,
  onRequestCSVFile: Function,
  onFilterButtonClick: Function,
  isScrolling: boolean,
  isFilterButtonFaded: boolean,
  isFilterDialogOpen: boolean,
};

@observer
export default class WalletTransactionsHeader extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      transactions,
      filterOptions,
      isScrolling,
      onRequestCSVFile,
      onFilterButtonClick,
      currentLocale,
      currentDateFormat,
      currentNumberFormat,
      isFilterButtonFaded,
      isFilterDialogOpen,
      /* hasAny, */
    } = this.props;
    // console.log('this.props.hasAny', this.props.hasAny);
    const hasAny = true;

    const componentClassnames = classnames([
      styles.component,
      isScrolling ? styles.isScrolling : null,
    ]);

    return (
      <div className={componentClassnames}>
        <div className={styles.numberOfTransactions}>
          {intl.formatMessage(messages.transactions)} ({transactions.length})
        </div>
        {hasAny && (
          <div className={styles.actions}>
            <TinyButton
              label={
                <Fragment>
                  {intl.formatMessage(messages.exportCSVButtonLabel)}
                  <SVGInline
                    svg={downloadIcon}
                    className={styles.downloadIcon}
                  />
                </Fragment>
              }
              onClick={() => onRequestCSVFile(fileContent)}
              containerClassName={styles.csvButtonContainer}
              className={styles.csvButton}
              loading={false}
            />
            <FilterButton
              numberOfFilterDimensionsApplied={3}
              faded={isFilterButtonFaded}
              onClick={onFilterButtonClick}
            />
          </div>
        )}
        {isFilterDialogOpen && (
          <FilterDialog
            locale={currentLocale}
            dateFormat={currentDateFormat}
            numberFormat={currentNumberFormat}
            defaultFilterOptions={{}}
            populatedFilterOptions={{
              incomingChecked: true,
              outgoingChecked: false,
              dateRange: 'dateRange',
              fromDate: 'fromDate',
              toDate: 'toDate',
              fromAmount: 'fromAmount',
              toAmount: 'toAmount',
            }}
            filterOptions={{
              incomingChecked: true,
              outgoingChecked: false,
              dateRange: 'dateRange',
              fromDate: 'fromDate',
              toDate: 'toDate',
              fromAmount: 'fromAmount',
              toAmount: 'toAmount',
            }}
            onFilter={(a, b, c) => {
              console.log('a', a);
              console.log('b', b);
              console.log('c', c);
            }}
            onClose={(a, b, c) => {
              console.log('a', a);
              console.log('b', b);
              console.log('c', c);
            }}
          />
        )}
      </div>
    );

    // const {
    //   filterOptions,
    //   searchRequest,
    //   hasAny,
    //   totalAvailable,
    //   allFiltered,
    //   recentFiltered,
    //   deletePendingTransaction,
    //   deleteTransactionRequest,
    // } = stores.transactions;
    // const { currentTimeFormat, currentDateFormat, currentLocale } = profile;

    // // Guard against potential null values
    // if (!filterOptions || !activeWallet) return null;

    // let walletTransactions = null;
    // const { searchLimit } = filterOptions;
    // const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
    //   filterOptions
    // );
    // const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    // const hasMoreToLoad = () =>
    //   searchLimit !== null &&
    //   searchLimit !== undefined &&
    //   totalAvailable > searchLimit;

    // return (
    //   <div>
    //     <FilterDialogContainer />
    //     {hasAny && (
    //       <FilterButton
    //         numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
    //         faded={isFilterButtonFaded}
    //         onClick={this.openFilterDialog}
    //       />
    //     )}
    //   </div>
    // );
  }
}
