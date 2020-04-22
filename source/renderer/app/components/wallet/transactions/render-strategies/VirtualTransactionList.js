// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import { throttle, debounce } from 'lodash';
import { WalletTransaction } from '../../../../domains/WalletTransaction';
import type { ScrollContextType } from '../WalletTransactionsList';
import { WalletTransactionsListScrollContext } from '../WalletTransactionsList';
import type { Row } from '../types';
import { TransactionInfo, TransactionsGroup } from '../types';
import styles from './VirtualTransactionList.scss';

/* eslint-disable react/no-unused-prop-types */

type Props = {
  getExpandedTransactions: () => Array<any>,
  renderRow: Row => Node,
  rows: Row[],
  isLoadingSpinnerShown?: boolean,
  isSyncingSpinnerShown?: boolean,
};

type RowHeight = number;

const GROUP_DATE_HEIGHT = 26;
const TX_CONTRACTED_ROW_HEIGHT = 86;
const TX_EXPANDED_ROW_BASE_HEIGHT = 260 + 16;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_BOTTOM_BORDER_MARGIN = 1;
const TX_ADDRESS_SELECTOR = '.Transaction_address';
const TX_ID_SELECTOR = '.Transaction_transactionId';

@observer
export class VirtualTransactionList extends Component<Props> {
  static defaultProps = {
    isLoadingSpinnerShown: false,
    isSyncingSpinnerShown: false,
  };

  UNSAFE_componentWillReceiveProps(nextProps: Props) {
    // Recompute all row heights in case the number of rows has changed
    const prevNumberOfRows = this.props.rows.length;
    const nextNumberOfRows = nextProps.rows.length;
    if (prevNumberOfRows && prevNumberOfRows !== nextNumberOfRows) {
      this.rowHeights = this.estimateRowHeights(nextProps.rows);
      this.recomputeVirtualRowHeights();
    }
  }

  list: List;
  rowHeights: RowHeight[] = [];
  txAddressHeight: number = 0;
  txIdHeight: number = 0;
  visibleExpandedTx: Array<WalletTransaction>;

  /**
   * Returns the row index of a given tx.
   */
  findIndexForTx = (tx: WalletTransaction): number =>
    this.props.rows.findIndex(
      r => r instanceof TransactionInfo && r.tx.id === tx.id
    );

  /**
   * Recomputes virtual row heights only once per tick (debounced)
   */
  recomputeVirtualRowHeights = debounce((startIndex: number = 0): void => {
    const { list } = this;
    if (!list) return;
    list.recomputeRowHeights(startIndex);
  });

  /**
   * Calculates the number of lines of the addresses and id from the first expanded tx
   */
  updateAddressesAndIdHeights = (): void => {
    const firstTxAddress = document.querySelector(TX_ADDRESS_SELECTOR);
    const firstTxId = document.querySelector(TX_ID_SELECTOR);
    if (
      firstTxAddress instanceof HTMLElement &&
      firstTxId instanceof HTMLElement
    ) {
      this.txAddressHeight = firstTxAddress.offsetHeight;
      this.txIdHeight = firstTxId.offsetHeight;
    }
  };

  /**
   * Gets an Info expanded row height
   */
  estimateHeightOfTxExpandedRow = (row: Row, tx: WalletTransaction): number => {
    if (!this.txAddressHeight) this.updateAddressesAndIdHeights();
    const txSingleAddressHeight = this.txAddressHeight;
    const txIdHeightValue = this.txIdHeight;
    const { addresses } = tx;
    const txAddressesCount = addresses.from.length + addresses.to.length;
    const txAddressesHeight = txAddressesCount * txSingleAddressHeight;
    const txBottomMargin = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 1;
    return (
      TX_EXPANDED_ROW_BASE_HEIGHT +
      txAddressesHeight +
      txIdHeightValue +
      txBottomMargin
    );
  };

  /**
   * Gets an Info contracted row height
   */
  estimateHeightOfTxContractedRow = (row: Row): number => {
    const txBottomMargin = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;
    return TX_CONTRACTED_ROW_HEIGHT + txBottomMargin;
  };

  /**
   * Updates and recomputes row height
   */
  updateTxRowHeight = (
    tx: WalletTransaction,
    isExpanded: boolean,
    wasToggled: boolean // Is "true" when transaction is manually expanded/collapsed
  ): void => {
    const { rowHeights } = this;
    const txIndex = this.findIndexForTx(tx);
    const row = this.props.rows[txIndex];
    if (row instanceof TransactionsGroup) return;
    rowHeights[txIndex] = isExpanded
      ? this.estimateHeightOfTxExpandedRow(row, tx)
      : this.estimateHeightOfTxContractedRow(row);
    this.recomputeVirtualRowHeights();

    // In case transaction has just been manually expanded we need to schedule
    // another row height calculation if the transaction still isn't fully
    // expanded in the moment of the initial execution of this method
    if (isExpanded && wasToggled) {
      const isFullyExpanded = this.checkIfTxContentIsFullyExpanded(tx);
      if (isFullyExpanded) {
        const estimatedHeight = rowHeights[txIndex];
        this.correctExpandedTxHeightEstimationErrors(tx, estimatedHeight);
      } else {
        setTimeout(this.updateTxRowHeight, 1, tx, true, true);
      }
    }
  };

  /**
   * Gets a row height based on its type
   */
  estimateRowHeight = (row: Row): number =>
    row instanceof TransactionInfo
      ? this.estimateHeightOfTxContractedRow(row)
      : GROUP_DATE_HEIGHT;

  /**
   * Maps over all rows and returns array of calculated heights.
   */
  estimateRowHeights = (rows: Row[]): RowHeight[] =>
    rows.map(this.estimateRowHeight);

  /**
   * Returns the DOM element for given transaction id
   */
  getTxRowElementById = (id: string) => document.getElementById(`tx-${id}`);

  /**
   * Measures the exact height of a rendered tx content DOM element.
   */
  measureTxContentHeight = (tx: WalletTransaction): ?number => {
    const txRow = this.getTxRowElementById(tx.id);
    if (txRow) {
      const txElement = txRow.firstChild;
      const style = window.getComputedStyle(txElement, null);
      return parseInt(style.getPropertyValue('height'), 10);
    }
    return null;
  };

  /**
   * Checks if rendered tx content DOM element has been fully expanded.
   */
  checkIfTxContentIsFullyExpanded = (tx: WalletTransaction): boolean => {
    const txRow = this.getTxRowElementById(tx.id);
    const txElement = txRow && txRow.firstChild;
    return (
      txElement instanceof HTMLElement &&
      txElement.classList.contains('Transaction_expanded')
    );
  };

  /**
   * Corrects potential estimation errors that can happen due to various reasons.
   */
  correctExpandedTxHeightEstimationErrors = (
    tx: WalletTransaction,
    estimatedHeight: number
  ) => {
    const txContentHeight = this.measureTxContentHeight(tx);
    if (!txContentHeight) return;
    const { rows } = this.props;
    const txIndex = this.findIndexForTx(tx);
    const txInfo = rows[txIndex];
    if (txInfo instanceof TransactionInfo) {
      const margin = txInfo.isLastInGroup
        ? TX_LAST_IN_GROUP_MARGIN
        : TX_BOTTOM_BORDER_MARGIN;
      const requiredHeight = txContentHeight + margin;
      const estimationError = Math.abs(estimatedHeight - requiredHeight);
      if (estimationError > 1) {
        this.rowHeights[txIndex] = requiredHeight;
        this.recomputeVirtualRowHeights();
      }
    }
  };

  updateVisibleExpandedTxRowHeights = () => {
    this.visibleExpandedTx.forEach(tx => {
      this.updateTxRowHeight(tx, true, false);
      const estimatedHeight = this.rowHeights[this.findIndexForTx(tx)];
      this.correctExpandedTxHeightEstimationErrors(tx, estimatedHeight);
    });
  };

  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   */
  onResize = (): void => {
    // First load, calculates all the rows heights
    if (!this.rowHeights.length) {
      this.rowHeights = this.estimateRowHeights(this.props.rows);
      return;
    }

    // Subsequently resizes, updates the expanded rows heights if there is any expanded one
    const expandedTransactions = this.props.getExpandedTransactions();
    if (!expandedTransactions.length) return;
    this.updateAddressesAndIdHeights();
    this.updateVisibleExpandedTxRowHeights();
  };

  /**
   * Callback that gets invoked when virtual rows are rendered.
   * Used to update the array of visible expanded transactions and remeasure their height.
   */
  onRowsRendered = ({
    overscanStartIndex,
    overscanStopIndex,
  }: {
    overscanStartIndex: number,
    overscanStopIndex: number,
  }) => {
    const expandedRows = this.props.getExpandedTransactions();
    this.visibleExpandedTx = expandedRows.filter(tx => {
      const index = this.findIndexForTx(tx);
      return index >= overscanStartIndex && index <= overscanStopIndex;
    });
    this.updateVisibleExpandedTxRowHeights();
  };

  rowRenderer = ({
    key, // Unique key within array of rows
    index, // Index of row within collection
    style, // Style object to be applied to row (to position it)
  }: {
    key: string,
    index: number,
    style: string,
  }) => (
    <div key={key} style={style} className={styles.row}>
      {this.props.renderRow(this.props.rows[index])}
    </div>
  );

  onListScroll = (
    context: ScrollContextType,
    { scrollTop }: { scrollTop: number }
  ) => {
    context.setFilterButtonFaded(scrollTop > 10);
  };

  // =============== REACT LIFECYCLE ================= //

  render() {
    const { rows, isLoadingSpinnerShown, isSyncingSpinnerShown } = this.props;

    // Prevent List rendering if we have no rows to render
    if (!rows.length) return false;

    const componentStyles = classNames([
      styles.component,
      isLoadingSpinnerShown ? styles.withLoadingSpinner : null,
      isSyncingSpinnerShown ? styles.withSyncingSpinner : null,
    ]);

    return (
      <WalletTransactionsListScrollContext.Consumer>
        {context => (
          <div className={componentStyles}>
            <AutoSizer
              onResize={throttle(this.onResize, 100, {
                leading: true,
                trailing: true,
              })}
            >
              {({ width, height }) => (
                <List
                  className={styles.list}
                  ref={list => {
                    this.list = list;
                  }}
                  width={width}
                  height={height}
                  onRowsRendered={throttle(this.onRowsRendered, 100, {
                    leading: true,
                    trailing: true,
                  })}
                  rowCount={rows.length}
                  rowHeight={({ index }) =>
                    this.rowHeights[index] || TX_CONTRACTED_ROW_HEIGHT
                  }
                  rowRenderer={this.rowRenderer}
                  style={{ overflowY: 'scroll' }}
                  onScroll={param => this.onListScroll(context, param)}
                />
              )}
            </AutoSizer>
          </div>
        )}
      </WalletTransactionsListScrollContext.Consumer>
    );
  }
}
