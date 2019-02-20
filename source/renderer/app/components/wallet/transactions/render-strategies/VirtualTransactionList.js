// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import { throttle, debounce } from 'lodash';
import { WalletTransaction } from '../../../../domains/WalletTransaction';
import type { Row } from '../types';
import styles from './VirtualTransactionList.scss';
import { TransactionInfo, TransactionsGroup } from '../types';

type Props = {
  getExpandedTransactions: () => Array<any>,
  renderRow: (Row) => Node,
  rows: Row[],
  isLoadingSpinnerShown?: boolean,
  isSyncingSpinnerShown?: boolean,
};

type RowHeight = number;

const GROUP_DATE_HEIGHT = 26;
const TX_CONTRACTED_ROW_HEIGHT = 86;
const TX_EXPANDED_ROW_BASE_HEIGHT = 260 + 16;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_ADDRESS_SELECTOR = '.Transaction_address';
const TX_ID_SELECTOR = '.Transaction_transactionId';

@observer
export class VirtualTransactionList extends Component<Props> {

  static defaultProps = {
    isLoadingSpinnerShown: false,
    isSyncingSpinnerShown: false,
  };

  list: List;
  rowHeights: RowHeight[] = [];
  txAddressHeight: number = 0;
  txIdHeight: number = 0;

  /**
   * Returns the row index of a given tx.
   */
  findIndexForTx = (tx: WalletTransaction): number => (
    this.props.rows.findIndex(r => (r instanceof TransactionInfo) && r.tx.id === tx.id)
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
   * Updates and recomputes row height
   */
  updateInfoRowHeight = (tx: WalletTransaction, isExpanded: boolean): void => {
    const { rowHeights } = this;
    const txIndex = this.findIndexForTx(tx);
    const row = this.props.rows[txIndex];
    if (row instanceof TransactionsGroup) return;
    rowHeights[txIndex] = isExpanded
      ? this.calculateHeightOfTxExpandedRow(row, tx)
      : this.calculateHeightOfTxContractedRow(row);
    this.recomputeVirtualRowHeights(0);
  };

  /**
   * Gets an Info expanded row height
   */
  calculateHeightOfTxExpandedRow = (row: Row, tx: WalletTransaction): number => {
    if (!this.txAddressHeight) this.updateAddressesAndIdHeights();
    const txSingleAddressHeight = this.txAddressHeight;
    const txIdHeight = this.txIdHeight;
    const { addresses } = tx;
    const txAddressesCount = addresses.from.length + addresses.to.length;
    const txAddressesHeight = txAddressesCount * txSingleAddressHeight;
    const txBottomMargin = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 1;
    return TX_EXPANDED_ROW_BASE_HEIGHT + txAddressesHeight + txIdHeight + txBottomMargin;
  };

  /**
   * Gets an Info contracted row height
   */
  calculateHeightOfTxContractedRow = (row: Row): number => {
    const txBottomMargin = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;
    return TX_CONTRACTED_ROW_HEIGHT + txBottomMargin;
  };

  /**
   * Gets a row height based on its type
   */
  calculateRowHeight = (row: Row): number => (
    (row instanceof TransactionInfo)
      ? this.calculateHeightOfTxContractedRow(row)
      : GROUP_DATE_HEIGHT
  );

  /**
   * Maps over all rows and returns array of calculated heights.
   */
  calculateRowHeights = (rows: Row[]): RowHeight[] => rows.map(this.calculateRowHeight);

  /**
   * Calculates the number of lines of the addresses and id from the first expanded tx
   */
  updateAddressesAndIdHeights = (): void => {
    const firstTxAddress = document.querySelector(TX_ADDRESS_SELECTOR);
    const firstTxId = document.querySelector(TX_ID_SELECTOR);
    if (firstTxAddress instanceof HTMLElement && firstTxId instanceof HTMLElement) {
      this.txAddressHeight = firstTxAddress.offsetHeight;
      this.txIdHeight = firstTxId.offsetHeight;
    }
  };

  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   */
  onResize = (): void => {
    // First load, calculates all the rows heights
    if (!this.rowHeights.length) {
      this.rowHeights = this.calculateRowHeights(this.props.rows);
      return;
    }

    // Subsequently resizes, updates the expanded rows heights if there is any expanded one
    const expandedTransactions = this.props.getExpandedTransactions();
    const { txAddressHeight, txIdHeight } = this;
    if (!expandedTransactions.length) return;
    this.updateAddressesAndIdHeights();
    if (txAddressHeight !== this.txAddressHeight || txIdHeight !== this.txIdHeight) {
      expandedTransactions.map(tx => this.updateInfoRowHeight(tx, true));
    }
  };

  rowRenderer = ({
    key, // Unique key within array of rows
    index, // Index of row within collection
    style // Style object to be applied to row (to position it)
  }: { key: string, index: number, style: string }) => (
    <div key={key} style={style} className={styles.row}>
      {this.props.renderRow(this.props.rows[index])}
    </div>
  );

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
      <div className={componentStyles}>
        <AutoSizer onResize={throttle(this.onResize, 50)}>
          {({ width, height }) => (
            <List
              className={styles.list}
              ref={(list) => this.list = list}
              width={width}
              height={height}
              rowCount={rows.length}
              rowHeight={({ index }) => this.rowHeights[index] || TX_CONTRACTED_ROW_HEIGHT}
              rowRenderer={this.rowRenderer}
              style={{ overflowY: 'scroll' }}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
