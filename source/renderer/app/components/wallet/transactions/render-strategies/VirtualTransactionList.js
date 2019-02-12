// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import { WalletTransaction } from '../../../../domains/WalletTransaction';
import type { Row } from '../types';
import styles from './VirtualTransactionList.scss';
import { TransactionInfo, TransactionsGroup } from '../types';

const { isWindows, isLinux } = global.environment;

type Props = {
  isTxExpanded: (WalletTransaction) => boolean,
  getExpandedTransactions: () => WalletTransaction[],
  renderRow: (Row) => Node,
  rows: Row[],
  isLoadingSpinnerShown?: boolean,
  isSyncingSpinnerShown?: boolean,
};

type RowHeight = number;

const GROUP_DATE_HEIGHT = 26;
const TX_CONTRACTED_ROW_HEIGHT = 86;
const TX_EXPANDED_ROW_BASE_HEIGHT = 285;
const TX_LINE_HEIGHT_FOR_ID = 16;
const TX_LINE_HEIGHT_FOR_ADDRESS = 19;
const TX_LAST_IN_GROUP_MARGIN = 20;

const TX_ADDRESSES_START_BREAKING_AT_WIDTH_MACOS = 985;
const TX_ADDRESSES_START_BREAKING_AT_WIDTH_WINDOWS = 1067;
const TX_ADDRESSES_START_BREAKING_AT_WIDTH_LINUX = 1107;
const TX_ID_START_BREAKING_AT_WIDTH_MACOS = 655;
const TX_ID_START_BREAKING_AT_WIDTH_WINDOWS = 655;
const TX_ID_START_BREAKING_AT_WIDTH_LINUX = 677;

let txAddressesStartBreakingAtWidth = TX_ADDRESSES_START_BREAKING_AT_WIDTH_MACOS;
let txIdStartBreakingAtWidth = TX_ID_START_BREAKING_AT_WIDTH_MACOS;
if (isWindows) {
  txAddressesStartBreakingAtWidth = TX_ADDRESSES_START_BREAKING_AT_WIDTH_WINDOWS;
  txIdStartBreakingAtWidth = TX_ID_START_BREAKING_AT_WIDTH_WINDOWS;
}
if (isLinux) {
  txAddressesStartBreakingAtWidth = TX_ADDRESSES_START_BREAKING_AT_WIDTH_LINUX;
  txIdStartBreakingAtWidth = TX_ID_START_BREAKING_AT_WIDTH_LINUX;
}

@observer
export class VirtualTransactionList extends Component<Props> {

  static defaultProps = {
    isLoadingSpinnerShown: false,
    isSyncingSpinnerShown: false,
  };

  list: List;
  rowHeights: RowHeight[] = [];
  txAddressesLines: number = 0;
  txIdLines: number = 0;

  /**
   * Returns the row index of a given tx.
   * @param tx
   * @returns {number}
   */
  findIndexForTx = (tx: WalletTransaction): number => (
    this.props.rows.findIndex(r => (r instanceof TransactionInfo) && r.tx.id === tx.id)
  );

  /**
   * Gets an Info row height and updates the list value
   * @param row
   * @returns {number[]}
   */
  updateInfoRowHeight = (tx: WalletTransaction) => {
    const { list, rowHeights } = this;
    if (!list) return;
    const txIndex = this.findIndexForTx(tx);
    const row = this.props.rows[txIndex];
    if (row instanceof TransactionsGroup) return;
    const txRowHeight = this.calculateInfoRowHeight(row);
    rowHeights[txIndex] = txRowHeight;
    list.recomputeRowHeights(txIndex);
  };

  /**
   * Gets an Info expanded row height
   * @param tx
   * @returns {number[]}
   */
  calculateHeightOfTxExpandedRow = (tx: WalletTransaction) => {
    const txSingleAddressHeight = TX_LINE_HEIGHT_FOR_ADDRESS * this.txAddressesLines;
    const txIdHeight = TX_LINE_HEIGHT_FOR_ID * this.txIdLines;
    const { addresses } = tx;
    const txAddresses = addresses.from.length + addresses.to.length;
    const txAddressesHeight = (txAddresses * txSingleAddressHeight);
    return txAddressesHeight + txIdHeight + TX_EXPANDED_ROW_BASE_HEIGHT;
  };

  /**
   * Gets an Info contracted row height
   * @param tx
   * @returns {number[]}
   */
  calculateHeightOfTxContractedRow = (row: Row) => {
    const headerSpacing = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;
    return TX_CONTRACTED_ROW_HEIGHT + headerSpacing;
  };

  /**
   * Gets an Info row height
   * @param tx
   * @returns {number[]}
   */
  calculateInfoRowHeight = (row: TransactionInfo) => {
    const isExpanded = this.props.isTxExpanded(row.tx);
    return isExpanded
      ? this.calculateHeightOfTxExpandedRow(row.tx)
      : this.calculateHeightOfTxContractedRow(row);
  };

  /**
   * Gets a row height based on its type
   * @param rows
   * @returns {number[]}
   */
  calculateRowHeight = (row: Row) => (
    (row instanceof TransactionInfo)
      ? this.calculateHeightOfTxContractedRow(row)
      : GROUP_DATE_HEIGHT
  );

  /**
   * Maps over all rows and returns array of calculated heights.
   * @param rows
   * @returns {number[]}
   */
  calculateRowHeights = (rows: Row[]): RowHeight[] => rows.map(this.calculateRowHeight);

  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   * @param width
   */
  onResize = ({ width }: { width: number }) => {

    // First load, calculates all the rows heights
    if (!this.rowHeights.length) {
      this.updateAddressesAndIdLines(width);
      this.rowHeights = this.calculateRowHeights(this.props.rows);
      return false;
    }
    // Subsequently resizes, updates the expanded rows heights
    const { txAddressesLines, txIdLines } = this;
    this.updateAddressesAndIdLines(width);
    if (txAddressesLines !== this.txAddressesLines || txIdLines !== this.txIdLines) {
      this.props.getExpandedTransactions().map(this.updateInfoRowHeight);
    }
  };

  updateAddressesAndIdLines = (width: number) => {
    this.txAddressesLines = (width >= txAddressesStartBreakingAtWidth) ? 1 : 2;
    this.txIdLines = (width >= txIdStartBreakingAtWidth) ? 1 : 2;
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
        <AutoSizer onResize={this.onResize}>
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
