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

type Props = {
  ixTxExpanded: (WalletTransaction) => boolean,
  getExpandedTransactions: () => WalletTransaction[],
  renderRow: (Row) => Node,
  rows: Row[],
  isLoadingSpinnerShown?: boolean,
};

type RowHeight = number;

const GROUP_DATE_HEIGHT = 26;
const TX_ROW_HEIGHT = 86;
const TX_ROW_HEIGHT_EXPANDED = 294;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_BASE_HEIGHT_PER_ADDRESS = 19;
const TX_ADDRESSES_START_BREAKING_AT_WIDTH = 1000;
const TX_REMEASURE_THRESHOLD = 100;

@observer
export class VirtualTransactionList extends Component<Props> {

  static defaultProps = {
    isLoadingSpinnerShown: false,
  };

  list: List;
  rowHeights: RowHeight[] = [];
  areTxAddressesBreaking = false;

  /**
   * Best effort to pre-calculate the height that a row will need.
   * This should be used anytime the row is not yet rendered on screen
   * but the list needs to allocate space for it in advance.
   *
   * This will never be 100% correct for expanded transaction rows
   * due to the way address can break at random window sizes.
   *
   * @param rowIndex
   * @returns {number}
   */
  calculateRowHeight = (rowIndex: number) => {
    const { rows } = this.props;
    const row = rows[rowIndex];
    if (row instanceof TransactionsGroup) {
      return GROUP_DATE_HEIGHT;
    }
    if (row instanceof TransactionInfo) {
      const tx = row.tx;
      // Calculate the height of a transaction row:
      const { addresses } = tx;
      const isExpanded = this.props.ixTxExpanded(tx);
      // Add spacing for the next date header to the last transaction in a group
      const headerSpacing = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;

      if (isExpanded) {
        const totalAddresses = addresses.from.length + addresses.to.length;
        // Since address can break at random window sizes it's not possible
        // to be calculate this more fine grained than just calculating the
        // largest height possible for all addresses.
        const heightPerAddressFactor = this.areTxAddressesBreaking ? 2 : 1;
        const totalAddressesHeight = (
          heightPerAddressFactor * totalAddresses * TX_BASE_HEIGHT_PER_ADDRESS
        );
        // Extra height for addresses when expanded
        const heightForAddresses = isExpanded ? totalAddressesHeight : 0;

        return TX_ROW_HEIGHT_EXPANDED + heightForAddresses + headerSpacing;
      }

      // Not expanded:
      return TX_ROW_HEIGHT + headerSpacing;
    }
    return 0;
  };

  /**
   * Maps over all rows and returns array of calculated heights.
   * @param rows
   * @returns {number[]}
   */
  calculateRowHeights = (rows: Row[]): RowHeight[] => (
    rows.map((tx, index) => this.calculateRowHeight(index))
  );

  /**
   * Updates the row at index with calculated height.
   * @param index
   */
  recalcRowHeightForIndex = (index: number) => {
    this.rowHeights[index] = this.calculateRowHeight(index);
  };

  getTxRowElementById = (id: string) => (
    document.getElementById(`tx-${id}`)
  );

  /**
   * Measures the exact height of a rendered tx content DOM element.
   * @param tx
   * @returns {number | null}
   */
  measureTxContentHeight = (tx: WalletTransaction): ?number => {
    const txRow = this.getTxRowElementById(tx.id);
    if (txRow) {
      const txElement = txRow.firstChild;
      if (txElement instanceof HTMLElement) {
        return txElement.offsetHeight + 1;
      }
    }
    return null;
  };

  /**
   * Measures the exact height of a rendered tx row DOM element.
   * @param tx
   * @returns {*}
   */
  measureTxRowHeight = (tx: WalletTransaction): ?number => {
    const txRow = this.getTxRowElementById(tx.id);
    if (txRow) {
      return txRow.offsetHeight;
    }
    return null;
  };

  /**
   * Returns the row index of a given tx.
   * @param tx
   * @returns {number}
   */
  findIndexForTx = (tx: WalletTransaction): number => (
    this.props.rows.findIndex(r => (r instanceof TransactionInfo) && r.tx.id === tx.id)
  );

  /**
   * Ensures that the height of a transaction row is correct under all circumstances.
   * @param tx
   */
  updateHeightOfTxRow = (tx: WalletTransaction) => {
    const { list, rowHeights } = this;
    if (!list) return;
    const txIndex = this.findIndexForTx(tx);
    const estimatedHeight = this.calculateRowHeight(txIndex);
    const measuredTxContentHeight = this.measureTxContentHeight(tx);
    const measuredTxRowHeight = this.measureTxRowHeight(tx);

    if (!measuredTxContentHeight || !measuredTxRowHeight) return;

    const estimationDelta = Math.abs(estimatedHeight - measuredTxContentHeight);
    // Estimate is far off from measurement -> tx has been toggled (big height change)
    const hasTxBeenToggled = estimationDelta > TX_REMEASURE_THRESHOLD;

    if (hasTxBeenToggled) {
      rowHeights[txIndex] = estimatedHeight;
      list.recomputeRowHeights(txIndex);
      // Trigger another update for when the toggled state is rendered
      setTimeout(() => this.updateHeightOfTxRow(tx), 50);
    } else {
      // We are close enough to the estimate, do fine adjustment only
      const measureDelta = Math.abs(measuredTxRowHeight - measuredTxContentHeight);
      if (measureDelta > 0) {
        // The tx row height is either larger or smaller than necessary
        const { rows } = this.props;
        const txInfo = rows[txIndex];
        if (txInfo instanceof TransactionInfo) {
          // Update tx row with exact height
          const headerSpacing = txInfo.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;
          rowHeights[txIndex] = measuredTxContentHeight + headerSpacing;
          list.recomputeRowHeights(txIndex);
        }
      }
    }
  };

  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   * @param width
   */
  onResize = ({ width }: { width: number }) => {
    if (!this.areTxAddressesBreaking && width < TX_ADDRESSES_START_BREAKING_AT_WIDTH) {
      this.areTxAddressesBreaking = true;
    } else if (this.areTxAddressesBreaking && width > TX_ADDRESSES_START_BREAKING_AT_WIDTH) {
      this.areTxAddressesBreaking = false;
    }
    this.props.getExpandedTransactions().map(this.updateHeightOfTxRow);
  };

  render() {
    const { rows, isLoadingSpinnerShown } = this.props;

    // Prevent List rendering if we have no rows to render
    if (!rows.length) return false;

    this.rowHeights = this.calculateRowHeights(rows);

    // Renders a single row within the virtual list
    const rowRenderer = ({
      key, // Unique key within array of rows
      index, // Index of row within collection
      style // Style object to be applied to row (to position it)
    }: { key: string, index: number, style: string }) => (
      <div key={key} style={style} className={styles.row}>
        {this.props.renderRow(rows[index])}
      </div>
    );

    const componentStyles = classNames([
      styles.component,
      isLoadingSpinnerShown ? styles.withLoadingSpinner : null,
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
              rowHeight={({ index }) => this.rowHeights[index]}
              rowRenderer={rowRenderer}
              style={{ overflowY: 'scroll' }}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
