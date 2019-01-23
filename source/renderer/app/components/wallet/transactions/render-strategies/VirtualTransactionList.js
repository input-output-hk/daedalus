// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
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
};

type RowHeight = number;

const GROUP_DATE_HEIGHT = 26;
const TX_ROW_HEIGHT = 86;
const TX_ROW_HEIGHT_EXPANDED = 310;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_HEIGHT_PER_ADDRESS = 19;
const ADDRESS_BREAKPOINT = 800;

@observer
export class VirtualTransactionList extends Component<Props> {

  static defaultProps = {
    onOpenExternalLink: () => {},
  };

  list: List;
  rowHeights: RowHeight[] = [];
  areTxAddressesBreaking = false;

  calculateRowHeights = (rows: Row[]): RowHeight[] => (
    rows.map((tx, index) => this.calculateRowHeight(rows, index))
  );

  calculateRowHeight = (rows: Row[], index: number) => {
    const row = rows[index];
    if (row instanceof TransactionsGroup) {
      return GROUP_DATE_HEIGHT;
    }
    if (row instanceof TransactionInfo) {
      const tx = row.tx;
      // Calculate the height of a transaction row:
      const { addresses } = tx;
      const isExpanded = this.props.ixTxExpanded(tx);
      const baseHeight = isExpanded ? TX_ROW_HEIGHT_EXPANDED : TX_ROW_HEIGHT;
      const totalAddresses = addresses.from.length + addresses.to.length;
      // Double the addresses height when the window is so small that they break
      const heightPerAddressFactor = this.areTxAddressesBreaking ? 2 : 1;
      const totalAddressesHeight = heightPerAddressFactor * totalAddresses * TX_HEIGHT_PER_ADDRESS;

      // Extra height for addresses when expanded
      const heightForAddresses = isExpanded ? totalAddressesHeight : 0;

      // Add spacing for the next date header to the last transaction in a group
      const headerSpacing = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;

      return baseHeight + heightForAddresses + headerSpacing;
    }
    return 0;
  };

  recalcRowHeightForIndex = (index: number) => {
    const { rows } = this.props;
    this.rowHeights[index] = this.calculateRowHeight(rows, index);
  };

  findIndexForTx = (tx: WalletTransaction): number => (
    this.props.rows.findIndex(r => (r instanceof TransactionInfo) && r.tx.id === tx.id)
  );

  recomputeHeightForTransaction = (tx: WalletTransaction) => {
    if (!this.list) return;
    this.recalcRowHeightForIndex(this.findIndexForTx(tx));
    this.list.recomputeRowHeights(0);
  };

  /**
   * Since the transaction addresses are pretty long, they break into the
   * next line on smaller window sizes and the height of expanded tx rows
   * in the list must be adjusted accordingly.
   * @param width
   */
  onResize = ({ width }: { width: number }) => {
    let needsUpdate = false;
    if (!this.areTxAddressesBreaking && width < ADDRESS_BREAKPOINT) {
      this.areTxAddressesBreaking = true;
      needsUpdate = true;
    } else if (this.areTxAddressesBreaking && width > ADDRESS_BREAKPOINT) {
      this.areTxAddressesBreaking = false;
      needsUpdate = true;
    }
    if (needsUpdate) {
      this.props.getExpandedTransactions().map(this.recomputeHeightForTransaction);
      // const sortedTxByIndex = this.props.getExpandedTransactions().sort((a, b) => (
      //   this.findIndexForTx(a) - this.findIndexForTx(b)
      // ));
      // console.log(sortedTxByIndex);
      // const firstExpandedInList = sortedTxByIndex[0];
      // if (firstExpandedInList) {
      //   console.log(this.findIndexForTx(firstExpandedInList));
      //   this.recomputeHeightForTransaction(firstExpandedInList);
      // }
    }
  };

  render() {
    const { rows } = this.props;
    this.rowHeights = this.calculateRowHeights(rows);

    // Renders a single row within the virtual list
    const rowRenderer = ({
      key, // Unique key within array of rows
      index, // Index of row within collection
      style // Style object to be applied to row (to position it)
    }: { key: string, index: number, style: string }) => {
      return (
        <div key={key} style={style}>
          {this.props.renderRow(rows[index])}
        </div>
      );
    };

    return (
      <div className={styles.component}>
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
