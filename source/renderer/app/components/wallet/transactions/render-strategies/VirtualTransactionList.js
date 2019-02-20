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
import waitForExist from '../../../../utils/waitForExist';

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
const TX_EXPANDED_ROW_BASE_HEIGHT = 285;
const TX_EXPANDED_ROW_BASE_PADDING = 10;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_ADDRESS_SELECTOR = '.Transaction_address';
const TX_ID_SELECTOR = '.Transaction_transactionId';
const TX_SELECTOR = '.Transaction_expanded';

@observer
export class VirtualTransactionList extends Component<Props> {

  static defaultProps = {
    isLoadingSpinnerShown: false,
    isSyncingSpinnerShown: false,
  };

  constructor(props: Props) {
    super(props);
    window.toggleDebounce = () => {
      this.setState(({
        debounce: !this.state.debounce
      }));
      console.log(`Debounce ${this.state.debounce}`);
    };
    window.toggleThrottle = () => {
      this.setState(({
        throttle: !this.state.throttle
      }));
      console.log(`Throttle ${this.state.throttle}`);
    };
    window.toggleLog = () => {
      this.setState(({
        log: !this.state.log
      }));
      console.log(`Log ${this.state.log}`);
    };
    window.debounce = this.state.debounce;
    window.throttle = this.state.throttle;
    window.log = this.state.log;

  }

  state = {
    debounce: false,
    throttle: false,
    log: false
  };

  list: List;
  rowHeights: RowHeight[] = [];
  txAddressHeight: number = 0;
  txIdHeight: number = 0;
  txExpandedRowBaseHeight: number = TX_EXPANDED_ROW_BASE_HEIGHT;
  baseHeightWasCalculated: boolean = false;

  /**
   * Returns the row index of a given tx.
   */
  findIndexForTx = (tx: WalletTransaction): number => (
    this.props.rows.findIndex(r => (r instanceof TransactionInfo) && r.tx.id === tx.id)
  );

  /**
   * Recomputes virtual row heights only once per tick (debounced)
   */
  recomputeVirtualRowHeights = this.state.debounce
    ? debounce((startIndex: number = 0): void => {
      const { list } = this;
      if (!list) return;
      list.recomputeRowHeights(startIndex);
    })
    : (startIndex: number = 0) => {
      const { list } = this;
      if (!list) return;
      list.recomputeRowHeights(startIndex);
    };

  /**
   * Updates and recomputes row height
   */
  updateInfoRowHeight = (tx: WalletTransaction, isExpanded: boolean): void => {
    const { rowHeights } = this;
    const txIndex = this.findIndexForTx(tx);
    const row = this.props.rows[txIndex];
    if (row instanceof TransactionsGroup) return;
    rowHeights[txIndex] = isExpanded
      ? this.calculateHeightOfTxExpandedRow(row.tx)
      : this.calculateHeightOfTxContractedRow(row);
    this.recomputeVirtualRowHeights(0);
  };

  /**
   * Gets an Info expanded row height
   */
  calculateHeightOfTxExpandedRow = (tx: WalletTransaction): number => {
    if (!this.txAddressHeight) {
      this.updateAddressesAndIdHeights();
    }
    const txSingleAddressHeight = this.txAddressHeight;
    const txIdHeight = this.txIdHeight;
    const txExpandedRowBaseHeight = this.txExpandedRowBaseHeight;
    const { addresses } = tx;
    const txAddressesCount = addresses.from.length + addresses.to.length;
    const txAddressesHeight = (txAddressesCount * txSingleAddressHeight);
    return txAddressesHeight + txIdHeight + txExpandedRowBaseHeight;
  };

  /**
   * Gets an Info contracted row height
   */
  calculateHeightOfTxContractedRow = (row: Row): number => {
    const headerSpacing = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;
    return TX_CONTRACTED_ROW_HEIGHT + headerSpacing;
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

  getBaseHeight = async () => {
    const firstExpandedTx = await waitForExist(TX_SELECTOR);
    const firstTxHeight = firstExpandedTx.offsetHeight;
    const numberOfAddresses = firstExpandedTx.querySelectorAll(TX_ADDRESS_SELECTOR).length;
    this.baseHeightWasCalculated = true;
    return firstTxHeight + TX_EXPANDED_ROW_BASE_PADDING -
      (this.txAddressHeight * numberOfAddresses) - this.txIdHeight;
  };

  /**
   * Calculates the number of lines of the addresses and id from the first expanded tx
   */
  updateAddressesAndIdHeights = async (): Promise<void> => {
    const firstTxAddress = document.querySelector(TX_ADDRESS_SELECTOR);
    const firstTxId = document.querySelector(TX_ID_SELECTOR);
    if (firstTxAddress instanceof HTMLElement && firstTxId instanceof HTMLElement) {
      this.txAddressHeight = firstTxAddress.offsetHeight;
      this.log('this.txAddressHeight', this.txAddressHeight);
      this.txIdHeight = firstTxId.offsetHeight;
      this.log('this.txIdHeight', this.txIdHeight);
      if (!this.baseHeightWasCalculated) this.txExpandedRowBaseHeight = await this.getBaseHeight();
      this.log('this.txExpandedRowBaseHeight', this.txExpandedRowBaseHeight);
    }
  };

  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   */
  onResize = (): void => {
    const { rows, getExpandedTransactions } = this.props;
    const expandedTransactions = getExpandedTransactions();

    // First load, calculates all the rows heights
    if (!this.rowHeights.length) {
      this.rowHeights = this.calculateRowHeights(rows);
      return;
    }
    // Subsequently resizes, updates the expanded rows heights if there is any expanded one
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

  log(...msg) {
    this.state.log ? console.log(...msg) : null;
  }

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
        <AutoSizer onResize={this.state.throttle
          ? throttle(this.onResize, 50)
          : this.onResize
        }
        >
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
