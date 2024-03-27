'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.VirtualTransactionList = void 0;
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const react_virtualized_1 = require('react-virtualized');
const lodash_1 = require('lodash');
const WalletTransactionsList_1 = require('../WalletTransactionsList');
const types_1 = require('../types');
const VirtualTransactionList_scss_1 = __importDefault(
  require('./VirtualTransactionList.scss')
);
const GROUP_DATE_HEIGHT = 26;
const TX_CONTRACTED_ROW_HEIGHT = 86;
const TX_EXPANDED_ROW_BASE_HEIGHT = 260 + 16;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_BOTTOM_BORDER_MARGIN = 1;
const TX_ADDRESS_SELECTOR = '.Transaction_address';
const TX_ID_SELECTOR = '.Transaction_transactionId';
let VirtualTransactionList = class VirtualTransactionList extends react_1.Component {
  list;
  rowHeights = [];
  txAddressHeight = 0;
  txIdHeight = 0;
  overscanStartIndex;
  overscanStopIndex;
  static defaultProps = {
    isLoadingSpinnerShown: false,
    isSyncingSpinnerShown: false,
  };
  componentDidMount() {
    window.addEventListener('resize', this.onResize);
  }
  componentWillUnmount() {
    window.removeEventListener('resize', this.onResize);
  }
  /**
   * Returns the row index of a given tx.
   */
  findIndexForTx = (tx) =>
    this.props.rows.findIndex(
      (r) => r instanceof types_1.TransactionInfo && r.tx.id === tx.id
    );
  /**
   * Recomputes virtual row heights only once per tick (debounced)
   */
  recomputeVirtualRowHeights = (0, lodash_1.debounce)((startIndex = 0) => {
    const { list } = this;
    if (!list) return;
    list.recomputeRowHeights(startIndex);
  });
  /**
   * Calculates the number of lines of the addresses and id from the first expanded tx
   */
  updateAddressesAndIdHeights = () => {
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
  estimateHeightOfTxExpandedRow = (row, tx) => {
    if (!this.txAddressHeight) this.updateAddressesAndIdHeights();
    const txSingleAddressHeight = this.txAddressHeight;
    const txIdHeightValue = this.txIdHeight;
    const { addresses } = tx;
    const txAddressesCount = addresses.from.length + addresses.to.length;
    const txAddressesHeight = txAddressesCount * txSingleAddressHeight;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isLastInGroup' does not exist on type 'R... Remove this comment to see the full error message
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
  estimateHeightOfTxContractedRow = (row) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isLastInGroup' does not exist on type 'R... Remove this comment to see the full error message
    const txBottomMargin = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;
    return TX_CONTRACTED_ROW_HEIGHT + txBottomMargin;
  };
  /**
   * Updates and recomputes row height
   */
  updateTxRowHeight = (
    tx,
    isExpanded,
    wasToggled // Is "true" when transaction is manually expanded/collapsed
  ) => {
    const { rowHeights } = this;
    const txIndex = this.findIndexForTx(tx);
    const row = this.props.rows[txIndex];
    if (row instanceof types_1.TransactionsGroup) return;
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
  estimateRowHeight = (row) => {
    if (row instanceof types_1.TransactionInfo) {
      const expandedTxMap = this.props.getExpandedTransactions();
      if (expandedTxMap.has(row.tx.id)) {
        return this.estimateHeightOfTxExpandedRow(row, row.tx);
      }
      return this.estimateHeightOfTxContractedRow(row);
    }
    return GROUP_DATE_HEIGHT;
  };
  /**
   * Maps over all rows and returns array of calculated heights.
   */
  estimateRowHeights = (rows) => rows.map(this.estimateRowHeight);
  /**
   * Returns the DOM element for given transaction id
   */
  getTxRowElementById = (id) => document.getElementById(`tx-${id}`);
  /**
   * Measures the exact height of a rendered tx content DOM element.
   */
  measureTxContentHeight = (tx) => {
    const txRow = this.getTxRowElementById(tx.id);
    if (txRow) {
      const txElement = txRow.firstChild;
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'ChildNode' is not assignable to ... Remove this comment to see the full error message
      const style = window.getComputedStyle(txElement, null);
      return parseInt(style.getPropertyValue('height'), 10);
    }
    return null;
  };
  /**
   * Checks if rendered tx content DOM element has been fully expanded.
   */
  checkIfTxContentIsFullyExpanded = (tx) => {
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
  correctExpandedTxHeightEstimationErrors = (tx, estimatedHeight) => {
    const txContentHeight = this.measureTxContentHeight(tx);
    if (!txContentHeight) return;
    const { rows } = this.props;
    const txIndex = this.findIndexForTx(tx);
    const txInfo = rows[txIndex];
    if (txInfo instanceof types_1.TransactionInfo) {
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
    const expandedTxMap = this.props.getExpandedTransactions();
    // This is needed because a spread Map results in an array of [key, value]
    const expandedTxArray = [...expandedTxMap].map((mapValue) => mapValue[1]);
    const visibleExpandedTx = expandedTxArray.filter((tx) => {
      const index = this.findIndexForTx(tx);
      return (
        index >= this.overscanStartIndex && index <= this.overscanStopIndex
      );
    });
    visibleExpandedTx.forEach((tx) => {
      this.updateTxRowHeight(tx, true, false);
      const estimatedHeight = this.rowHeights[this.findIndexForTx(tx)];
      this.correctExpandedTxHeightEstimationErrors(tx, estimatedHeight);
    });
  };
  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   */
  onResize = () => {
    // First load, calculates all the rows heights
    if (!this.rowHeights.length) {
      this.rowHeights = this.estimateRowHeights(this.props.rows);
      return;
    }
    // Subsequently resizes, updates the expanded rows heights if there is any expanded one
    const expandedTransactions = this.props.getExpandedTransactions();
    if (!expandedTransactions.size) return;
    this.updateAddressesAndIdHeights();
    this.updateVisibleExpandedTxRowHeights();
  };
  /**
   * Callback that gets invoked when virtual rows are rendered.
   * Used to update the array of visible expanded transactions and remeasure their height.
   */
  onRowsRendered = ({ overscanStartIndex, overscanStopIndex }) => {
    this.overscanStartIndex = overscanStartIndex;
    this.overscanStopIndex = overscanStopIndex;
    this.updateVisibleExpandedTxRowHeights();
  };
  rowRenderer = ({
    key,
    // Unique key within array of rows
    index,
    // Index of row within collection
    style, // Style object to be applied to row (to position it)
  }) =>
    // @ts-ignore ts-migrate(2559) FIXME: Type 'string' has no properties in common with typ... Remove this comment to see the full error message
    react_1.default.createElement(
      'div',
      {
        key: key,
        style: style,
        className: VirtualTransactionList_scss_1.default.row,
      },
      this.props.renderRow(this.props.rows[index])
    );
  onListScroll = (context, { scrollTop }) => {
    context.setIsScrolling(scrollTop > 10);
  };
  // =============== REACT LIFECYCLE ================= //
  render() {
    const { rows, isLoadingSpinnerShown, isSyncingSpinnerShown } = this.props;
    // Prevent List rendering if we have no rows to render
    if (!rows.length) return false;
    if (rows.length !== this.rowHeights.length) {
      this.rowHeights = this.estimateRowHeights(rows);
      this.updateVisibleExpandedTxRowHeights();
    }
    const componentStyles = (0, classnames_1.default)([
      VirtualTransactionList_scss_1.default.component,
      isLoadingSpinnerShown
        ? VirtualTransactionList_scss_1.default.withLoadingSpinner
        : null,
      isSyncingSpinnerShown
        ? VirtualTransactionList_scss_1.default.withSyncingSpinner
        : null,
    ]);
    return react_1.default.createElement(
      WalletTransactionsList_1.WalletTransactionsListScrollContext.Consumer,
      null,
      (context) =>
        react_1.default.createElement(
          'div',
          { className: componentStyles },
          react_1.default.createElement(
            react_virtualized_1.AutoSizer,
            {
              onResize: (0, lodash_1.throttle)(this.onResize, 100, {
                leading: true,
                trailing: true,
              }),
            },
            ({ width, height }) =>
              react_1.default.createElement(react_virtualized_1.List, {
                className: VirtualTransactionList_scss_1.default.list,
                ref: (list) => {
                  this.list = list;
                },
                width: width,
                height: height,
                onRowsRendered: (0, lodash_1.throttle)(
                  this.onRowsRendered,
                  100,
                  {
                    leading: true,
                    trailing: true,
                  }
                ),
                rowCount: rows.length,
                rowHeight: ({ index }) =>
                  this.rowHeights[index] || TX_CONTRACTED_ROW_HEIGHT,
                rowRenderer: this.rowRenderer,
                style: {
                  overflowY: 'scroll',
                },
                onScroll: (param) => this.onListScroll(context, param),
              })
          )
        )
    );
  }
};
VirtualTransactionList = __decorate(
  [mobx_react_1.observer],
  VirtualTransactionList
);
exports.VirtualTransactionList = VirtualTransactionList;
//# sourceMappingURL=VirtualTransactionList.js.map
