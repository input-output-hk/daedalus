import React, { Component } from 'react';
import { throttle } from 'lodash';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import WalletAddress from '../../../domains/WalletAddress';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VirtualAddressesList.scss' o... Remove this comment to see the full error message
import styles from './VirtualAddressesList.scss';

type Props = {
  rows: Array<WalletAddress>;
  renderRow: (...args: Array<any>) => any;
};

/**
 *
 * The breakpoints define the number of lines
 * based on the width of the following element:
 *
 * `.ReactVirtualized__Grid__innerScrollContainer`
 *
 */
const BREAKPOINT_1_LINE = 1108;
const BREAKPOINT_2_LINES = 635;
const ADDRESS_LINE_HEIGHT = 22;
const ADDRESS_LINE_PADDING = 21;
const ADDRESS_SELECTOR = '.Address';

@observer
class VirtualAddressesList extends Component<Props> {
  list: List;
  listWidth = 0;
  addressHeight = 0;

  /**
   * Estimate the address height based on number of lines
   */
  estimateAddressHeight = (lines: number): number =>
    ADDRESS_LINE_HEIGHT * lines + ADDRESS_LINE_PADDING;

  /**
   * Gets the number of lines based on the container width
   */
  getLinesFromWidth = (width: number): number => {
    if (width >= BREAKPOINT_1_LINE) return 1;
    if (width >= BREAKPOINT_2_LINES) return 2;
    return 3;
  };

  /**
   * Virtual row heights only once per tick (debounced)
   */
  updateRowHeights = () => {
    const { list, addressHeight } = this;
    if (!list) return;
    const firstAddress = document.querySelector(ADDRESS_SELECTOR);

    if (firstAddress instanceof HTMLElement) {
      this.addressHeight = firstAddress.offsetHeight;
    } else {
      this.addressHeight = this.estimateAddressHeight(
        this.getLinesFromWidth(this.listWidth)
      );
      // Since we could only estimate the address heights, re-try
      // the update and hope that DOM is rendered then (for exact measurements)
      setTimeout(this.updateRowHeights, 100);
    }

    if (addressHeight !== this.addressHeight) {
      list.recomputeRowHeights(0);
    }
  };

  /**
   * Update row height and recompute virtual rows.
   */
  onResize = ({ width }: { width: number }): void => {
    this.listWidth = width;
    this.updateRowHeights();
  };
  rowRenderer = ({
    index,
    // Index of row
    key,
    // Unique key within array of rendered rows
    style, // Style object to be applied to row (to position it);
  }: {
    index: number;
    key: string;
    style: string;
  }) => {
    const { rows, renderRow } = this.props;
    const address = rows[index];
    return (
      // @ts-ignore ts-migrate(2559) FIXME: Type 'string' has no properties in common with typ... Remove this comment to see the full error message
      <div key={key} style={style} className={styles.address}>
        {renderRow(address, index)}
      </div>
    );
  };

  render() {
    const { rows } = this.props;
    if (!rows.length) return null;
    return (
      <div className={styles.component}>
        <AutoSizer onResize={throttle(this.onResize, 100)}>
          {({ width, height }) => (
            <List
              className={styles.list}
              ref={(list) => {
                this.list = list;
              }}
              width={width}
              height={height}
              rowCount={rows.length}
              rowHeight={() => this.addressHeight}
              rowRenderer={this.rowRenderer}
              style={{
                overflowY: 'scroll',
              }}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}

export { VirtualAddressesList };
