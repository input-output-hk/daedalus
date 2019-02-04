// @flow
import React, { Component } from 'react';
import { debounce } from 'lodash';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import type { Addresses, Address } from '../../../api/addresses/types';
import styles from './VirtualAddressesList.scss';

type Props = {
  rows: Addresses,
  renderRow: Function,
};

type RowHeight = number;

const BREAKPOINT_1_LINES = 1172;
const BREAKPOINT_2_LINES = 673;

const ADDRESS_LINE_HEIGHT = 22;
const ADDRESS_LINE_PADDING = 21;

@observer
export class VirtualAddressesList extends Component<Props> {

  list: List;
  lines: number = 0;
  rowHeights: RowHeight[] = [];

  calculateRowHeights = (lines: number) => {
    const { rows } = this.props;
    const height = this.getHeightFromNumberOfLines(lines);
    rows.forEach((row: Address, index: number) => {
      this.rowHeights[index] = height;
      this.list.recomputeRowHeights(index);
    });
  };

  /**
   * Gets the breakpoint based on the container width
   * @param width
   * @returns {number}
   */
  getLinesFromWidth = (width: number) => {
    if (width >= BREAKPOINT_1_LINES) return 1;
    if (width >= BREAKPOINT_2_LINES) return 2;
    return 3;
  };

  /**
   * Calculates the row's height based on the number of lines
   * @param lines
   * @returns {number}
   */
  getHeightFromNumberOfLines = (lines: number) => (
    (ADDRESS_LINE_HEIGHT * lines) + ADDRESS_LINE_PADDING
  );

  /**
   * Decides if the addresses heights need to be updated
   * and which type of calculation, generic or individual
   */
  onResize = (width: number) => {
    const lines = this.getLinesFromWidth(width);
    if (lines !== this.lines) {
      this.lines = lines;
      this.calculateRowHeights(lines);
    }
  };

  /**
   * Updates width and triggers re-calculation of breakpoints after a debounced resize.
   */
  onResizeDebounce = debounce(({ width }: { width: number }) => {
    this.onResize(width);
  }, 100, { leading: true, trailing: true });

  rowRenderer = ({
    index, // Index of row
    key, // Unique key within array of rendered rows
    style // Style object to be applied to row (to position it);
  }: { key: string, index: number, style: string }) => {
    const { rows, renderRow } = this.props;
    const address = rows[index];
    return (
      <div
        key={key}
        style={style}
        className={styles.address}
      >
        {renderRow(address, index)}
      </div>
    );
  };

  getRowHeights = ({ index }: { index: number }) => (
    this.rowHeights[index] || this.getHeightFromNumberOfLines(this.lines)
  );

  render() {
    const { rows } = this.props;
    if (!rows.length) return null;
    return (
      <div className={styles.component}>
        <AutoSizer onResize={this.onResizeDebounce}>
          {({ width, height }) => (
            <List
              className={styles.list}
              ref={(list) => this.list = list}
              width={width}
              height={height}
              rowCount={rows.length}
              rowHeight={this.getRowHeights}
              rowRenderer={this.rowRenderer}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
