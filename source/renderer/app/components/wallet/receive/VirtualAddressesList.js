// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import type { Addresses } from '../../../api/addresses/types';
import styles from './VirtualAddressesList.scss';

type Props = {
  rows: Addresses,
  renderRow: Function,
};

const BREAKPOINT_1_LINES = 1172;
const BREAKPOINT_2_LINES = 673;

const ADDRESS_LINE_HEIGHT = 22;
const ADDRESS_LINE_PADDING = 21;

@observer
export class VirtualAddressesList extends Component<Props> {

  list: List;
  lines: number = 0;
  height: number = ADDRESS_LINE_HEIGHT;

  calculateRowsHeight = (lines: number) => {
    this.height = (ADDRESS_LINE_HEIGHT * lines) + ADDRESS_LINE_PADDING;
  };

  /**
   * Gets the number of lines based on the container width
   * @param width
   * @returns {number}
   */
  getLinesFromWidth = (width: number) => {
    if (width >= BREAKPOINT_1_LINES) return 1;
    if (width >= BREAKPOINT_2_LINES) return 2;
    return 3;
  };

  /**
   * Decides if the addresses heights need to be updated
   */
  onResize = ({ width }: { width: number }) => {
  // onResize = (width: number) => {
    const lines = this.getLinesFromWidth(width);
    if (lines !== this.lines) {
      this.lines = lines;
      this.calculateRowsHeight(lines);
    }
  };

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

  render() {
    const { rows } = this.props;
    if (!rows.length) return null;
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
              rowHeight={this.height}
              rowRenderer={this.rowRenderer}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
