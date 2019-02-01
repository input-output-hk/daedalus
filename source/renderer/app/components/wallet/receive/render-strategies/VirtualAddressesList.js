// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import type { Addresses } from '../../../../api/addresses/types';
import styles from './VirtualAddressesList.scss';

type Props = {
  rows: Addresses,
  renderRow: Function,
  showUsed: boolean,
};

const ADDRESS_LINE_HEIGHT = 30;
const ADDRESS_TWO_LINES_BREAKPOINT = 1198;
const ADDRESS_THREE_LINES_BREAKPOINT = 668;

@observer
export class VirtualAddressesList extends Component<Props> {

  rowHeight: number = 0;

  updateRowHeight = (width: number) => {
    let lines = 1;
    let padding = 10;
    if (width <= ADDRESS_TWO_LINES_BREAKPOINT) lines = 2;
    if (width <= ADDRESS_THREE_LINES_BREAKPOINT) lines = 3;
    if (lines > 1) padding = 0;
    this.rowHeight = (ADDRESS_LINE_HEIGHT * lines) + padding;
  }

  rowRenderer = ({
    index, // Index of row
    key, // Unique key within array of rendered rows
    style // Style object to be applied to row (to position it);
  }: { key: string, index: number, style: string }) => {
    const { rows, renderRow, showUsed } = this.props;
    const address = rows[index];
    const isAddressVisible = !address.used || showUsed;
    if (!isAddressVisible) return null;
    return (
      <div
        key={key}
        style={style}
        className={styles.address}
      >
        {renderRow(address, index)}
      </div>
    );
  }

  render() {
    const { rows } = this.props;

    return (
      <div className={styles.component}>
        <AutoSizer onResize={({ width }) => this.updateRowHeight(width)}>
          {({ width, height }) => {
            this.updateRowHeight(width);
            return (
              <List
                className={styles.list}
                width={width}
                height={height}
                rowCount={rows.length}
                rowHeight={this.rowHeight}
                rowRenderer={this.rowRenderer}
              />
            );
          }
        }
        </AutoSizer>
      </div>
    );
  }
}
