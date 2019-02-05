// @flow
import React, { Component } from 'react';
import { throttle } from 'lodash';
import { observer } from 'mobx-react';
import { AutoSizer, List } from 'react-virtualized';
import type { Addresses } from '../../../api/addresses/types';
import styles from './VirtualAddressesList.scss';

type Props = {
  rows: Addresses,
  renderRow: Function,
};

type State = {
  height: number,
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

@observer
export class VirtualAddressesList extends Component<Props, State> {

  list: List;
  lines: number = 0;

  state = {
    height: ADDRESS_LINE_HEIGHT,
  };

  calculateRowsHeight = (lines: number) => {
    this.setState({
      height: (ADDRESS_LINE_HEIGHT * lines) + ADDRESS_LINE_PADDING,
    });
  };

  /**
   * Gets the number of lines based on the container width
   * @param width
   * @returns {number}
   */
  getLinesFromWidth = (width: number) => {
    if (width >= BREAKPOINT_1_LINE) return 1;
    if (width >= BREAKPOINT_2_LINES) return 2;
    return 3;
  };

  /**
   * Decides if the addresses height need to be updated
   */
  onResize = ({ width }: { width: number }) => {
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
        <AutoSizer onResize={throttle(this.onResize, 100)}>
          {({ width, height }) => (
            <List
              className={styles.list}
              ref={(list) => this.list = list}
              width={width}
              height={height}
              rowCount={rows.length}
              rowHeight={this.state.height}
              rowRenderer={this.rowRenderer}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
