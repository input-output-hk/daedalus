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

@observer
export class VirtualAddressesList extends Component<Props> {

  /**
   * Since the transaction addresses are pretty long, they break into the next line on smaller
   * window sizes and the height of expanded tx rows in the list must be adjusted accordingly.
   * @param width
   */
  onResize = ({ width }: { width: number }) => {
    console.log('width', width);
  };

  rowRenderer = ({
    index, // Index of row
    key, // Unique key within array of rendered rows
    style // Style object to be applied to row (to position it);
  }: { key: string, index: number, style: string }) => {
    const { rows, renderRow, showUsed } = this.props;
    console.log('showUsed', showUsed);
    const address = rows[index];
    const isAddressVisible = !address.used || showUsed;
    console.log('isAddressVisible', isAddressVisible);
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
        <AutoSizer onResize={this.onResize}>
          {({ width, height }) => (
            <List
              className={styles.list}
              width={width}
              height={height}
              rowCount={rows.length}
              rowHeight={68}
              rowRenderer={this.rowRenderer}
            />
          )}
        </AutoSizer>
      </div>
    );
  }
}
