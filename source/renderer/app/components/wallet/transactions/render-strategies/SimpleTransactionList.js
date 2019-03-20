// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import type { Row } from '../types';
import styles from './SimpleTransactionList.scss';

type Props = {
  renderRow: (Row) => Node,
  rows: Row[],
};

@observer
export class SimpleTransactionList extends Component<Props> {

  static defaultProps = {
    onOpenExternalLink: () => {},
  };

  render() {
    const { rows, renderRow } = this.props;
    return (
      <div className={styles.component}>
        {rows.map((row, index) => (
          <div key={index}>
            {renderRow(row)}
          </div>
        ))}
      </div>
    );
  }
}
