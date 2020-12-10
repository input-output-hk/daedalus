// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './StakePoolsTable.scss';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import { defaultTableOrdering } from './StakePoolsTable';

type TableHeaderProps = {
  availableTableHeaders: Array<{ name: string, title: any }>,
  stakePoolsSortBy: string,
  stakePoolsOrder: string,
  onHandleSort: Function,
};

@observer
export class StakePoolsTableHeader extends Component<TableHeaderProps> {
  render() {
    const {
      availableTableHeaders,
      stakePoolsSortBy,
      stakePoolsOrder,
      onHandleSort,
    } = this.props;
    return map(availableTableHeaders, (tableHeader) => {
      const isSorted =
        tableHeader.name === stakePoolsSortBy ||
        (tableHeader.name === 'ticker' && stakePoolsSortBy === 'ticker');
      const defaultOrdering = defaultTableOrdering[tableHeader.name];
      const sortIconClasses = classNames([
        styles.sortIcon,
        isSorted ? styles.sorted : null,
        isSorted && stakePoolsOrder === 'asc' ? styles.ascending : null,
        isSorted && styles[`${stakePoolsOrder}CurrentOrdering`],
        styles[`${defaultOrdering}DefaultOrdering`],
      ]);
      return (
        <th
          key={tableHeader.name}
          onClick={() => onHandleSort(tableHeader.name)}
        >
          {tableHeader.title}
          <SVGInline svg={sortIcon} className={sortIconClasses} />
        </th>
      );
    });
  }
}
