import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsTable.scss' or its... Remove this comment to see the full error message
import styles from './StakePoolsTable.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ascendi... Remove this comment to see the full error message
import sortIcon from '../../../assets/images/ascending.inline.svg';
import { defaultTableOrdering } from './StakePoolsTable';

type TableHeaderProps = {
  availableTableHeaders: Array<{
    name: string;
    title: any;
  }>;
  stakePoolsSortBy: string;
  stakePoolsOrder: string;
  onHandleSort: (...args: Array<any>) => any;
};

@observer
class StakePoolsTableHeader extends Component<TableHeaderProps> {
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

export { StakePoolsTableHeader };
