import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './StakePoolsTable.scss';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import { defaultTableOrdering } from './StakePoolsTable';
import { StakePoolsOrder, StakePoolSortableProps } from './hooks';

type TableHeaderProps = {
  name: string;
  stakePoolsSortBy: StakePoolSortableProps;
  stakePoolsOrder: StakePoolsOrder;
  onHandleSort: (name: string) => void;
  children: React.ReactNode;
};

@observer
class StakePoolsTableHeaderCell extends Component<TableHeaderProps> {
  render() {
    const {
      name,
      stakePoolsSortBy,
      stakePoolsOrder,
      onHandleSort,
      children,
      ...headerProps
    } = this.props;
    const isSorted = name === stakePoolsSortBy;
    const defaultOrdering = defaultTableOrdering[name];
    const sortIconClasses = classNames([
      styles.sortIcon,
      isSorted ? styles.sorted : null,
      isSorted && styles[`${stakePoolsOrder}CurrentOrdering`],
      styles[`${defaultOrdering}DefaultOrdering`],
    ]);
    return (
      <div
        className={styles.th}
        onClick={() => onHandleSort(name)}
        {...headerProps}
      >
        {children}
        <SVGInline svg={sortIcon} className={sortIconClasses} />
      </div>
    );
  }
}

export { StakePoolsTableHeaderCell };
