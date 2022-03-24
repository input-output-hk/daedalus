import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './StakePoolsTable.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ascendi... Remove this comment to see the full error message
import sortIcon from '../../../assets/images/ascending.inline.svg';
import { defaultTableOrdering } from './StakePoolsTable';

type TableHeaderProps = {
  name: string;
  stakePoolsSortBy: string;
  stakePoolsOrder: string;
  onHandleSort: (...args: Array<any>) => any;
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
