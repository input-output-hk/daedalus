import React from 'react';
import { observer } from 'mobx-react';
import { map } from 'lodash';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './StakePoolsTable.scss';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import { defaultTableOrdering } from './StakePoolsTable';
import { useInViewPort } from '../../widgets/InView';

type Props = {
  availableTableHeaders: Array<{
    name: string;
    title: any;
  }>;
  stakePoolsSortBy: string;
  stakePoolsOrder: string;
  onHandleSort: (newSortBy: string) => void;
  onTableHeaderMouseEnter: () => void;
  onTableHeaderMouseLeave: () => void;
};

export const StakePoolsTableHeader = observer(
  ({
    availableTableHeaders,
    stakePoolsSortBy,
    stakePoolsOrder,
    onHandleSort,
    onTableHeaderMouseEnter,
    onTableHeaderMouseLeave,
  }: Props) => {
    const { setTargetRef, isInViewport } = useInViewPort();

    return (
      <>
        <caption ref={setTargetRef} />
        <thead
          className={!isInViewport ? styles.stickyHeader : ''}
          onMouseEnter={onTableHeaderMouseEnter}
          onMouseLeave={onTableHeaderMouseLeave}
        >
          <tr>
            {map(availableTableHeaders, (tableHeader) => {
              const isSorted =
                tableHeader.name === stakePoolsSortBy ||
                (tableHeader.name === 'ticker' &&
                  stakePoolsSortBy === 'ticker');
              const defaultOrdering = defaultTableOrdering[tableHeader.name];
              const sortIconClasses = classNames([
                styles.sortIcon,
                isSorted ? styles.sorted : null,
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
            })}
          </tr>
        </thead>
      </>
    );
  }
);
