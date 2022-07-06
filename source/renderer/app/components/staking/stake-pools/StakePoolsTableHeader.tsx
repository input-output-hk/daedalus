import React from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { HeaderGroup } from 'react-table';
import { StakePoolsTableHeaderCell } from './StakePoolsTableHeaderCell';
import StakePool from '../../../domains/StakePool';
import {
  useInViewPort,
  StakePoolsOrder,
  StakePoolSortableProps,
} from './hooks';
import styles from './StakePoolsTable.scss';

type Props = {
  headerGroups: HeaderGroup<StakePool>[];
  stakePoolsOrder: StakePoolsOrder;
  stakePoolsSortBy: StakePoolSortableProps;
  onHandleSort: (name: string) => void;
  onTableHeaderMouseEnter: () => void;
  onTableHeaderMouseLeave: () => void;
};

export function Component({
  stakePoolsSortBy,
  stakePoolsOrder,
  headerGroups,
  onHandleSort,
  onTableHeaderMouseEnter,
  onTableHeaderMouseLeave,
}: Props) {
  const { setTargetRef, isInViewport } = useInViewPort();

  return (
    <>
      <div ref={setTargetRef} />
      <div
        className={classNames(
          styles.thead,
          !isInViewport && styles.stickyHeader
        )}
        onMouseEnter={onTableHeaderMouseEnter}
        onMouseLeave={onTableHeaderMouseLeave}
      >
        {headerGroups.map((headerGroup) => (
          /* eslint-disable-next-line react/jsx-key */
          <div {...headerGroup.getHeaderGroupProps()} className={styles.tr}>
            {headerGroup.headers.map((column) => (
              <StakePoolsTableHeaderCell
                {...column.getHeaderProps({
                  style: { width: undefined },
                })}
                stakePoolsSortBy={stakePoolsSortBy}
                stakePoolsOrder={stakePoolsOrder}
                onHandleSort={onHandleSort}
                name={column.id}
                key={column.id}
              >
                {column.render('Header')}
              </StakePoolsTableHeaderCell>
            ))}
          </div>
        ))}
      </div>
    </>
  );
}

export const StakePoolsTableHeader = observer(Component);
