import React, { useEffect, useState, useCallback } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { injectIntl } from 'react-intl';
import { useTable, useFlexLayout } from 'react-table';
import List from 'react-virtualized/dist/commonjs/List';
import WindowScroller from 'react-virtualized/dist/commonjs/WindowScroller';
import AutoSizer from 'react-virtualized/dist/commonjs/AutoSizer';
import { Intl } from '../../../types/i18nTypes';
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';
import styles from './StakePoolsTable.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import BorderedBox from '../../widgets/BorderedBox';
import {
  useSortedStakePoolList,
  useCreateColumns,
  StakePoolsOrder,
  StakePoolSortableProps,
} from './hooks';
import { StakePoolsTableHeader } from './StakePoolsTableHeader';

export const defaultTableOrdering: Record<
  StakePoolSortableProps,
  StakePoolsOrder
> = {
  ranking: StakePoolsOrder.Asc,
  ticker: StakePoolsOrder.Asc,
  saturation: StakePoolsOrder.Asc,
  cost: StakePoolsOrder.Asc,
  profitMargin: StakePoolsOrder.Asc,
  producedBlocks: StakePoolsOrder.Desc,
  nonMyopicMemberRewards: StakePoolsOrder.Desc,
  pledge: StakePoolsOrder.Asc,
  retiring: StakePoolsOrder.Asc,
};
// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;
type Props = {
  stakePoolsList: Array<StakePool>;
  listName?: string;
  isListActive?: boolean;
  currentTheme: string;
  setListActive?: (...args: Array<any>) => any;
  showWithSelectButton?: boolean;
  onSelect?: (...args: Array<any>) => any;
  containerClassName: string;
  numberOfRankedStakePools: number;
  selectedPoolId?: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  currentLocale: string;
  onTableHeaderMouseEnter: (...args: Array<any>) => any;
  onTableHeaderMouseLeave: (...args: Array<any>) => any;
  intl: Intl;
};
type State = {
  isPreloading: boolean;
  stakePoolsOrder: StakePoolsOrder;
  stakePoolsSortBy: StakePoolSortableProps;
};
const initialState: State = {
  isPreloading: true,
  stakePoolsOrder: StakePoolsOrder.Asc,
  stakePoolsSortBy: 'ranking',
};

function StakePoolsTableComponent({
  stakePoolsList = [],
  listName,
  onTableHeaderMouseEnter,
  onTableHeaderMouseLeave,
  showWithSelectButton = false,
  intl,
  numberOfRankedStakePools,
  currentTheme,
  onOpenExternalLink,
  containerClassName,
  onSelect,
}: Props) {
  const [state, setState] = useState<State>(initialState);

  useEffect(() => {
    setState((s) => ({ ...s, isPreloading: false }));
  }, []);

  const handleSort = useCallback(
    (newSortBy: StakePoolSortableProps) => {
      const { stakePoolsOrder, stakePoolsSortBy } = state;
      let newOrder = defaultTableOrdering[newSortBy];

      if (newSortBy === stakePoolsSortBy) {
        newOrder =
          stakePoolsOrder === StakePoolsOrder.Asc
            ? StakePoolsOrder.Desc
            : StakePoolsOrder.Asc;
      }

      setState((s) => ({
        ...s,
        stakePoolsOrder: newOrder,
        stakePoolsSortBy: newSortBy,
      }));
    },
    [state]
  );

  const { isPreloading, stakePoolsSortBy, stakePoolsOrder } = state;

  const sortedStakePoolList = useSortedStakePoolList({
    order: stakePoolsOrder,
    sortBy: stakePoolsSortBy,
    stakePoolList: stakePoolsList,
  });

  const columns = useCreateColumns({
    containerClassName,
    currentTheme,
    intl,
    numberOfRankedStakePools,
    onOpenExternalLink,
    onSelect,
    showWithSelectButton,
  });

  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    rows,
    prepareRow,
  } = useTable(
    {
      columns,
      data: sortedStakePoolList,
    },
    useFlexLayout
  );

  const RenderRow = useCallback(
    ({ index, style }) => {
      const row = rows[index];
      prepareRow(row);
      return (
        <div
          {...row.getRowProps({
            style,
          })}
          className={styles.tr}
        >
          {row.cells.map((cell) => {
            return (
              /* eslint-disable-next-line react/jsx-key */
              <div
                {...cell.getCellProps({ style: { width: undefined } })}
                className={styles.td}
              >
                {cell.render('Cell')}
              </div>
            );
          })}
        </div>
      );
    },
    [prepareRow, rows]
  );

  const componentClasses = classNames([styles.component, listName]);

  if (stakePoolsList.length > PRELOADER_THRESHOLD && isPreloading)
    return (
      <div className={styles.preloadingBlockWrapper}>
        <LoadingSpinner big />
      </div>
    );

  return (
    <StakingPageScrollContext.Consumer>
      {(stakePoolsScrollContext) => (
        <WindowScroller
          scrollElement={stakePoolsScrollContext.scrollElementRef.current}
        >
          {({
            height,
            isScrolling,
            registerChild,
            onChildScroll,
            scrollTop,
          }) => (
            <div>
              <div className={componentClasses}>
                {sortedStakePoolList.length > 0 && (
                  <BorderedBox>
                    <div {...getTableProps()} className={styles.table}>
                      <StakePoolsTableHeader
                        stakePoolsSortBy={stakePoolsSortBy}
                        stakePoolsOrder={stakePoolsOrder}
                        headerGroups={headerGroups}
                        onHandleSort={handleSort}
                        onTableHeaderMouseEnter={onTableHeaderMouseEnter}
                        onTableHeaderMouseLeave={onTableHeaderMouseLeave}
                      />

                      <AutoSizer disableHeight>
                        {({ width }) => (
                          <div
                            className={styles.tbody}
                            {...getTableBodyProps()}
                            ref={registerChild}
                          >
                            <List
                              autoHeight
                              height={height || 0}
                              isScrolling={isScrolling}
                              onScroll={onChildScroll}
                              overscanRowCount={10}
                              rowCount={rows.length}
                              rowHeight={36}
                              rowRenderer={RenderRow}
                              scrollTop={scrollTop}
                              width={width}
                            />
                          </div>
                        )}
                      </AutoSizer>
                    </div>
                  </BorderedBox>
                )}
              </div>
            </div>
          )}
        </WindowScroller>
      )}
    </StakingPageScrollContext.Consumer>
  );
}

export const StakePoolsTable = injectIntl(observer(StakePoolsTableComponent));
