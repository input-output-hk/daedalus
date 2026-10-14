import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import { AutoSizer, List, WindowScroller } from 'react-virtualized';
import { GovernancePageScrollContext } from '../layouts/GovernanceWithNavigation';
import DRepCard from './DRepCard';
import DRepTableRow from './DRepTableRow';
import { isStaleFavorite } from './helpers';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDirectoryList.scss';
import tableStyles from './DRepDirectoryTable.scss';
import type { ListViewMode } from '../../../types/listViewTypes';
import { governanceSharedMessages } from '../_shared/governanceSharedMessages';

// The cards tile into a grid, so their footprint has to be known up front:
// react-virtualized sizes rows before it renders them. Stake pools computes
// its own grid the same way from a thumb size and a gap.
const CARD_WIDTH = 300;
const CARD_GAP = 12;
const CARD_HEIGHT = 200;
const TABLE_ROW_HEIGHT = 64;

// Windowing earns its keep over a list long enough to hurt, and costs
// something over one that is not: it fixes the row height, measures the
// scroll container, and renders only part of what is there. The suggested
// cohort is twenty cards and simply renders. Showing every DRep, or a
// favourites list someone has let grow, goes through the window.
const VIRTUALIZE_ABOVE = 50;

const messages = defineMessages({
  columnName: {
    id: 'governance.drepDirectory.table.column.name',
    defaultMessage: '!!!DRep',
    description: 'Table column heading for the DRep name and id',
  },
  columnStatus: {
    id: 'governance.drepDirectory.table.column.status',
    defaultMessage: '!!!Status',
    description: 'Table column heading for the DRep status',
  },
  columnFavorite: {
    id: 'governance.drepDirectory.table.column.favorite',
    defaultMessage: '!!!Favorite',
    description: 'Table column heading for the favorite toggle',
  },
  columnActions: {
    id: 'governance.drepDirectory.table.column.actions',
    defaultMessage: '!!!Actions',
    description: 'Table column heading for the row actions',
  },
});

interface Props {
  entries: AppDRepDirectoryEntry[];
  favoriteDRepIds: ReadonlySet<string>;
  onToggleFavorite: (drepId: string) => void;
  isFavoritesView?: boolean;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  isCurrentDRep?: (entry: AppDRepDirectoryEntry) => boolean;
  viewMode?: ListViewMode;
  canDelegate?: boolean;
  totalDRepStake?: BigNumber | null;
  epochLength?: number | null;
  slotLength?: number | null;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectoryList({
  entries,
  viewMode = 'cards',
  favoriteDRepIds,
  onToggleFavorite,
  isFavoritesView = false,
  isStaleFavoriteEntry = isStaleFavorite,
  isCurrentDRep,
  canDelegate = true,
  totalDRepStake = null,
  epochLength = null,
  slotLength = null,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  const cardProps = (entry: AppDRepDirectoryEntry) => ({
    entry,
    isFavorite: favoriteDRepIds.has(entry.drepId),
    onToggleFavorite,
    isStaleFavorite: isFavoritesView && isStaleFavoriteEntry(entry),
    isCurrentDRep: isCurrentDRep?.(entry),
    canDelegate,
    totalDRepStake,
    epochLength,
    slotLength,
    onSelectForDelegation,
    onViewDetails,
  });

  const rowProps = (entry: AppDRepDirectoryEntry) => ({
    entry,
    isFavorite: favoriteDRepIds.has(entry.drepId),
    onToggleFavorite,
    isCurrentDRep: isCurrentDRep?.(entry),
    canDelegate,
    totalDRepStake,
    epochLength,
    slotLength,
    onSelectForDelegation,
    onViewDetails,
  });

  const renderCardRows = (width: number) => {
    const perRow = Math.max(1, Math.floor(width / (CARD_WIDTH + CARD_GAP)));
    const rowCount = Math.ceil(entries.length / perRow);

    const rowRenderer = ({ index, key, style }) => (
      <div key={key} style={style}>
        <div
          className={styles.cardRow}
          style={{ gridTemplateColumns: `repeat(${perRow}, minmax(0, 1fr))` }}
        >
          {entries
            .slice(index * perRow, index * perRow + perRow)
            .map((entry) => (
              <DRepCard key={entry.drepId} {...cardProps(entry)} />
            ))}
        </div>
      </div>
    );

    return { rowCount, rowHeight: CARD_HEIGHT + CARD_GAP, rowRenderer };
  };

  const renderTableRows = () => ({
    rowCount: entries.length,
    rowHeight: TABLE_ROW_HEIGHT,
    rowRenderer: ({ index, key, style }) => (
      <div key={key} style={style}>
        <DRepTableRow {...rowProps(entries[index])} />
      </div>
    ),
  });

  // One windowed list for both views. The page div in GovernanceWithNavigation
  // is what actually scrolls, so WindowScroller is pointed at it rather than
  // the window, exactly as the stake pools lists are pointed at theirs.
  const renderWindowedList = (
    build: (width: number) => {
      rowCount: number;
      rowHeight: number;
      rowRenderer: (args: any) => React.ReactNode;
    }
  ) => (
    <GovernancePageScrollContext.Consumer>
      {({ scrollElementRef }) => (
        <WindowScroller scrollElement={scrollElementRef?.current ?? window}>
          {({ height, scrollTop, registerChild }) => (
            <AutoSizer disableHeight>
              {({ width }) => {
                if (!width || entries.length === 0) return null;
                const { rowCount, rowHeight, rowRenderer } = build(width);
                return (
                  <div ref={(el) => registerChild(el)}>
                    <List
                      autoHeight
                      width={width}
                      height={height}
                      scrollTop={scrollTop}
                      rowCount={rowCount}
                      rowHeight={rowHeight}
                      rowRenderer={rowRenderer}
                      overscanRowCount={3}
                    />
                  </div>
                );
              }}
            </AutoSizer>
          )}
        </WindowScroller>
      )}
    </GovernancePageScrollContext.Consumer>
  );

  const isVirtualized = entries.length > VIRTUALIZE_ABOVE;

  const renderPlainCards = () => (
    <div className={styles.cardGrid}>
      {entries.map((entry) => (
        <DRepCard key={entry.drepId} {...cardProps(entry)} />
      ))}
    </div>
  );

  const renderPlainRows = () =>
    entries.map((entry) => (
      <DRepTableRow key={entry.drepId} {...rowProps(entry)} />
    ));

  if (viewMode === 'table') {
    return (
      <div className={styles.container}>
        <div className={tableStyles.scrollContainer}>
          <div className={tableStyles.table} role="table">
            <div className={tableStyles.headerRow} role="row">
              {[
                [messages.columnFavorite, tableStyles.colFavorite],
                [messages.columnName, tableStyles.colName],
                [messages.columnStatus, tableStyles.colStatus],
                [
                  governanceSharedMessages.votingPower,
                  tableStyles.colVotingPower,
                ],
                [messages.columnActions, tableStyles.colActions],
              ].map(([message, columnClass]) => (
                <div
                  key={(message as { id: string }).id}
                  className={`${tableStyles.headerCell} ${columnClass}`}
                  role="columnheader"
                >
                  {intl.formatMessage(message)}
                </div>
              ))}
            </div>
            <div role="rowgroup">
              {isVirtualized
                ? renderWindowedList(renderTableRows)
                : renderPlainRows()}
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={styles.container}>{renderWindowedList(renderCardRows)}</div>
  );
}

export default injectIntl(DRepDirectoryList);
