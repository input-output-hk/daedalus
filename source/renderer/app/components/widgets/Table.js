// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map, entries, reduce } from 'lodash';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import sortIcon from '../../assets/images/ascending.inline.svg';
import styles from './Table.scss';
import {
  bigNumberComparator,
  dateComparator,
  numberComparator,
  stringComparator,
} from '../../utils/sortComparators';

export type TableColumn = {
  title: any,
  id: string,
  // Custom function for rendering a table cell
  render?: Function,
  // If `type` is informed, the specific type will be used for sorting
  type?: 'date' | 'node' | 'bigNumber',
  // Custom function for rendering a sorting value
  sortValue?: Function,
};

export type TableRow = {
  [key: string]: any,
};

const SORT_ORDERS = {
  ASCENDING: 'asc',
  DESCENDING: 'desc',
};

type Props = {
  columns: Array<TableColumn>,
  rows: Array<TableRow>,
  className?: string,
  onClickRow?: Function,
  onClickCell?: Function,
  enableSort?: boolean,
  sortBy?: string,
  maxHeight?: number,
  isCompact?: boolean,
};

type State = {
  sortOrder: string,
  sortBy: string,
};

@observer
export default class Table extends Component<Props, State> {
  static defaultProps = {
    enableSort: true,
  };

  constructor(props: Props) {
    super(props);
    const { columns, sortBy: sortByProp } = props;
    const sortBy = sortByProp || columns[0].id;
    this.state = {
      sortOrder: SORT_ORDERS.DESCENDING,
      sortBy,
    };
  }

  handleSort = (newSortBy: string) => {
    const { sortOrder, sortBy } = this.state;
    let newsortOrder;
    if (sortBy === newSortBy) {
      // on same sort change order
      newsortOrder = sortOrder === 'asc' ? 'desc' : 'asc';
    } else {
      // on new sort instance, order by initial value 'descending'
      newsortOrder = 'desc';
    }

    this.setState({
      sortBy: newSortBy,
      sortOrder: newsortOrder,
    });
  };

  getSortedData = (): Array<any> => {
    const { rows } = this.props;
    const { sortOrder, sortBy } = this.state;
    const isAscending = sortOrder === SORT_ORDERS.ASCENDING;
    const columnsObj = this.getColumnsObj();
    const column = columnsObj[sortBy] || {};
    const { sortValue, type } = column;
    return rows.slice().sort((dataA: any, dataB: any) => {
      const sortDataA = sortValue ? sortValue(dataA[sortBy]) : dataA[sortBy];
      const sortDataB = sortValue ? sortValue(dataB[sortBy]) : dataB[sortBy];
      let comparator = stringComparator;
      if (type === 'date') {
        comparator = dateComparator;
      } else if (type === 'bigNumber') {
        comparator = bigNumberComparator;
      } else if (typeof sortDataA === 'string') {
        comparator = stringComparator;
      } else if (typeof sortDataA === 'number') {
        comparator = numberComparator;
      }
      return comparator ? comparator(sortDataA, sortDataB, isAscending) : 0;
    });
  };

  getColumnsObj = () =>
    reduce(
      this.props.columns,
      (obj, column) => {
        obj[column.id] = column;
        return obj;
      },
      {}
    );

  render() {
    const {
      columns,
      rows,
      className,
      onClickRow,
      onClickCell,
      maxHeight,
      isCompact,
      enableSort,
    } = this.props;
    const { sortOrder, sortBy } = this.state;

    const columnsObj = this.getColumnsObj();

    const componentStyles = classnames(styles.component, className, {
      [styles.compact]: isCompact,
      [styles.sticky]: !!maxHeight,
    });

    const tableData = enableSort ? this.getSortedData() : rows;

    return (
      <div className={componentStyles}>
        <div className={styles.content} style={{ maxHeight }}>
          <table>
            <thead>
              <tr>
                {map(columns, ({ id, title, type }: TableColumn) => {
                  const isSorted = id === sortBy;
                  const sortIconClasses = classnames([
                    styles.sortIcon,
                    isSorted ? styles.sorted : null,
                    isSorted && sortOrder === 'asc' ? styles.ascending : null,
                  ]);
                  return (
                    <th key={id} onClick={() => this.handleSort(id)}>
                      {title}
                      {type !== 'node' && (
                        <SVGInline svg={sortIcon} className={sortIconClasses} />
                      )}
                    </th>
                  );
                })}
              </tr>
            </thead>
            <tbody>
              {map(tableData, (item: TableRow, key) => (
                <tr key={key} onClick={onClickRow}>
                  {map(entries(item), ([columnKey, entry]) => {
                    const column = columnsObj[columnKey] || {};
                    const renderedEntry = column.render
                      ? column.render(entry)
                      : entry;
                    return (
                      <td
                        key={key + columnKey}
                        role="presentation"
                        className={styles.rewardWallet}
                        onClick={onClickCell}
                      >
                        {renderedEntry}
                      </td>
                    );
                  })}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    );
  }
}
