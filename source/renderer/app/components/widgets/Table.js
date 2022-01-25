// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map, reduce } from 'lodash';
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

type SortDirection = 'asc' | 'desc';

const SORT_DIRECTIONS = {
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
  maxHeight?: number,
  isCompact?: boolean,
  initialSortBy?: string,
  initialSortDirection?: SortDirection,
  rowClassname?: Function,
};

type State = {
  sortDirection: string,
  sortBy: string,
};

@observer
export default class Table extends Component<Props, State> {
  static defaultProps = {
    enableSort: true,
  };

  constructor(props: Props) {
    super(props);
    const { columns, initialSortBy, initialSortDirection } = props;
    const sortBy = initialSortBy || columns[0].id;
    const sortDirection = initialSortDirection || SORT_DIRECTIONS.DESCENDING;
    this.state = {
      sortDirection,
      sortBy,
    };
  }

  handleSort = (newSortBy: string) => {
    const { initialSortDirection } = this.props;
    const { sortDirection, sortBy } = this.state;
    let newsortDirection;
    if (sortBy === newSortBy) {
      // on same sort change direction
      newsortDirection =
        sortDirection === SORT_DIRECTIONS.ASCENDING
          ? SORT_DIRECTIONS.DESCENDING
          : SORT_DIRECTIONS.ASCENDING;
    } else {
      // on new sort instance, direction by initial value 'descending'
      newsortDirection = initialSortDirection || SORT_DIRECTIONS.DESCENDING;
    }

    this.setState({
      sortBy: newSortBy,
      sortDirection: newsortDirection,
    });
  };

  getSortedData = (): Array<any> => {
    const { rows } = this.props;
    const { sortDirection, sortBy } = this.state;
    const isAscending = sortDirection === SORT_DIRECTIONS.ASCENDING;
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

  renderColumns = () => {
    const { columns } = this.props;
    const { sortDirection, sortBy } = this.state;
    return map(columns, ({ id, title, type }: TableColumn) => {
      const isSorted = id === sortBy;
      const sortIconClasses = classnames([
        styles.sortIcon,
        isSorted ? styles.sorted : null,
        isSorted && sortDirection === 'asc' ? styles.ascending : null,
      ]);
      return (
        <th className={id} key={id} onClick={() => this.handleSort(id)}>
          {title}
          {type !== 'node' && (
            <SVGInline svg={sortIcon} className={sortIconClasses} />
          )}
        </th>
      );
    });
  };

  renderRows = () => {
    const { rows, columns, onClickRow, enableSort, rowClassname } = this.props;
    const tableData = enableSort ? this.getSortedData() : rows;
    return map(tableData, (item: TableRow, key) => (
      <tr
        className={rowClassname ? rowClassname(item) : null}
        key={key}
        onClick={onClickRow}
      >
        {map(columns, (column) => this.renderRow(column, item, key))}
      </tr>
    ));
  };

  renderRow = (column: TableColumn, row: TableRow, key: number) => {
    const { onClickCell } = this.props;
    const { id: columnId, render } = column;
    const rawValue = row[columnId];
    const renderedValue = render ? render(rawValue, row) : rawValue;
    return (
      <td
        key={key + columnId}
        role="presentation"
        className={columnId}
        onClick={onClickCell}
      >
        {renderedValue}
      </td>
    );
  };

  render() {
    const { className, maxHeight, isCompact } = this.props;

    const componentStyles = classnames(
      styles.component,
      {
        [styles.compact]: isCompact,
        [styles.sticky]: !!maxHeight,
      },
      className
    );

    const columns = this.renderColumns();
    const rows = this.renderRows();

    return (
      <div className={componentStyles}>
        <div className={styles.content} style={{ maxHeight }}>
          <table>
            <thead>
              <tr>{columns}</tr>
            </thead>
            <tbody>{rows}</tbody>
          </table>
        </div>
      </div>
    );
  }
}
