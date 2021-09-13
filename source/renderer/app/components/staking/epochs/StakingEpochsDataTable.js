// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { map } from 'lodash';
import classNames from 'classnames';
import sortIcon from '../../../assets/images/ascending.inline.svg';
import styles from './StakingEpochs.scss';

type TableHeaders = {
  name: string,
  title: string,
};

type Props = {
  tableHeaders: TableHeaders,
  tableBody: Node,
  order: string,
  sortBy: string,
  handleDataSort: Function,
};

@observer
export default class StakingEpochsDataTable extends Component<Props> {
  render() {
    const {
      tableHeaders,
      tableBody,
      order,
      sortBy,
      handleDataSort,
    } = this.props;

    return (
      <table>
        <thead>
          <tr>
            {map(tableHeaders, (tableHeader) => {
              const isSorted = tableHeader.name === sortBy;
              const sortIconClasses = classNames([
                styles.sortIcon,
                isSorted ? styles.sorted : null,
                isSorted && order === 'asc' ? styles.ascending : null,
              ]);

              return (
                <th
                  key={tableHeader.name}
                  onClick={() => handleDataSort(tableHeader.name)}
                >
                  {tableHeader.title}
                  <SVGInline svg={sortIcon} className={sortIconClasses} />
                </th>
              );
            })}
          </tr>
        </thead>
        {tableBody}
      </table>
    );
  }
}
