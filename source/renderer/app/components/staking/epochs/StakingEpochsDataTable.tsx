import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { map } from 'lodash';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ascendi... Remove this comment to see the full error message
import sortIcon from '../../../assets/images/ascending.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingEpochs.scss' or its c... Remove this comment to see the full error message
import styles from './StakingEpochs.scss';

type TableHeaders = {
  name: string;
  title: string;
};
type Props = {
  tableHeaders: TableHeaders;
  tableBody: Node;
  order: string;
  sortBy: string;
  handleDataSort: (...args: Array<any>) => any;
};

@observer
class StakingEpochsDataTable extends Component<Props> {
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
              // @ts-ignore ts-migrate(2339) FIXME: Property 'name' does not exist on type 'string'.
              const isSorted = tableHeader.name === sortBy;
              const sortIconClasses = classNames([
                styles.sortIcon,
                isSorted ? styles.sorted : null,
                isSorted && order === 'asc' ? styles.ascending : null,
              ]);
              return (
                <th
                  // @ts-ignore ts-migrate(2339) FIXME: Property 'name' does not exist on type 'string'.
                  key={tableHeader.name}
                  // @ts-ignore ts-migrate(2339) FIXME: Property 'name' does not exist on type 'string'.
                  onClick={() => handleDataSort(tableHeader.name)}
                >
                  {/* @ts-ignore ts-migrate(2339) FIXME: Property 'title' does not exist on type 'string'. */}
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

export default StakingEpochsDataTable;
