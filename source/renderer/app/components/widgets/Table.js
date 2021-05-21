// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import { get, map } from 'lodash';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import LoadingSpinner from './LoadingSpinner';
import sortIcon from '../../assets/images/ascending.inline.svg';
import styles from './Table.scss';

type ColumnAccessor = string;

type ColumnItem = {
  Header: string | number | Node,
  acessor: ColumnAccessor,
};

type DataItem = {
  [key: ColumnAccessor]: string | number | Node,
};

type Props = {
  columns: Array<ColumnItem>,
  data: Array<DataItem>,
  className?: string,
  isLoading?: boolean,
  onClickRow?: Function,
  onClickCell?: Function,
};

@observer
export default class StakingRewardsForIncentivizedTestnet extends Component<Props> {
  static defaultProps = {
    isLoading: false,
  };

  handleRewardsSort = (acessor: ColumnAccessor) => {
    console.log('acessor', acessor);
  };

  render() {
    const {
      columns,
      data,
      className,
      isLoading,
      onClickRow,
      onClickCell,
    } = this.props;

    const componentStyles = classnames([styles.component, className]);

    return (
      <table className={componentStyles}>
        <thead>
          <tr>
            {map(columns, ({ acessor, Header }: ColumnItem) => {
              return (
                <th
                  key={acessor}
                  onClick={() => this.handleRewardsSort(acessor)}
                >
                  {Header}
                </th>
              );
            })}
          </tr>
        </thead>
        <tbody>
          {map(data, (item: DataItem, key) => {
            console.log('item', item);
            return (
              <tr key={key} onClick={onClickRow}>
                <td className={styles.rewardWallet}>Test</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    );
  }
}
