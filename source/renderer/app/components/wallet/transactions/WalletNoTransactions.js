// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import FilterButton from './FilterButton';
import styles from './WalletNoTransactions.scss';

type Props = {
  label: string,
  onFilterButtonClick?: Function,
};

@observer
export default class WalletNoTransactions extends Component<Props> {
  render() {
    const { onFilterButtonClick, label } = this.props;

    return (
      <div className={styles.component}>
        {onFilterButtonClick && (
          <FilterButton faded={false} onClick={onFilterButtonClick} />
        )}
        <div className={styles.label}>{label}</div>
      </div>
    );
  }
}
