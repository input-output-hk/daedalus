// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import type { Row } from '../types';
import FilterButton from '../FilterButton';
import styles from './SimpleTransactionList.scss';

type Props = {
  renderRow: Row => Node,
  rows: Row[],
  onFilterButtonClick?: Function,
};

type State = {
  isFilterButtonFaded: boolean,
};

@observer
export class SimpleTransactionList extends Component<Props, State> {
  static defaultProps = {
    onOpenExternalLink: () => {},
    onFilterButtonClick: () => null,
  };

  state = {
    isFilterButtonFaded: false,
  };

  onListScroll = (evt: SyntheticEvent<HTMLElement>) => {
    const { scrollTop } = evt.currentTarget;
    if (scrollTop > 10 && !this.state.isFilterButtonFaded) {
      this.setState({ isFilterButtonFaded: true });
    } else if (scrollTop <= 10 && this.state.isFilterButtonFaded) {
      this.setState({ isFilterButtonFaded: false });
    }
  };

  render() {
    const { rows, renderRow, onFilterButtonClick } = this.props;
    const { isFilterButtonFaded } = this.state;
    return (
      <div className={styles.component} onScroll={this.onListScroll}>
        <FilterButton
          faded={isFilterButtonFaded}
          onClick={onFilterButtonClick}
        />
        {rows.map((row, index) => (
          // eslint-disable-next-line react/jsx-no-bind
          // eslint-disable-next-line react/no-array-index-key
          <div key={index}>{renderRow(row)}</div>
        ))}
      </div>
    );
  }
}
