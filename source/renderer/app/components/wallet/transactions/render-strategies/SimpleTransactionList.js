// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { uniqueId } from 'lodash';
import type { Row } from '../types';
import type { ScrollContextType } from '../WalletTransactionsList';
import { WalletTransactionsListScrollContext } from '../WalletTransactionsList';
import styles from './SimpleTransactionList.scss';

type Props = {
  renderRow: Row => Node,
  rows: Row[],
};

@observer
export class SimpleTransactionList extends Component<Props> {
  static defaultProps = {
    onOpenExternalLink: () => {},
  };

  onListScroll = (
    context: ScrollContextType,
    evt: SyntheticEvent<HTMLElement>
  ) => {
    const { scrollTop } = evt.currentTarget;
    if (scrollTop > 10) {
      context.setFilterButtonFaded(true);
    } else {
      context.setFilterButtonFaded(false);
    }
  };

  render() {
    const { rows, renderRow } = this.props;

    return (
      <WalletTransactionsListScrollContext.Consumer>
        {context => (
          <div
            className={styles.component}
            onScroll={evt => this.onListScroll(context, evt)}
          >
            {rows.map(row => (
              <div key={uniqueId()}>{renderRow(row)}</div>
            ))}
          </div>
        )}
      </WalletTransactionsListScrollContext.Consumer>
    );
  }
}
