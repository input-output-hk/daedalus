import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import type { Row } from '../types';
import type { ScrollContextType } from '../WalletTransactionsList';
import { WalletTransactionsListScrollContext } from '../WalletTransactionsList';
import styles from './SimpleTransactionList.scss';

type Props = {
  renderRow: (arg0: Row) => Node;
  rows: Row[];
};

@observer
class SimpleTransactionList extends Component<Props> {
  static defaultProps = {
    onOpenExternalLink: () => {},
  };
  root: HTMLDivElement | null | undefined;

  revealTransaction = (transactionId: string): boolean => {
    if (!this.root) return false;
    this.forceUpdate(() => {
      const row = document.getElementById(`tx-${transactionId}`);
      if (
        row instanceof HTMLElement &&
        this.root?.contains(row) &&
        row.scrollIntoView
      ) {
        row.scrollIntoView({ block: 'center' });
      }
    });
    return true;
  };
  onListScroll = (
    context: ScrollContextType,
    evt: React.SyntheticEvent<HTMLElement>
  ) => {
    const { scrollTop } = evt.currentTarget;

    if (scrollTop > 10) {
      context.setIsScrolling(true);
    } else {
      context.setIsScrolling(false);
    }
  };

  render() {
    const { rows, renderRow } = this.props;
    return (
      <WalletTransactionsListScrollContext.Consumer>
        {(context) => (
          <div
            className={styles.component}
            ref={(element) => {
              this.root = element;
            }}
            onScroll={(evt) => this.onListScroll(context, evt)}
          >
            {rows.map((
              row,
              index // eslint-disable-next-line react/no-array-index-key
            ) => (
              <div key={`simple-transaction-list-row-${index}`}>
                {renderRow(row)}
              </div>
            ))}
          </div>
        )}
      </WalletTransactionsListScrollContext.Consumer>
    );
  }
}

export { SimpleTransactionList };
