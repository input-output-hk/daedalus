// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import styles from './WalletTransactionsList.scss';
import Transaction from './Transaction';
import WalletTransaction from '../../../domains/WalletTransaction';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { WalletAssuranceMode } from '../../../api/wallets/types';

const messages = defineMessages({
  today: {
    id: 'wallet.summary.page.todayLabel',
    defaultMessage: '!!!Today',
    description: 'Label for the "Today" label on the wallet summary page.',
  },
  yesterday: {
    id: 'wallet.summary.page.yesterdayLabel',
    defaultMessage: '!!!Yesterday',
    description: 'Label for the "Yesterday" label on the wallet summary page.',
  },
  showMoreTransactionsButtonLabel: {
    id: 'wallet.summary.page.showMoreTransactionsButtonLabel',
    defaultMessage: '!!!Show more transactions',
    description: 'Label for the "Show more transactions" button on the wallet summary page.',
  },
  syncingTransactionsMessage: {
    id: 'wallet.summary.page.syncingTransactionsMessage',
    defaultMessage: '!!!Your transaction history for this wallet is being synced with the blockchain.',
    description: 'Syncing transactions message on async wallet restore.',
  },
});

const dateFormat = 'YYYY-MM-DD';

type Props = {
  transactions: Array<WalletTransaction>,
  isLoadingTransactions: boolean,
  isRestoreActive: boolean,
  hasMoreToLoad: boolean,
  assuranceMode: WalletAssuranceMode,
  walletId: string,
  formattedWalletAmount: Function,
  showMoreTransactionsButton?: boolean,
  onShowMoreTransactions?: Function,
  onOpenExternalLink?: Function,
};

type TransactionsGroup = { date: moment.Moment, transactions: Array<WalletTransaction>};

@observer
export default class WalletTransactionsList extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
    // Localized dateFormat:
    // English - MM/DD/YYYY
    // Japanese - YYYY/MM/DD
  }

  list: HTMLElement;
  loadingSpinner: ?LoadingSpinner;
  localizedDateFormat: 'MM/DD/YYYY';

  groupTransactionsByDay(transactions: Array<WalletTransaction>): Array<TransactionsGroup> {
    const groups: Array<TransactionsGroup> = [];
    for (const transaction of transactions) {
      const date = moment(transaction.date);
      let group = groups.find((g) => g.date.format(dateFormat) === date.format(dateFormat));
      if (!group) {
        group = { date, transactions: [] };
        groups.push(group);
      }
      group.transactions.push(transaction);
    }
    return groups.sort((a: TransactionsGroup, b: TransactionsGroup) => (
      b.date.valueOf() - a.date.valueOf()
    ));
  }

  isSpinnerVisible() {
    const spinner = this.loadingSpinner;
    if (spinner == null || spinner.root == null) return false;
    const spinnerRect = spinner.root.getBoundingClientRect();
    const clientHeight = document.documentElement ? document.documentElement.clientHeight : 0;
    const windowHeight = window.innerHeight;
    const viewHeight = Math.max(clientHeight, windowHeight);
    return !(spinnerRect.bottom < 0 || spinnerRect.top - viewHeight >= 0);
  }

  localizedDate(date: string) {
    const { intl } = this.context;
    const today = moment().format(dateFormat);
    if (date === today) return intl.formatMessage(messages.today);
    const yesterday = moment().subtract(1, 'days').format(dateFormat);
    if (date === yesterday) return intl.formatMessage(messages.yesterday);
    return moment(date).format(this.localizedDateFormat);
  }

  render() {
    const {
      transactions,
      isLoadingTransactions,
      hasMoreToLoad,
      assuranceMode,
      walletId,
      formattedWalletAmount,
      onOpenExternalLink,
      showMoreTransactionsButton,
      isRestoreActive,
    } = this.props;

    const { intl } = this.context;

    const transactionsGroups = this.groupTransactionsByDay(transactions);

    const loadingSpinner = (isLoadingTransactions || hasMoreToLoad) && !isRestoreActive ? (
      <LoadingSpinner ref={(component) => { this.loadingSpinner = component; }} />
    ) : null;

    const syncingTransactionsSpinner = isRestoreActive ? (
      <div className={styles.syncingTransactionsWrapper}>
        <LoadingSpinner big />
        <p className={styles.syncingTransactionsText}>
          {intl.formatMessage(messages.syncingTransactionsMessage)}
        </p>
      </div>
    ) : null;

    const buttonClasses = classnames([
      'primary',
      styles.showMoreTransactionsButton,
    ]);

    return (
      <div className={styles.component}>
        {syncingTransactionsSpinner}

        {transactionsGroups.map((group, groupIndex) => (
          <div className={styles.group} key={walletId + '-' + groupIndex}>
            <div className={styles.groupDate}>{this.localizedDate(group.date)}</div>
            <div className={styles.list}>
              {group.transactions.map((transaction, transactionIndex) => (
                <div key={`${walletId}-${transaction.id}-${transaction.type}`}>
                  <Transaction
                    data={transaction}
                    isRestoreActive={isRestoreActive}
                    isLastInList={transactionIndex === group.transactions.length - 1}
                    state={transaction.state}
                    assuranceLevel={transaction.getAssuranceLevelForMode(assuranceMode)}
                    formattedWalletAmount={formattedWalletAmount}
                    onOpenExternalLink={onOpenExternalLink}
                  />
                </div>
              ))}
            </div>
          </div>
        ))}

        {loadingSpinner}

        {showMoreTransactionsButton &&
          <Button
            className={buttonClasses}
            label={intl.formatMessage(messages.showMoreTransactionsButtonLabel)}
            onClick={this.onShowMoreTransactions.bind(this, walletId)}
            skin={ButtonSkin}
          />
        }
      </div>
    );
  }

  onShowMoreTransactions = (walletId: string) => {
    if (this.props.onShowMoreTransactions) {
      this.props.onShowMoreTransactions(walletId);
    }
  };
}
