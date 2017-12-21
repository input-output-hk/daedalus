// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletAccountsList.scss';
import Account from './Account';
import WalletAccount from '../../../domain/WalletAccount';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  accountsList: {
    id: 'wallet.summary.page.accountsListLabel',
    defaultMessage: '!!!Accounts list',
    description: 'Label for the "Accounts list" headline on the wallet summary page.',
  },
  addAccount: {
    id: 'wallet.summary.page.addAccountButtonLabel',
    defaultMessage: '!!!Add account',
    description: 'Label for the "Add account" button on the wallet summary page.',
  },
});

type Props = {
  accounts: Array<WalletAccount>,
  walletId: string,
};

@observer
export default class WalletAccountsList extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { accounts, walletId } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.headline}>
          <div>{intl.formatMessage(messages.accountsList)}</div>
          <div>+ {intl.formatMessage(messages.addAccount)}</div>
        </div>
        <div className={styles.list}>
          {!accounts.length ? <LoadingSpinner /> : (
            accounts.map((account, index) => (
              <div key={walletId + '-' + account.id} className={styles.item}>
                <Account data={account} index={index} />
              </div>
            ))
          )}
        </div>
      </div>
    );
  }

}
