import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Input from 'react-toolbox/lib/input/Input';
import styles from './WalletTransactionsSearch.scss';

const messages = defineMessages({
  searchHint: {
    id: 'wallet.transactions.search.hint',
    defaultMessage: '!!!Search transaction',
    description: 'Hint in the transactions search box.'
  },
});

@observer
export default class WalletTransactionsSearch extends Component {

  props: {
    searchTerm: string,
    onChange: Function,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { searchTerm, onChange } = this.props;
    return (
      <div className={styles.component}>
        <Input
          className={styles.input}
          type="text"
          hint={intl.formatMessage(messages.searchHint)}
          value={searchTerm}
          onChange={onChange}
        />
      </div>
    );
  }
}
