import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTransactionsSearch.scs... Remove this comment to see the full error message
import styles from './WalletTransactionsSearch.scss';

const messages = defineMessages({
  searchHint: {
    id: 'wallet.transactions.search.hint',
    defaultMessage: '!!!Search transaction',
    description: 'Hint in the transactions search box.',
  },
});
type Props = {
  searchTerm: string;
  onChange: (...args: Array<any>) => any;
};

@observer
class WalletTransactionsSearch extends Component<Props> {
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
          placeholder={intl.formatMessage(messages.searchHint)}
          value={searchTerm}
          onChange={onChange}
          skin={InputSkin}
        />
      </div>
    );
  }
}

export default WalletTransactionsSearch;
