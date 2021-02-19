// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { /* defineMessages, */ intlShape } from 'react-intl';
// import globalMessages from '../../../i18n/global-messages';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummaryNoTokens.scss';

/* const messages = defineMessages({
  transactionsLabel: {
    id: 'wallet.summary.header.transactionsLabel',
    defaultMessage: '!!!Number of transactions',
    description: '"Number of transactions" label on Wallet summary header page',
  },
  pendingTransactionsLabel: {
    id: 'wallet.summary.header.pendingTransactionsLabel',
    defaultMessage: '!!!Number of pending transactions',
    description:
      '"Number of pending transactions" label on Wallet summary header page',
  },
}); */

type Props = {
  // onExternalLinkClick: Function,
};

@observer
export default class WalletSummaryNoTokens extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    /* const {
      onExternalLinkClick,
    } = this.props;
    const { intl } = this.context; */

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.noTokensContainer} />
        </BorderedBox>
      </div>
    );
  }
}
