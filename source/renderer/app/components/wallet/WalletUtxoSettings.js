// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import styles from './WalletUtxoSettings.scss';
import type { WalletUtxos } from '../../api/wallets/types';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import Wallet from '../../domains/Wallet';
// import {
//   LineChart,
//   YAxis,
//   XAxis,
//   Line,
//   CartesianGrid,
//   Tooltip,
//   Legend,
//   ResponsiveContainer,
// } from 'recharts';

export const messages = defineMessages({
  title: {
    id: 'wallet.settings.utxos.title',
    defaultMessage: '!!!Wallet UTxO distribution',
    description: 'Title for the "Wallet Utxos" screen.',
  },
  description: {
    id: 'wallet.settings.utxos.description',
    defaultMessage:
      '!!!This wallet contains <b>{amount} ADA</b> on <b>{utxos} UTxOs</b> (unspent transaction outputs). Examine the histogram below to see the distribution of UTxOs with different amounts of ada.',
    description: 'Description for the "Wallet Utxos" screen.',
  },
});

type Props = {
  walletUtxos: ?WalletUtxos,
  wallet: Wallet,
};

@observer
export default class WalletUtxoSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { walletUtxos, wallet } = this.props;
    const amount = wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA);
    const utxos = 21;

    if (!walletUtxos) return <div className={styles.component}>Loading...</div>;

    // const { allStakes, histogram, boundType } = walletUtxos;
    return (
      <div className={styles.component}>
        <h1>{intl.formatMessage(messages.title)}</h1>

        <p>
          <FormattedHTMLMessage
            {...messages.description}
            values={{ amount, utxos }}
          />
        </p>

        {/* <pre>
          {JSON.stringify({ allStakes, histogram, boundType }, null, 2)}
        </pre> */}
      </div>
    );
  }
}
