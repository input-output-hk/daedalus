// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import {
  BarChart,
  CartesianGrid,
  XAxis,
  YAxis,
  Tooltip,
  Bar,
  ResponsiveContainer,
} from 'recharts';
import styles from './WalletUtxoSettings.scss';
import chartStyles from './WalletUtxoSettingsStyles.js';
import type { WalletUtxos } from '../../api/wallets/types';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import Wallet from '../../domains/Wallet';
import { formattedPrettyAmount } from '../../utils/formatters';

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

  get data() {
    const { histogram } = this.props.walletUtxos || { histogram: {} };

    return Object.entries(histogram)
      .sort()
      .map<any>(([amount, utxos]) => ({
        amount: formattedPrettyAmount(amount),
        utxos,
      }));
  }

  render() {
    const { intl } = this.context;
    const { wallet } = this.props;
    const amount = wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA);
    const utxos = 21;

    return (
      <div className={styles.component}>
        <h1>{intl.formatMessage(messages.title)}</h1>

        <p>
          <FormattedHTMLMessage
            {...messages.description}
            values={{ amount, utxos }}
          />
        </p>

        <ResponsiveContainer width="100%" height={280}>
          <BarChart width="100%" height={280} data={this.data}>
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis
              dataKey="amount"
              interval={0}
              axisLine={false}
              tickLine={false}
              tick={chartStyles.xAxis}
            />
            <YAxis
              dataKey="utxos"
              axisLine={false}
              tickLine={false}
              tick={chartStyles.yAxis}
            />
            <Tooltip />
            <Bar dataKey="utxos" fill="#445b7c" />
          </BarChart>
        </ResponsiveContainer>
      </div>
    );
  }
}
