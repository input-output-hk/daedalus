// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
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
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import styles from './WalletUtxoSettings.scss';
import chartStyles from './WalletUtxoSettingsStyles.js';

export const messages = defineMessages({
  title: {
    id: 'wallet.settings.utxos.title',
    defaultMessage: '!!!Wallet UTxO distribution',
    description: 'Title for the "Wallet Utxos" screen.',
  },
  description: {
    id: 'wallet.settings.utxos.description',
    defaultMessage:
      '!!!This wallet contains <b>{formattedWalletAmount} ADA</b> on <b>{walletUtxosAmount} UTxOs</b> (unspent transaction outputs). Examine the histogram below to see the distribution of UTxOs with different amounts of ada.',
    description: 'Description for the "Wallet Utxos" screen.',
  },
});

type Props = {
  walletAmount: BigNumber,
  walletUtxosAmount: number,
  chartData: Array<any>,
};

const Cursor = ({ payload }) => {
  console.log('payload', payload[0].payload);
  return <div>HEY!</div>;
};

@observer
export default class WalletUtxoSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { walletAmount, walletUtxosAmount, chartData } = this.props;
    const formattedWalletAmount = walletAmount.toFormat(DECIMAL_PLACES_IN_ADA);
    return (
      <div className={styles.component}>
        <h1>{intl.formatMessage(messages.title)}</h1>

        <p>
          <FormattedHTMLMessage
            {...messages.description}
            values={{ formattedWalletAmount, walletUtxosAmount }}
          />
        </p>

        <ResponsiveContainer width="100%" height={280}>
          <BarChart height={280} data={chartData}>
            <CartesianGrid
              horizontal={false}
              vertical={false}
              fill={chartStyles.cartesianGridBackground.fill}
            />
            <XAxis
              dataKey="walletAmount"
              interval={0}
              axisLine={false}
              tickLine={false}
              tick={chartStyles.xAxis}
            />
            <YAxis
              dataKey="walletUtxosAmount"
              axisLine={false}
              tickLine={false}
              tick={chartStyles.yAxis}
            />
            <Tooltip {...chartStyles.tooltip} cursor={<Cursor />} />
            <Bar dataKey="walletUtxosAmount" fill={chartStyles.bar.fill} />
          </BarChart>
        </ResponsiveContainer>
      </div>
    );
  }
}
