// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import { get } from 'lodash';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
// import { formattedUtxosPrettyAmount } from '../../utils/formatters';
import millify from 'millify';
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
  labelX: {
    id: 'wallet.settings.utxos.labelX',
    defaultMessage: '!!!amount',
    description: 'Label X for the "Wallet Utxos" screen.',
  },
  labelY: {
    id: 'wallet.settings.utxos.labelY',
    defaultMessage: '!!!Nº UTxO',
    description: 'Label Y for the "Wallet Utxos" screen.',
  },
});

type Props = {
  walletAmount: BigNumber,
  walletUtxosAmount: number,
  chartData: Array<any>,
};

@observer
export default class WalletUtxoSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getPrettyAmount = (amount: number) =>
    amount >= 1000 ? millify(parseInt(amount, 10)) : amount;

  getTooltipContent = props => {
    const { walletAmount, walletUtxosAmount } = get(
      props,
      'payload[0].payload',
      {}
    );
    const previousWalletAmount =
      walletAmount === 45000000000 ? 10000000000 : walletAmount / 10;
    const prettyWalletAmount = this.getPrettyAmount(walletAmount);
    const prettyPreviousWalletAmount = this.getPrettyAmount(
      previousWalletAmount
    );

    return (
      <p>
        {walletUtxosAmount} UTxOs containing <br />
        between {prettyPreviousWalletAmount} and {prettyWalletAmount} ADA
      </p>
    );
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
          <BarChart data={chartData}>
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
              tick={chartStyles.xAxis.tick}
              tickFormatter={this.getPrettyAmount}
              label={{
                ...chartStyles.xAxis.label,
                value: intl.formatMessage(messages.labelX),
              }}
              y={0}
            />
            <YAxis
              dataKey="walletUtxosAmount"
              axisLine={false}
              tickLine={false}
              tick={chartStyles.yAxis.tick}
              label={{
                ...chartStyles.yAxis.label,
                value: intl.formatMessage(messages.labelY),
              }}
            />
            <Tooltip
              {...chartStyles.tooltip}
              content={this.getTooltipContent}
            />
            <Bar dataKey="walletUtxosAmount" fill={chartStyles.bar.fill} />
          </BarChart>
        </ResponsiveContainer>
      </div>
    );
  }
}
