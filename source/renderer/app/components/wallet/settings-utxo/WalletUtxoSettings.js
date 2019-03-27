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
  Label,
  Bar,
  ResponsiveContainer,
} from 'recharts';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './WalletUtxoSettings.scss';

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

type TickDefaultProps = {
  x: number,
  y: number,
  payload: {
    value: number,
  },
};

type TickCustomProps = {
  className: string,
  prettify?: boolean,
  textAnchor: 'start' | 'end',
};

type TooltipProps = {
  payload: Array<{ payload: any }>,
};

@observer
export default class WalletUtxoSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getPrettyAmount = (amount: number) =>
    amount >= 1000 ? millify(parseInt(amount, 10)) : amount;

  getTooltipContent = (props: TooltipProps) => {
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

  getTick = (
    { x, y, payload: { value } }: TickDefaultProps,
    { className, prettify, textAnchor }: TickCustomProps
  ) => (
    <g transform={`translate(${x},${y})`} className={className}>
      <text textAnchor={textAnchor}>
        {prettify ? this.getPrettyAmount(value) : value}
      </text>
    </g>
  );

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

        <ResponsiveContainer
          height={280}
          className={styles.responsiveContainer}
        >
          <BarChart data={chartData}>
            <CartesianGrid
              horizontal={false}
              vertical={false}
              fill="rgba(68, 91, 124, 0.06)"
            />
            <XAxis
              dataKey="walletAmount"
              interval={0}
              axisLine={false}
              tickLine={false}
              tickFormatter={this.getPrettyAmount}
              tick={props =>
                this.getTick(props, {
                  className: styles.xAxisTick,
                  textAnchor: 'start',
                  prettify: true,
                })
              }
              className={styles.xAxis}
              y={0}
            >
              <Label
                value={intl.formatMessage(messages.labelX)}
                offset={20}
                position="insideBottomRight"
                className={styles.xAxisLabel}
              />
            </XAxis>
            <YAxis
              dataKey="walletUtxosAmount"
              axisLine={false}
              tickLine={false}
              tick={props =>
                this.getTick(props, {
                  className: styles.yAxisTick,
                  textAnchor: 'end',
                })
              }
            >
              <Label
                value={intl.formatMessage(messages.labelY)}
                offset={-20}
                position="insideTopLeft"
                className={styles.yAxisLabel}
              />
            </YAxis>
            <Tooltip content={this.getTooltipContent} />
            <Bar dataKey="walletUtxosAmount" className={styles.bar} />
          </BarChart>
        </ResponsiveContainer>
      </div>
    );
  }
}
