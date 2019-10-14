// @flow
import React, { Component, Fragment } from 'react';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
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
import BorderedBox from '../../widgets/BorderedBox';
import Tick from './WalletUtxoTick';
import CustomTooltip from './WalletUtxoTooltip';
import Cursor from './WalletUtxoCursor';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './WalletUtxo.scss';
import type { TickProps } from './WalletUtxoTick';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';

export const messages = defineMessages({
  title: {
    id: 'wallet.settings.utxos.title',
    defaultMessage: '!!!Wallet UTXO distribution',
    description: 'Title for the "Wallet Utxos" screen.',
  },
  description: {
    id: 'wallet.settings.utxos.description',
    defaultMessage:
      '!!!This wallet contains <b>{formattedWalletAmount} ADA</b> on <b>{walletUtxosAmount} UTXOs</b> (unspent transaction outputs). Examine the histogram below to see the distribution of UTXOs with different amounts of ada.',
    description: 'Description for the "Wallet Utxos" screen.',
  },
  emptyWallet: {
    id: 'wallet.settings.utxos.emptyWallet',
    defaultMessage:
      '!!!This wallet is empty so it does not contain any UTXOs (unspent transaction outputs).',
    description: 'Empty wallet description for the "Wallet Utxos" screen.',
  },
  findOutMoreLink: {
    id: 'wallet.settings.utxos.findOutMoreLink',
    defaultMessage: '!!!Find out more',
    description: '"Find out more" link on the "Wallet Utxos" screen.',
  },
  findOutMoreLinkUrl: {
    id: 'wallet.settings.utxos.findOutMoreLinkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360034118013',
    description: '"Find out more" link URL on the "Wallet Utxos" screen.',
  },
  labelX: {
    id: 'wallet.settings.utxos.labelX',
    defaultMessage: '!!!amount',
    description: 'Label X for the "Wallet Utxos" screen.',
  },
  labelY: {
    id: 'wallet.settings.utxos.labelY',
    defaultMessage: '!!!Nº UTXO',
    description: 'Label Y for the "Wallet Utxos" screen.',
  },
  pendingTransactions: {
    id: 'wallet.settings.utxos.pendingTransactions',
    defaultMessage:
      '!!!<b>Pending transactions</b> may affect the accuracy of data presented here. <br /> You have <b>{pendingTxnsCount}</b> pending transaction{txnsPlural}.',
    description:
      'Number of pending transactions for the "Wallet Utxos" screen.',
  },
});

type Props = {
  walletAmount: BigNumber,
  walletUtxosAmount: number,
  chartData: Array<any>,
  onExternalLinkClick: Function,
  pendingTxnsCount: number,
};

type State = {
  isHoveringChart: boolean,
};

export default class WalletUtxo extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isHoveringChart: false,
  };

  shouldComponentUpdate() {
    return !this.state.isHoveringChart;
  }

  renderPendingTxns = (pendingTxnsCount: number) => {
    const txnsPlural = (!pendingTxnsCount || pendingTxnsCount > 1) && 's';
    return (
      <div className={styles.pendingTxnsWrapper}>
        <div>
          <p>
            <FormattedHTMLMessage
              {...messages.pendingTransactions}
              values={{
                pendingTxnsCount,
                txnsPlural,
              }}
            />
          </p>
        </div>
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const {
      walletAmount,
      walletUtxosAmount,
      chartData,
      onExternalLinkClick,
      pendingTxnsCount,
    } = this.props;
    const formattedWalletAmount = walletAmount.toFormat(DECIMAL_PLACES_IN_ADA);
    const isEmpty = walletUtxosAmount === 0;
    const componentStyles = classnames([
      styles.component,
      isEmpty ? styles.isEmpty : null,
    ]);

    const findOutMoreLinkUrl = intl.formatMessage(messages.findOutMoreLinkUrl);
    const findOutMoreLink = (
      <a
        className={styles.externalLink}
        href={findOutMoreLinkUrl}
        onClick={event => onExternalLinkClick(findOutMoreLinkUrl, event)}
      >
        {intl.formatMessage(messages.findOutMoreLink)}
        <SVGInline svg={externalLinkIcon} />
      </a>
    );

    return (
      <div className={componentStyles}>
        <BorderedBox>
          <div
            className={styles.container}
            onMouseEnter={() => this.setState({ isHoveringChart: true })}
            onMouseLeave={() => this.setState({ isHoveringChart: false })}
          >
            <h1>{intl.formatMessage(messages.title)}</h1>

            {!isEmpty ? (
              <Fragment>
                <p>
                  <FormattedHTMLMessage
                    {...messages.description}
                    values={{
                      formattedWalletAmount,
                      walletUtxosAmount,
                    }}
                  />{' '}
                  {findOutMoreLink}
                </p>

                <div className={styles.responsiveContainerWrapper}>
                  <ResponsiveContainer
                    height={280}
                    className={styles.responsiveContainer}
                  >
                    <BarChart data={chartData} barSize={23}>
                      <CartesianGrid
                        className={styles.cartesianGrid}
                        horizontal={false}
                        vertical={false}
                        y={-10}
                        height={255}
                        fill="transparent"
                      />
                      <XAxis
                        dataKey="walletAmount"
                        interval={0}
                        axisLine={false}
                        tickLine={false}
                        tick={(props: TickProps) => (
                          <Tick {...props} textAnchor="start" vertical />
                        )}
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
                        allowDecimals={false}
                        domain={[0, 'dataMax']}
                        tick={(props: TickProps) => (
                          <Tick {...props} textAnchor="end" />
                        )}
                      >
                        <Label
                          value={intl.formatMessage(messages.labelY)}
                          offset={0}
                          position="insideTopLeft"
                          className={styles.yAxisLabel}
                        />
                      </YAxis>
                      <Tooltip
                        cursor={<Cursor offsetWidth={28} />}
                        isAnimationActive={false}
                        content={<CustomTooltip />}
                      />
                      <Bar dataKey="walletUtxosAmount" className={styles.bar} />
                    </BarChart>
                  </ResponsiveContainer>
                  {this.renderPendingTxns(pendingTxnsCount)}
                </div>
              </Fragment>
            ) : (
              <p>{intl.formatMessage(messages.emptyWallet)}</p>
            )}
          </div>
        </BorderedBox>
      </div>
    );
  }
}
