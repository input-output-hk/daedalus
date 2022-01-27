import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import StakingChartTooltipItem from './StakingChartTooltipItem';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingChartTooltip.scss' or... Remove this comment to see the full error message
import styles from './StakingChartTooltip.scss';

const dateFormat = 'YYYY-MM-DD-HH:mm';
const messages = defineMessages({
  slot: {
    id: 'staking.chart.tooltip.slot.label',
    defaultMessage: '!!!slot',
    description: '"slot" label on staking chart tooltip.',
  },
  transactions: {
    id: 'staking.chart.tooltip.transactions.label',
    defaultMessage: '!!!transactions',
    description: '"transactions" label on staking chart tooltip.',
  },
  mpcPhase: {
    id: 'staking.chart.tooltip.mpc.phase.label',
    defaultMessage: '!!!MPC phase',
    description: '"MPC phase" label on staking chart tooltip.',
  },
  commitments: {
    id: 'staking.chart.tooltip.commitments.label',
    defaultMessage: '!!!commitments',
    description: '"commitments" label on staking chart tooltip.',
  },
  openings: {
    id: 'staking.chart.tooltip.openings.label',
    defaultMessage: '!!!openings',
    description: '"openings" label on staking chart tooltip.',
  },
  shares: {
    id: 'staking.chart.tooltip.shares.label',
    defaultMessage: '!!!shares',
    description: '"shares" label on staking chart tooltip.',
  },
});
type Props = {
  slot: number;
  time: Date;
  hash: string;
  numberOfTransactions: number;
  mpcPhase: string;
  commitments: string;
  openings: string;
  shares: string;
};

@observer
class StakingChartTooltip extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      slot,
      time,
      hash,
      numberOfTransactions,
      mpcPhase,
      commitments,
      openings,
      shares,
    } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.slot}>
          {`#${slot}`} {intl.formatMessage(messages.slot)}
        </div>
        <div className={styles.date}>{moment(time).format(dateFormat)}</div>
        <div className={styles.hash}>{hash}</div>
        <StakingChartTooltipItem
          key="transactions"
          label={intl.formatMessage(messages.transactions)}
          value={numberOfTransactions.toString()}
        />
        <StakingChartTooltipItem
          key="mpcPhase"
          label={intl.formatMessage(messages.mpcPhase)}
          value={mpcPhase}
        />
        <StakingChartTooltipItem
          key="commitments"
          label={intl.formatMessage(messages.commitments)}
          value={commitments}
        />
        <StakingChartTooltipItem
          key="openings"
          label={intl.formatMessage(messages.openings)}
          value={openings}
        />
        <StakingChartTooltipItem
          key="shares"
          label={intl.formatMessage(messages.shares)}
          value={shares}
        />
      </div>
    );
  }
}

export default StakingChartTooltip;
