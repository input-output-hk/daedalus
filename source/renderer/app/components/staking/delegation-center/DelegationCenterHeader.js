// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import styles from './DelegationCenterHeader.scss';
import CountdownWidget from '../../widgets/CountdownWidget';
import humanizeDurationByLocale from '../../../utils/humanizeDurationByLocale';

import type {
  NextEpoch,
  TipInfo,
  FutureEpoch,
} from '../../../api/network/types';
import {
  SLOTS_TOTAL,
  EPOCH_COUNTDOWN_INTERVAL,
} from '../../../config/epochsConfig';
import { generateFieldPanel } from './helpers';

const messages = defineMessages({
  epoch: {
    id: 'staking.delegationCenter.epoch',
    defaultMessage: '!!!Epoch',
    description: 'Headline for the Delegation center.',
  },
  currentSlot: {
    id: 'staking.delegationCenter.currentSlot',
    defaultMessage: '!!!Current slot',
    description: 'Headline for the Delegation center.',
  },
  totalSlots: {
    id: 'staking.delegationCenter.totalSlots',
    defaultMessage: '!!!Total slots',
    description: 'Headline for the Delegation center.',
  },
  headingLeft: {
    id: 'staking.delegationCenter.headingLeft',
    defaultMessage: '!!!Cardano epoch {nextEpochNumber} starts in',
    description: 'Headline for the Delegation center.',
  },
  headingRight: {
    id: 'staking.delegationCenter.headingRight',
    defaultMessage: '!!!Current Cardano epoch',
    description: 'Headline for the Delegation center.',
  },
  description: {
    id: 'staking.delegationCenter.description',
    defaultMessage:
      '!!!Changes to delegation preferences will take effect after the next two Cardano epochs have completed. Epochs on the Incentivized Testnet last one day. Any changes made now will take effect in {timeUntilFutureEpoch}.',
    description: 'Delegation description for the Delegation center.',
  },
});

type Props = {
  networkTip: ?TipInfo,
  nextEpoch: ?NextEpoch,
  futureEpoch: ?FutureEpoch,
  currentLocale: string,
};
type State = { timeUntilFutureEpoch: number };

@observer
export default class DelegationCenterHeader extends Component<Props, State> {
  intervalHandler: ?IntervalID = null;
  state = { timeUntilFutureEpoch: 0 };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidMount() {
    this.updateTimeUntilFutureEpoch();
    this.intervalHandler = setInterval(
      () => this.updateTimeUntilFutureEpoch(),
      EPOCH_COUNTDOWN_INTERVAL
    );
  }

  updateTimeUntilFutureEpoch = () => {
    const { futureEpoch } = this.props;
    if (!futureEpoch) return;
    const { epochStart } = futureEpoch;
    if (epochStart) {
      const timeUntilFutureEpoch = Math.max(
        0,
        new Date(epochStart).getTime() - new Date().getTime()
      );
      this.setState({ timeUntilFutureEpoch });
    }
  };

  componentWillUnmount() {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
    }
  }

  generateCurrentEpochPanels = (
    epoch: number,
    slots: number,
    totalSlots: number
  ) => {
    const { intl } = this.context;
    const epochLabel = intl.formatMessage(messages.epoch);
    const currentSlotLabel = intl.formatMessage(messages.currentSlot);
    const totalSlotsLabel = intl.formatMessage(messages.totalSlots);
    const labels: Array<string> = [
      epochLabel,
      currentSlotLabel,
      totalSlotsLabel,
    ];

    const values = [epoch, slots, totalSlots];
    const keys = ['epoch', 'slots', 'totalSlots'];

    return labels.map<any>((
      label: string, // eslint-disable-line
      index: number
    ) => (
      <Fragment key={keys[index]}>
        {generateFieldPanel(labels, values, index)}
      </Fragment>
    ));
  };

  render() {
    const { intl } = this.context;
    const { networkTip, nextEpoch, currentLocale } = this.props;
    const epoch = get(networkTip, 'epoch', '-');
    const nextEpochStart = get(nextEpoch, 'epochStart', '');
    const nextEpochNumber = get(nextEpoch, 'epochNumber', 0);
    const slot = get(networkTip, 'slot', '-');
    const totalSlots = SLOTS_TOTAL;
    const headingFirst = intl.formatMessage(messages.headingRight);
    const headingSecond = intl.formatMessage(messages.headingLeft, {
      nextEpochNumber,
    });
    const timeUntilFutureEpoch = humanizeDurationByLocale(
      this.state.timeUntilFutureEpoch,
      currentLocale
    );
    const description = intl.formatMessage(messages.description, {
      timeUntilFutureEpoch,
    });
    const fieldPanels = this.generateCurrentEpochPanels(
      epoch,
      slot,
      totalSlots
    );

    const nextEpochDateDifference =
      new Date(nextEpochStart).getTime() - new Date().getTime();

    const showNextEpochCountdown =
      nextEpochNumber > 0 && nextEpochDateDifference > 0;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.mainContainer}>
            <div className={styles.countdownContainer}>
              <div className={styles.heading}>{headingFirst}</div>
              <div className={styles.epochsContainer}>
                <div className={styles.epochs}>{fieldPanels}</div>
              </div>
            </div>
            {showNextEpochCountdown && (
              <div className={styles.countdownContainer}>
                <div className={styles.heading}>{headingSecond}</div>
                <CountdownWidget
                  nextEpochStart={nextEpochStart}
                  showLoader={false}
                  format="DD-HH-mm-ss"
                />
              </div>
            )}
          </div>
          <div className={styles.description}>{description}</div>
        </div>
      </div>
    );
  }
}
