// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import humanizeDuration from 'humanize-duration';
import styles from './DelegationCenterHeader.scss';
import CountdownWidget from '../../widgets/CountdownWidget';
import type { NextEpoch, TipInfo } from '../../../api/network/types';
import { SLOTS_TOTAL } from '../../../config/epochsConfig';
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
      '!!!Changes to delegation preferences will take effect after the next two Cardano epochs have completed. Epochs on the Incentivized Testnet last one day. Any changes made now will take effect in 2 days, 3 hours, and 1 minute.',
    description: 'Delegation description for the Delegation center.',
  },
});

type Props = {
  networkTip: ?TipInfo,
  nextEpoch: ?NextEpoch,
};

@observer
export default class DelegationCenterHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

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
    const { networkTip, nextEpoch } = this.props;
    const epoch = get(networkTip, 'epoch', '-');
    const nextEpochStart = get(nextEpoch, 'epochStart', '');
    const nextEpochNumber = get(nextEpoch, 'epochNumber', 0);
    const slot = get(networkTip, 'slot', '-');
    const totalSlots = SLOTS_TOTAL;
    // const timeUntilNextEpochStart = humanizeDuration(timeLeft || 0, {
    //   round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
    //   language: humanizedDurationLanguage,
    //   conjunction: ' and ',
    //   units: ['d', 'h', 'm'],
    //   serialComma: false,
    // });
    const headingFirst = intl.formatMessage(messages.headingRight);
    const headingSecond = intl.formatMessage(messages.headingLeft, {
      nextEpochNumber,
    });
    const description = intl.formatMessage(messages.description);
    const fieldPanels = this.generateCurrentEpochPanels(
      epoch,
      slot,
      totalSlots
    );

    const showNextEpochCountdown =
      nextEpochNumber > 0 && nextEpochStart.length > 0;

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
