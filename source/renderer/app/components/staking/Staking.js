// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BlockGenerationInfo from './BlockGenerationInfo';
import styles from './Staking.scss';

const messages = defineMessages({
  heading: {
    id: 'paper.delegation.notification.heading',
    defaultMessage: '!!!Cardano decentralisation',
    description: 'Headline for the Decentralisation notification.',
  },
  info: {
    id: 'paper.delegation.notification.info',
    defaultMessage:
      '!!!Cardano will soon start its transition from a federated to a decentralized system.This will mark the beginning of the reward era in which stakeholders will be able to participate in the process of staking or can delegate their stake to stake pools to earn rewards in ada.',
    description: 'Info for the Decentralisation notification.',
  },
  timeLeftDesc: {
    id: 'paper.delegation.notification.timeLeftDesc',
    defaultMessage: '!!!Reward era begins in',
    description: 'Description for the Decentralisation notification.',
  },
  buttonLabel: {
    id: 'paper.delegation.notification.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralisation notification.',
  },
});

@observer
export default class Staking extends Component<any> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <BlockGenerationInfo
            heading={intl.formatMessage(messages.heading)}
            info={intl.formatMessage(messages.info)}
            timeLeftDesc={intl.formatMessage(messages.timeLeftDesc)}
            timeLeft={(78 * 60 + 10) * 60 * 1000}
            buttonLabel={intl.formatMessage(messages.buttonLabel)}
          />
        </div>
      </div>
    );
  }
}
