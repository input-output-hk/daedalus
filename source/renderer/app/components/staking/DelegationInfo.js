// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import humanizeDuration from 'humanize-duration';
import styles from './DelegationInfo.scss';

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

type Props = { currentLocale: string, timeLeft?: number };

@observer
export default class DelegationInfo extends Component<Props> {
  static defaultProps = {
    timeLeft: 0,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  translateTimeLeft = () => {
    const { currentLocale, timeLeft } = this.props;

    let humanizedDurationLanguage = null;
    switch (currentLocale) {
      case 'ja-JP':
        humanizedDurationLanguage = 'ja';
        break;
      case 'zh-CN':
        humanizedDurationLanguage = 'zh_CN';
        break;
      case 'ko-KR':
        humanizedDurationLanguage = 'ko';
        break;
      case 'de-DE':
        humanizedDurationLanguage = 'de';
        break;
      default:
        humanizedDurationLanguage = 'en';
    }

    const duration = humanizeDuration(timeLeft, {
      round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
      language: humanizedDurationLanguage,
    }).replace(/,/g, ''); // clean period without comma

    return duration;
  };

  render() {
    const { intl } = this.context;
    const heading = intl.formatMessage(messages.heading);
    const info = intl.formatMessage(messages.info);
    const timeLeftDesc = intl.formatMessage(messages.timeLeftDesc);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const timeLeftString = this.translateTimeLeft();

    return (
      <div className={styles.component}>
        <div className={styles.heading}>{heading}</div>
        <div className={styles.info}>{info}</div>
        <div className={styles.timeLeftDesc}>{timeLeftDesc}</div>
        <div className={styles.timeLeft}>{timeLeftString}</div>
        <Button label={buttonLabel} skin={ButtonSkin} />
      </div>
    );
  }
}
