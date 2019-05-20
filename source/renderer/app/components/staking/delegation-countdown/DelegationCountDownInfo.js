// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import humanizeDuration from 'humanize-duration';
import moment from 'moment';
import styles from './DelegationCountDownInfo.scss';

const messages = defineMessages({
  heading: {
    id: 'staking.delegationCountDown.heading',
    defaultMessage: '!!!Cardano decentralization',
    description: 'Headline for the Decentralisation notification.',
  },
  description: {
    id: 'staking.delegationCountDown.description',
    defaultMessage:
      '!!!Cardano will soon start its transition from a federated to a decentralized system. This will mark the start of stakeholders being able to earn rewards for participating in the running of the network. They will be able to participate directly in the process of staking or can delegate their stake to stake pools to earn rewards in ada.',
    description: 'Info for the Decentralisation notification.',
  },
  timeLeftDesc: {
    id: 'staking.delegationCountDown.timeLeftDesc',
    defaultMessage: '!!!Rewards begin in',
    description: 'Description for the Decentralisation notification.',
  },
  buttonLabel: {
    id: 'staking.delegationCountDown.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralisation notification.',
  },
});

type Props = { currentLocale: string, startDateTime: string };

@observer
export default class DelegationCountDownInfo extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  translateTimeLeft = () => {
    const { currentLocale, startDateTime } = this.props;
    const timeLeft = Math.max(
      0,
      moment(startDateTime).valueOf() - new Date().getTime()
    );

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
    const description = intl.formatMessage(messages.description);
    const timeLeftDesc = intl.formatMessage(messages.timeLeftDesc);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const timeLeftString = this.translateTimeLeft();

    return (
      <div className={styles.component}>
        <div className={styles.heading}>{heading}</div>
        <div className={styles.description}>{description}</div>
        <div className={styles.timeLeftDesc}>{timeLeftDesc}</div>
        <div className={styles.timeLeft}>{timeLeftString}</div>
        <Button label={buttonLabel} skin={ButtonSkin} />
      </div>
    );
  }
}
