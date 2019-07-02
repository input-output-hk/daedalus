// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import humanizeDuration from 'humanize-duration';
import styles from './CountdownWidget.scss';

const TIME_LEFT_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;

type Props = {
  redirectToStakingInfo?: Function,
  currentLocale: string,
  startDateTime: string,
};
type State = { timeLeft: number };

@observer
export default class CountdownWidget extends Component<Props, State> {
  intervalHandler: ?IntervalID = null;
  state = { timeLeft: 0 };

  componentDidMount() {
    this.intervalHandler = setInterval(
      () => this.updateTimeLeft(),
      TIME_LEFT_INTERVAL
    );
  }

  updateTimeLeft = () => {
    const { redirectToStakingInfo, startDateTime } = this.props;
    const timeLeft = Math.max(
      0,
      new Date(startDateTime).getTime() - new Date().getTime()
    );

    this.setState({ timeLeft });

    if (timeLeft === 0) {
      if (this.intervalHandler) {
        clearInterval(this.intervalHandler);
      }

      if (redirectToStakingInfo) {
        redirectToStakingInfo();
      }
    }
  };

  componentWillUnmount() {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
    }
  }

  translateTimeLeft = () => {
    const { currentLocale } = this.props;
    const { timeLeft } = this.state;
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
    const timeLeftString = this.translateTimeLeft();

    return <div className={styles.timeLeft}>{timeLeftString}</div>;
  }
}
