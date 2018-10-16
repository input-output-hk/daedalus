// @flow
import React, { Component } from 'react';
import humanizeDuration from 'humanize-duration';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
import { ALLOWED_TIME_DIFFERENCE } from '../../config/timingConfig';
import styles from './SystemTimeErrorOverlay.scss';

const messages = defineMessages({
  overlayTitle: {
    id: 'systemTime.error.overlayTitle',
    defaultMessage: '!!!Unable to sync - incorrect time',
    description: 'Title of Sync error overlay'
  },
  overlayText: {
    id: 'systemTime.error.overlayText',
    defaultMessage: '!!!Attention, Daedalus is unable to sync with the blockchain because the time on your machine is different from the global time. Your time is off by 2 hours 12 minutes 54 seconds.<br>To synchronize the time and fix this issue, please visit the FAQ section of Daedalus website:',
    description: 'Text of Sync error overlay'
  },
  timeOffsetIndeterminable: {
    id: 'systemTime.error.timeOffsetIndeterminable',
    defaultMessage: '!!!more than 15 seconds',
    description: 'Time offset text shown in case NTP service is unreachable'
  },
  problemSolutionLink: {
    id: 'systemTime.error.problemSolutionLink',
    defaultMessage: '!!!daedaluswallet.io/faq',
    description: 'Link to Daedalus website FAQ page'
  },
  onCheckTheTimeAgainLink: {
    id: 'systemTime.error.onCheckTheTimeAgainLink',
    defaultMessage: '!!!Check the time again',
    description: 'Text of Check the time again button'
  },
});

type Props = {
  localTimeDifference: ?number,
  currentLocale: string,
  onProblemSolutionClick: Function,
  onCheckTheTimeAgain: Function,
  isCheckingSystemTime: boolean,
};

@observer
export default class SystemTimeErrorOverlay extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { localTimeDifference, currentLocale, isCheckingSystemTime } = this.props;
    const problemSolutionLink = intl.formatMessage(messages.problemSolutionLink);

    let humanizedDurationLanguage;
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

    const isNTPServiceReachable = !!localTimeDifference;
    const allowedTimeDifferenceInSeconds = ALLOWED_TIME_DIFFERENCE / 1000000;

    const timeOffset = isNTPServiceReachable ? (
      humanizeDuration((localTimeDifference || 0) / 1000, {
        round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
        language: humanizedDurationLanguage,
      }).replace(/,/g, '') // replace 1 day, 3 hours, 12 seconds* to clean period without comma
    ) : (
      // NTP service is unreachable so we need to show a generic time offset message
      intl.formatMessage(messages.timeOffsetIndeterminable, { allowedTimeDifferenceInSeconds })
    );

    return (
      <div className={styles.component}>

        <SVGInline svg={attentionIcon} className={styles.icon} />

        <p><FormattedHTMLMessage {...messages.overlayText} values={{ timeOffset }} /></p>

        <Button
          className="disclaimer"
          label={problemSolutionLink}
          skin={ButtonSkin}
          onClick={this.onProblemSolutionClick.bind(this, problemSolutionLink)}
        />

        <button
          className={styles.checkLink}
          onClick={() => this.props.onCheckTheTimeAgain()}
          disabled={isCheckingSystemTime}
        >
          {intl.formatMessage(messages.onCheckTheTimeAgainLink)}
        </button>

      </div>
    );
  }

  onProblemSolutionClick = (link: string) => {
    this.props.onProblemSolutionClick(link);
  };
}
