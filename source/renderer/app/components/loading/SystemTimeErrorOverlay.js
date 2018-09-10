// @flow
import React, { Component } from 'react';
import humanizeDuration from 'humanize-duration';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
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
  onCheckTheTimeAgainLinkExecuting: {
    id: 'systemTime.error.onCheckTheTimeAgainLinkExecuting',
    defaultMessage: '!!!Checking time',
    description: 'Text of Check the time again button when executing'
  },
});

type Props = {
  localTimeDifference: number,
  currentLocale: string,
  onProblemSolutionClick: Function,
  onCheckTheTimeAgain: Function,
  isCheckingTheTimeAgain: boolean,
};

@observer
export default class SystemTimeErrorOverlay extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { localTimeDifference, currentLocale, isCheckingTheTimeAgain } = this.props;
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

    const timeOffset = humanizeDuration(localTimeDifference / 1000, {
      round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
      language: humanizedDurationLanguage,
    }).replace(/,/g, ''); // replace 1 day, 3 hours, 12 seconds* to clean period without comma

    const checkTheTimeAgainMessage = isCheckingTheTimeAgain
      ? messages.onCheckTheTimeAgainLinkExecuting
      : messages.onCheckTheTimeAgainLink;

    window.onCheckTheTimeAgain = this.props.onCheckTheTimeAgain;

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
          disabled={isCheckingTheTimeAgain}
        >
          {intl.formatMessage(checkTheTimeAgainMessage)}
        </button>

      </div>
    );
  }

  onProblemSolutionClick = (link: string) => {
    this.props.onProblemSolutionClick(link);
  }
}
