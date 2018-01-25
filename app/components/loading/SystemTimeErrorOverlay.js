// @flow
import React, { Component } from 'react';
import humanizeDuration from 'humanize-duration';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/raw/ButtonSkin';
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
    defaultMessage: '!!!Attention, Daedalus is unable to sync with the blockchain because the time on your machine is different from the global time. You are 2 hours 12 minutes 54 seconds behind.<br>To synchronize the time and fix this issue, please visit the FAQ section of Daedalus website:',
    description: 'Text of Sync error overlay'
  },
  problemSolutionLink: {
    id: 'systemTime.error.problemSolutionLink',
    defaultMessage: '!!!daedaluswallet.io/faq',
    description: 'Link to Daedalus website FAQ page'
  },
});

type Props = {
  localTimeDifference: number,
  currentLocale?: string,
  onProblemSolutionClick: Function,
};

@observer
export default class SystemTimeErrorOverlay extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { localTimeDifference, currentLocale } = this.props;
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

    const behindTime = humanizeDuration(localTimeDifference / 1000, {
      round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
      language: humanizedDurationLanguage
    }).replace(/,/g, ''); // replace 1 day, 3 hours, 12 seconds* to clean period without comma

    return (
      <div className={styles.component}>

        <SvgInline svg={attentionIcon} className={styles.icon} />

        <h1>{intl.formatMessage(messages.overlayTitle)}</h1>

        <p><FormattedHTMLMessage {...messages.overlayText} values={{ behindTime }} /></p>

        <Button
          label={problemSolutionLink}
          skin={<SimpleButtonSkin />}
          onClick={this.onProblemSolutionClick.bind(this, problemSolutionLink)}
        />

      </div>
    );
  }

  onProblemSolutionClick = (link: string) => {
    this.props.onProblemSolutionClick(link);
  }
}
