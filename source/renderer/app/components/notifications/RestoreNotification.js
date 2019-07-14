// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import humanizeDuration from 'humanize-duration';
import { defineMessages, intlShape } from 'react-intl';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import styles from './RestoreNotification.scss';

const messages = defineMessages({
  activeRestoreMessage: {
    id: 'wallet.statusMessages.activeRestore',
    defaultMessage: '!!!Wallet restore in progress',
    description:
      'Status message "Wallet restore in progress" shown while wallet is being restored.',
  },
});

type Props = {
  currentLocale: string,
  restoreProgress: number,
  restoreETA: number,
};

@observer
export default class RestoreNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { currentLocale, restoreProgress, restoreETA } = this.props;

    const restoreNotificationClasses = classnames(
      styles.component,
      'ActiveRestoreNotification'
    );

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

    const estimatedCompletionTime = humanizeDuration(restoreETA, {
      round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
      language: humanizedDurationLanguage,
    });

    return (
      <div className={restoreNotificationClasses}>
        <span className={styles.text}>
          {intl.formatMessage(messages.activeRestoreMessage)}: {restoreProgress}
          % ({estimatedCompletionTime})
        </span>
        <SVGInline svg={spinnerIcon} className={styles.icon} />
      </div>
    );
  }
}
