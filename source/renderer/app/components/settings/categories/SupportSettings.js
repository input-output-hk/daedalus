// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import styles from './SupportSettings.scss';
import globalMessages from '../../../i18n/global-messages.js';

const messages = defineMessages({
  faqTitle: {
    id: 'settings.support.faq.title',
    defaultMessage: '!!!Help and support',
    description: 'Title "Help and support" on the support settings page.',
  },
  faqContent: {
    id: 'settings.support.faq.content',
    defaultMessage:
      '!!!If you are experiencing issues, for guidance please see the {faqLink} article in the Support Portal.',
    description:
      'Content for the "Help and support" section on the support settings page.',
  },
  faqLink: {
    id: 'settings.support.faq.faqLink',
    defaultMessage: '!!!Known Issues',
    description:
      '"Known Issues" link in the "Help and support" section on the support settings page',
  },
  reportProblemTitle: {
    id: 'settings.support.reportProblem.title',
    defaultMessage: '!!!Reporting a problem',
    description: 'Title "Reporting a problem" on the support settings page.',
  },
  reportProblemContent: {
    id: 'settings.support.reportProblem.content',
    defaultMessage:
      '!!!If you are still experiencing an issue, please submit a support request.',
    description:
      'Content for the "Reporting a problem" section on the support settings page.',
  },
  supportRequestLink: {
    id: 'settings.support.reportProblem.link',
    defaultMessage: '!!!submit a support request',
    description:
      '"submit a support request" link in the "Report a problem" section on the support settings page.',
  },
  logsTitle: {
    id: 'settings.support.logs.title',
    defaultMessage: '!!!Logs',
    description: 'Title "Logs" on the support settings page.',
  },
  logsContent: {
    id: 'settings.support.logs.content',
    defaultMessage:
      '!!!Please download your logs here and attach the downloaded file when submitting a support ticket to help the support team investigate the issue. Logs do not contain sensitive information.',
    description: 'Content for the "Logs" section on the support settings page.',
  },
  downloadLogsLink: {
    id: 'settings.support.logs.downloadLogsLink',
    defaultMessage: '!!!download your logs here',
    description:
      '"download your logs here" link in the Logs section on the support settings page',
  },
});

type Props = {
  onExternalLinkClick: Function,
  onSupportRequestClick: Function,
  onDownloadLogs: Function,
};

@observer
export default class SupportSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      onExternalLinkClick,
      onSupportRequestClick,
      onDownloadLogs,
    } = this.props;
    const { intl } = this.context;
    const faqLinkUrl = intl.formatMessage(globalMessages.faqLinkUrl);

    const faqLink = (
      <a
        href={faqLinkUrl}
        onClick={event => onExternalLinkClick(faqLinkUrl, event)}
      >
        {intl.formatMessage(messages.faqLink)}
      </a>
    );

    const supportRequestLink = (
      <button onClick={onSupportRequestClick}>
        {intl.formatMessage(messages.supportRequestLink)}
      </button>
    );

    const downloadLogsLink = (
      <button onClick={onDownloadLogs}>
        {intl.formatMessage(messages.downloadLogsLink)}
      </button>
    );

    return (
      <div className={styles.component}>
        <h1>{intl.formatMessage(messages.faqTitle)}</h1>

        <p>
          <FormattedMessage {...messages.faqContent} values={{ faqLink }} />
        </p>

        <h1>{intl.formatMessage(messages.reportProblemTitle)}</h1>

        <p>
          <FormattedMessage
            {...messages.reportProblemContent}
            values={{ supportRequestLink }}
          />
        </p>

        <h1>{intl.formatMessage(messages.logsTitle)}</h1>

        <p>
          <FormattedMessage
            {...messages.logsContent}
            values={{ downloadLogsLink }}
          />
        </p>
      </div>
    );
  }
}
