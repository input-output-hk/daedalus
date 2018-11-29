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
    defaultMessage: '!!!If you are experiencing issues, for guidance please see the {faqLink} article in the Support Portal.',
    description: 'Content for the "Help and support" section on the support settings page.',
  },
  faqLink: {
    id: 'settings.support.faq.faqLink',
    defaultMessage: '!!!Known Issues',
    description: '"Known Issues" link in the "Help and support" section on the support settings page',
  },
  reportProblemTitle: {
    id: 'settings.support.reportProblem.title',
    defaultMessage: '!!!Reporting a problem',
    description: 'Title "Reporting a problem" on the support settings page.',
  },
  reportProblemContent: {
    id: 'settings.support.reportProblem.content',
    defaultMessage: '!!!If the FAQ does not solve the issue you are experiencing, please use our {supportRequestLink} feature.',
    description: 'Content for the "Reporting a problem" section on the support settings page.',
  },
  supportRequestLink: {
    id: 'settings.support.reportProblem.link',
    defaultMessage: '!!!Support request',
    description: '"Support request" link in the "Report a problem" section on the support settings page.',
  },
  supportRequestLinkUrl: {
    id: 'settings.support.reportProblem.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/categories/360000877653-Daedalus-wallet-mainnet',
    description: '"Support request" link URL in the "Report a problem" section on the support settings page.',
  },
  logsTitle: {
    id: 'settings.support.logs.title',
    defaultMessage: '!!!Logs',
    description: 'Title "Logs" on the support settings page.',
  },
  logsContent: {
    id: 'settings.support.logs.content',
    defaultMessage: '!!!If you want to inspect logs, you can {downloadLogsLink}. Logs do not contain sensitive information, and it would be helpful to attach them to problem reports to help the team investigate the issue you are experiencing. Logs can be attached automatically when using the bug reporting feature.',
    description: 'Content for the "Logs" section on the support settings page.',
  },
  downloadLogsLink: {
    id: 'settings.support.logs.downloadLogsLink',
    defaultMessage: '!!!download them here',
    description: '"download them here" link in the Logs section on the support settings page',
  },
});

type Props = {
  onExternalLinkClick: (event: MouseEvent, url: string) => void,
  onDownloadLogs: Function,
};

@observer
export default class SupportSettings extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onExternalLinkClick, onDownloadLogs } = this.props;
    const { intl } = this.context;
    const faqLinkUrl = intl.formatMessage(globalMessages.faqLinkUrl);

    const faqLink = (
      <a
        href={faqLinkUrl}
        onClick={event => onExternalLinkClick(event, faqLinkUrl)}
      >
        {intl.formatMessage(messages.faqLink)}
      </a>
    );

    const supportRequestLinkUrl = intl.formatMessage(messages.supportRequestLinkUrl);
    const supportRequestLink = (
      <button onClick={event => onExternalLinkClick(event, supportRequestLinkUrl)}>
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

        <p><FormattedMessage {...messages.faqContent} values={{ faqLink }} /></p>

        <h1>{intl.formatMessage(messages.reportProblemTitle)}</h1>

        <p>
          <FormattedMessage {...messages.reportProblemContent} values={{ supportRequestLink }} />
        </p>

        <h1>{intl.formatMessage(messages.logsTitle)}</h1>

        <p><FormattedMessage {...messages.logsContent} values={{ downloadLogsLink }} /></p>

      </div>
    );
  }

}
