// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './SupportSettings.scss';

const messages = defineMessages({
  faqTitle: {
    id: 'settings.support.faq.title',
    defaultMessage: '!!!Frequently asked questions',
    description: 'Title "Frequently asked questions" on the support settings page.',
  },
  faqContent: {
    id: 'settings.support.faq.content',
    defaultMessage: 'If you are experiencing issues, please look at the <a href="https://daedaluswallet.io/faq/">FAQ on Daedalus website</a> for solutions to known issues.',
    description: 'Content for the FAQ on the support settings page.',
  },
  reportProblemTitle: {
    id: 'settings.support.reportProblem.title',
    defaultMessage: '!!!Reporting a problem',
    description: 'Title "Reporting a problem" on the support settings page.',
  },
  reportProblemContent: {
    id: 'settings.support.reportProblem.content',
    defaultMessage: 'If the FAQ does not contain a solution for the issue you are experiencing, please use <a href="#">problem reporting feature</a> to report the issue you are experiencing.',
    description: 'Content for the FAQ on the support settings page.',
  },
  logsTitle: {
    id: 'settings.support.logs.title',
    defaultMessage: '!!!Logs',
    description: 'Title "Logs" on the support settings page.',
  },
  logsContent: {
    id: 'settings.support.logs.content',
    defaultMessage: 'If you want to inspect logs, you can <a href="#">download them here</a>. Logs do not contain any sensitive information, and it would be helpful to attach them to problem reports to help the team to investigate the issue you are experiencing. Logs can be attached automatically when using the bug reporting feature.',
    description: 'Content for the FAQ on the support settings page.',
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
    const { onExternalLinkClick, onSupportRequestClick, onDownloadLogs } = this.props;
    const { intl } = this.context;

    return (
      <div className={styles.component}>

        <h1>{intl.formatMessage(messages.faqTitle)}</h1>

        <p
          onClick={event => onExternalLinkClick(event)}
          dangerouslySetInnerHTML={{ __html: intl.formatMessage(messages.faqContent) }}
        />

        <h1>{intl.formatMessage(messages.reportProblemTitle)}</h1>

        <p
          onClick={event => onSupportRequestClick(event)}
          dangerouslySetInnerHTML={{ __html: intl.formatMessage(messages.reportProblemContent) }}
        />

        <h1>{intl.formatMessage(messages.logsTitle)}</h1>

        <p
          onClick={event => onDownloadLogs(event)}
          dangerouslySetInnerHTML={{ __html: intl.formatMessage(messages.logsContent) }}
        />

      </div>
    );
  }

}
