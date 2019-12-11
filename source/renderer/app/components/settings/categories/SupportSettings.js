// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import styles from './SupportSettings.scss';
import globalMessages from '../../../i18n/global-messages';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';

const messages = defineMessages({
  faqTitle: {
    id: 'settings.support.faq.title',
    defaultMessage: '!!!Help and support',
    description: 'Title "Help and support" on the support settings page.',
  },
  faqContent: {
    id: 'settings.support.faq.content',
    defaultMessage:
      '!!!If you are experiencing a problem, please look for guidance using the list of {faqLink} on the support pages. If you can’t find a solution, please submit a support ticket.',
    description:
      'Content for the "Help and support" section on the support settings page.',
  },
  faqLink: {
    id: 'settings.support.faq.faqLink',
    defaultMessage: '!!!Known Issues',
    description:
      '"Known Issues" link in the "Help and support" section on the support settings page',
  },
  stepsTitle: {
    id: 'settings.support.steps.title',
    defaultMessage: '!!!Steps for creating a support request:',
    description:
      'Title "Steps for creating a support request" on the support settings page.',
  },
  stepsDownloadLogsTitle: {
    id: 'settings.support.steps.downloadLogs.title',
    defaultMessage: '!!!Download the logs',
    description: 'Title "Download the logs" on the support settings page.',
  },
  stepsDownloadLogsDescription: {
    id: 'settings.support.steps.downloadLogs.description',
    defaultMessage:
      '!!!Please {downloadLogsLink} and attach the downloaded file when submitting a support request to help the support team investigate the issue. Logs do not contain sensitive information.',
    description:
      'Description of "Download the logs" on the support settings page.',
  },
  stepsDownloadLogsLink: {
    id: 'settings.support.steps.downloadLogs.link',
    defaultMessage: '!!!download your logs here',
    description:
      '"download your logs here" link in the Logs section on the support settings page',
  },
  stepsReportProblemTitle: {
    id: 'settings.support.steps.reportProblem.title',
    defaultMessage: '!!!Report a problem',
    description: 'Title "Report a problem" on the support settings page.',
  },
  stepsReportProblemDescription: {
    id: 'settings.support.steps.reportProblem.description',
    defaultMessage:
      '!!!Please {downloadLogsLink} and attach the downloaded file when submitting a support request to help the support team investigate the issue. Logs do not contain sensitive information.',
    description:
      'Description of "Download the logs" on the support settings page.',
  },
  stepsReportProblemLink: {
    id: 'settings.support.steps.reportProblem.link',
    defaultMessage: '!!!download your logs here',
    description:
      '"download your logs here" link in the Logs section on the support settings page',
  },
});

type Props = {
  onExternalLinkClick: Function,
  onSupportRequestClick: Function,
  onDownloadLogs: Function,
  disableDownloadLogs: boolean,
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
      disableDownloadLogs,
    } = this.props;
    const { intl } = this.context;
    const faqLinkUrl = intl.formatMessage(globalMessages.faqLinkUrl);

    const faqLink = (
      <a
        className={styles.externalLinkNotWrapped}
        href={faqLinkUrl}
        onClick={event => onExternalLinkClick(faqLinkUrl, event)}
      >
        {intl.formatMessage(messages.faqLink)}
        <SVGInline svg={externalLinkIcon} />
      </a>
    );

    const stepsDownloadLogsLink = (
      <button onClick={onDownloadLogs} disabled={disableDownloadLogs}>
        {intl.formatMessage(messages.stepsDownloadLogsLink)}
      </button>
    );

    const reportProblemLink = (
      <span
        className={styles.externalLink}
        role="presentation"
        onClick={onSupportRequestClick}
      >
        {intl.formatMessage(messages.stepsReportProblemLink)}
        <SVGInline svg={externalLinkIcon} />
      </span>
    );

    return (
      <div className={styles.component}>
        {/* Help and Support */}

        <h1>{intl.formatMessage(messages.faqTitle)}</h1>

        <p>
          <FormattedMessage {...messages.faqContent} values={{ faqLink }} />
        </p>

        {/* Steps for creating a support request: */}

        <h1>{intl.formatMessage(messages.stepsTitle)}</h1>

        <ol>
          <li>
            <h2>{intl.formatMessage(messages.stepsDownloadLogsTitle)}</h2>
            <p>
              <FormattedMessage
                {...messages.stepsDownloadLogsDescription}
                values={{ stepsDownloadLogsLink }}
              />
            </p>
          </li>
          <li>
            <h2>{intl.formatMessage(messages.stepsReportProblemTitle)}</h2>
            <p>
              <FormattedMessage
                {...messages.stepsReportProblemDescription}
                values={{ reportProblemLink }}
              />
            </p>
          </li>
        </ol>
      </div>
    );
  }
}
