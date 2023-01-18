import { defineMessages } from 'react-intl';

export const messages = defineMessages({
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
  analyticsSectionTitle: {
    id: 'analytics.form.title',
    defaultMessage: '!!!Analytics data collection',
    description: 'Analytics form title',
  },
  analyticsAcceptedDescription: {
    id: 'analytics.form.analyticsAcceptedDescription',
    defaultMessage:
      '!!!You have <strong>opted in</strong> to analytics data collection. You can {changeAnalyticsSettingsLink}.',
    description: 'Analytics data collection description when user opted in',
  },
  analyticsDeclinedDescription: {
    id: 'analytics.form.analyticsDeclinedDescription',
    defaultMessage:
      '!!!You have <strong>opted out</strong> of analytics data collection. You can {changeAnalyticsSettingsLink}.',
    description: 'Analytics data collection description when user opted out ',
  },
  changeAnalyticsSettings: {
    id: 'analytics.form.changeAnalyticsSettings',
    defaultMessage: '!!!You can {changeAnalyticsSettingsLink}.',
    description: 'Change analytics settings link',
  },
  changeAnalyticsSettingsLink: {
    id: 'analytics.form.changeAnalyticsSettingsLink',
    defaultMessage: '!!!change this setting here',
    description: 'Change analytics settings link text',
  },
});
