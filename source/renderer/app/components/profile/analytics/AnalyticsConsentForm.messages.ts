import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'ariadne.analytics.v2.title',
    defaultMessage: '!!!Ariadne analytics — provisional notice v2',
    description:
      'Draft consent copy for local review, not approved release wording',
  },
  description: {
    id: 'ariadne.analytics.v2.description',
    defaultMessage:
      '!!!Optional pseudonymous usage data is sent to the configured Ariadne support deployment to improve Daedalus. A separate random installation identifier links your events across accepted restarts. This is pseudonymous information, not anonymous data. Events are not linked to support tickets. Previous Matomo permission does not apply.',
    description: 'Recipient and purpose; provisional review copy',
  },
  allowButton: {
    id: 'ariadne.analytics.v2.allow',
    defaultMessage: '!!!Allow Ariadne analytics',
    description: 'Explicit fresh acceptance',
  },
  disallowButton: {
    id: 'ariadne.analytics.v2.reject',
    defaultMessage: '!!!Keep off / revoke',
    description: 'Reject or withdraw consent',
  },
  analyticsSectionPrivacyPolicy: {
    id: 'ariadne.analytics.v2.retention',
    defaultMessage:
      '!!!Events are retained for up to 24 months; private backups follow the deployment retention policy. Revoking stops collection, discards unsent events and clears this installation identifier. It does not delete events already received. Accepting again creates a new identifier. Do not share recovery phrases or spending passwords. Final recipient, privacy notice, backup retention and deletion-request arrangements require review before release.',
    description:
      'Provisional retention, revocation and outstanding policy decisions',
  },
  disabled: {
    id: 'ariadne.analytics.v2.disabled',
    defaultMessage:
      '!!!Collection is disabled or unavailable in this configuration. Wallet features remain available.',
    description: 'Main-process disabled configuration',
  },
  saveFailed: {
    id: 'ariadne.analytics.v2.saveFailed',
    defaultMessage:
      '!!!The choice could not be saved. Collection is stopped for this session. Retry before restarting to ensure your choice is preserved.',
    description: 'Consent persistence failed; no raw error details',
  },
});
