import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'ariadne.analytics.v2.details',
    defaultMessage: '!!!What Ariadne receives',
    description: 'Provisional data inventory heading',
  },
  userBehaviorTitle: {
    id: 'ariadne.analytics.v2.eventsTitle',
    defaultMessage: '!!!Usage events',
    description: 'Event inventory heading',
  },
  userBehaviorText: {
    id: 'ariadne.analytics.v2.events',
    defaultMessage:
      '!!!Allowlisted page names and actions, event time, network, installation identifier and hardware/software wallet type where applicable. Delegation submission and voting registration setup also send start, completion or pre-submission cancellation steps with a separate random identifier for each attempt. No wallet addresses, balances, transaction identifiers, messages, recovery material, raw routes or free-text labels. IP addresses are not analytics fields; separate infrastructure access logs may contain IP addresses.',
    description: 'Exact v1/v2 event inventory, provisional copy',
  },
  deviceInfoTitle: {
    id: 'ariadne.analytics.v2.deviceTitle',
    defaultMessage: '!!!Device categories',
    description: 'Device heading',
  },
  deviceInfoText: {
    id: 'ariadne.analytics.v2.device',
    defaultMessage:
      '!!!Operating system and numeric version where supported, CPU family, rounded RAM size, Daedalus version, and whether legacy or hardware wallets are present. These fields are pseudonymous, not anonymous.',
    description: 'Exact v1/v2 dimensions',
  },
  expandButton: {
    id: 'analytics.dialog.expandButton',
    defaultMessage: '!!!Expand details',
    description: 'Expand details button',
  },
  collapseButton: {
    id: 'analytics.dialog.collapseButton',
    defaultMessage: '!!!Collapse details',
    description: 'Collapse details button',
  },
});
