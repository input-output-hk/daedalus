import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'dapp.catalog.title',
    defaultMessage: '!!!Preferred dApps',
    description: 'Heading for the local preferred dApp catalog.',
  },
  disclaimer: {
    id: 'dapp.catalog.disclaimer',
    defaultMessage:
      '!!!Preferred dApps have been compatibility-tested by Daedalus. This does not mean they have been security audited or endorsed.',
    description: 'Security disclaimer shown above every available catalog.',
  },
  unavailable: {
    id: 'dapp.catalog.unavailable',
    defaultMessage:
      '!!!The preferred dApp catalog is unavailable in this Daedalus build.',
    description:
      'Shown instead of catalog entries when the feature is unavailable.',
  },
  notReady: {
    id: 'dapp.catalog.notReady',
    defaultMessage:
      '!!!Finish wallet setup and synchronization before launching a dApp.',
    description: 'Explains why catalog launch controls are disabled.',
  },
  open: {
    id: 'dapp.catalog.open',
    defaultMessage: '!!!A dApp session is open.',
    description: 'Status shown while a trusted dApp session is open.',
  },
  launch: {
    id: 'dapp.catalog.launch',
    defaultMessage: '!!!Launch',
    description: 'Button label for launching a trusted dApp.',
  },
  launching: {
    id: 'dapp.catalog.launching',
    defaultMessage: '!!!Launching…',
    description: 'Button label while a trusted dApp is launching.',
  },
  close: {
    id: 'dapp.catalog.close',
    defaultMessage: '!!!Close dApp',
    description: 'Button label for closing the open dApp session.',
  },
});

export default messages;
