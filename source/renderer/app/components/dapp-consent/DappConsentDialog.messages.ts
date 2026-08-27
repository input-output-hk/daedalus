import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  connectionTitle: {
    id: 'dapp.consent.connection.title',
    defaultMessage: '!!!Connect dApp',
    description: 'Title for a dApp connection approval dialog.',
  },
  disclosureTitle: {
    id: 'dapp.consent.keyDisclosure.title',
    defaultMessage: '!!!Share wallet public keys',
    description: 'Title for elevated public-key disclosure approval.',
  },
  origin: {
    id: 'dapp.consent.origin',
    defaultMessage: '!!!Origin: {origin}',
    description: 'Remote dApp origin requesting consent.',
  },
  wallet: {
    id: 'dapp.consent.wallet',
    defaultMessage: '!!!Wallet: {wallet}',
    description: 'Wallet selected for a dApp consent request.',
  },
  network: {
    id: 'dapp.consent.network',
    defaultMessage: '!!!Network: {network}',
    description: 'Network selected for a dApp consent request.',
  },
  scopes: {
    id: 'dapp.consent.scopes',
    defaultMessage: '!!!Permissions: {scopes}',
    description: 'Permissions requested by a dApp.',
  },
  extensions: {
    id: 'dapp.consent.extensions',
    defaultMessage: '!!!Extensions: {extensions}',
    description: 'CIP extensions requested by a dApp.',
  },
  disclosureWarning: {
    id: 'dapp.consent.keyDisclosure.warning',
    defaultMessage:
      '!!!Stake and DRep public keys can correlate this wallet with governance activity. This permission is separate from the connection and can be revoked independently.',
    description: 'Privacy warning before elevated public-key disclosure.',
  },
  reject: {
    id: 'dapp.consent.reject',
    defaultMessage: '!!!Reject',
    description: 'Reject a dApp consent request.',
  },
  approve: {
    id: 'dapp.consent.approve',
    defaultMessage: '!!!Approve',
    description: 'Approve a dApp consent request.',
  },
});
