import { defineMessages } from 'react-intl';

export default defineMessages({
  title: {
    id: 'settings.dappConnections.title',
    defaultMessage: '!!!dApp connections',
    description: 'Title of the dApp connection settings page.',
  },
  closeDescription: {
    id: 'settings.dappConnections.closeDescription',
    defaultMessage:
      '!!!Close dApp closes the browser window but remembers this connection.',
    description: 'Explanation of closing a dApp browser.',
  },
  disconnectDescription: {
    id: 'settings.dappConnections.disconnectDescription',
    defaultMessage:
      '!!!Disconnect ends the current wallet session but remembers this connection.',
    description: 'Explanation of disconnecting a live dApp wallet session.',
  },
  forgetDescription: {
    id: 'settings.dappConnections.forgetDescription',
    defaultMessage:
      '!!!Forget connection disconnects the dApp and permanently removes its saved permissions.',
    description: 'Explanation of forgetting a dApp connection.',
  },
  empty: {
    id: 'settings.dappConnections.empty',
    defaultMessage: '!!!No saved dApp connections.',
    description: 'Empty state for dApp connection settings.',
  },
  baseAccess: {
    id: 'settings.dappConnections.baseAccess',
    defaultMessage: '!!!Wallet connection and read access',
    description: 'Label for the base CIP-30 connection permission.',
  },
  cip95: {
    id: 'settings.dappConnections.cip95',
    defaultMessage: '!!!CIP-95 governance public-key disclosure',
    description: 'Label for the separately revocable CIP-95 disclosure scope.',
  },
  cip104Unavailable: {
    id: 'settings.dappConnections.cip104Unavailable',
    defaultMessage:
      '!!!CIP-104 account public-key disclosure is unavailable and grants no access.',
    description: 'Terminal-disabled CIP-104 status.',
  },
  cip104Legacy: {
    id: 'settings.dappConnections.cip104Legacy',
    defaultMessage: '!!!Legacy CIP-104 account public-key disclosure',
    description: 'Label for a defensive legacy CIP-104 disclosure scope.',
  },
  disconnect: {
    id: 'settings.dappConnections.disconnect',
    defaultMessage: '!!!Disconnect {origin}',
    description: 'Button label to disconnect a dApp origin.',
  },
  forget: {
    id: 'settings.dappConnections.forget',
    defaultMessage: '!!!Forget {origin}',
    description: 'Button label to forget a dApp origin.',
  },
  wallet: {
    id: 'settings.dappConnections.wallet',
    defaultMessage: '!!!Wallet',
    description: 'Wallet field label for a saved dApp connection.',
  },
  networkMagic: {
    id: 'settings.dappConnections.networkMagic',
    defaultMessage: '!!!Network magic',
    description: 'Network magic field label for a saved dApp connection.',
  },
  source: {
    id: 'settings.dappConnections.source',
    defaultMessage: '!!!Source',
    description: 'Launch source field label for a saved dApp connection.',
  },
  granted: {
    id: 'settings.dappConnections.granted',
    defaultMessage: '!!!Granted',
    description: 'Grant date field label for a saved dApp connection.',
  },
  revoke: {
    id: 'settings.dappConnections.revoke',
    defaultMessage: '!!!Revoke {scope} for {origin}',
    description: 'Button label to revoke one dApp disclosure scope.',
  },
  corrupt: {
    id: 'settings.dappConnections.corrupt',
    defaultMessage:
      '!!!Saved dApp connections are damaged. Access is blocked until the empty connection store is repaired.',
    description: 'Fail-closed corrupted grant store message.',
  },
  repair: {
    id: 'settings.dappConnections.repair',
    defaultMessage: '!!!Repair and remove saved connections',
    description: 'Button label to repair the corrupted dApp grant store.',
  },
  failed: {
    id: 'settings.dappConnections.failed',
    defaultMessage: '!!!The connection change could not be completed.',
    description: 'Generic connection management failure.',
  },
  catalog: {
    id: 'settings.dappConnections.catalog',
    defaultMessage: '!!!Preferred catalog',
    description: 'Label for a preferred catalog dApp connection.',
  },
  diagnostics: {
    id: 'settings.dappConnections.diagnostics',
    defaultMessage: '!!!Diagnostics',
    description: 'Label for an arbitrary diagnostics dApp connection.',
  },
});
