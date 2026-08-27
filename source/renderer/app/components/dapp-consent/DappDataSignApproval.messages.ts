import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'dapp.dataSign.approval.title',
    defaultMessage: '!!!Review data signature request',
    description: 'Title for a CIP-30 data-signing request.',
  },
  approve: {
    id: 'dapp.dataSign.approval.approve',
    defaultMessage: '!!!Sign data',
    description: 'Approve a CIP-30 data-signing request.',
  },
  reject: {
    id: 'dapp.dataSign.approval.reject',
    defaultMessage: '!!!Reject',
    description: 'Reject a CIP-30 data-signing request.',
  },
  origin: {
    id: 'dapp.dataSign.approval.origin',
    defaultMessage: '!!!Origin: {value}',
    description: 'Origin requesting a data signature.',
  },
  wallet: {
    id: 'dapp.dataSign.approval.wallet',
    defaultMessage: '!!!Wallet: {value}',
    description: 'Wallet selected for data signing.',
  },
  network: {
    id: 'dapp.dataSign.approval.network',
    defaultMessage: '!!!Network: {value}',
    description: 'Network selected for data signing.',
  },
  credential: {
    id: 'dapp.dataSign.approval.credential',
    defaultMessage: '!!!Credential: {value}',
    description: 'Payment or stake credential selected for signing.',
  },
  address: {
    id: 'dapp.dataSign.approval.address',
    defaultMessage: '!!!Canonical address bytes',
    description: 'Label for canonical raw address hex.',
  },
  payload: {
    id: 'dapp.dataSign.approval.payload',
    defaultMessage: '!!!Exact payload bytes',
    description: 'Label for exact payload hex.',
  },
  preview: {
    id: 'dapp.dataSign.approval.preview',
    defaultMessage: '!!!Safe UTF-8 preview',
    description: 'Label for safe exact UTF-8 payload preview.',
  },
  noPreview: {
    id: 'dapp.dataSign.approval.noPreview',
    defaultMessage:
      '!!!No safe UTF-8 preview is available. Review the exact hex.',
    description: 'Shown when payload bytes are not safe display text.',
  },
  password: {
    id: 'dapp.dataSign.approval.password',
    defaultMessage: '!!!Wallet spending password',
    description: 'Label for software wallet spending password.',
  },
});
