import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  drepInputLabel: {
    id: 'voting.governance.drepInputLabel',
    defaultMessage:
      '!!!Please type or paste a valid DRep ID (CIP-105) here. Look up {drepDirectoryLink}',
    description: 'Label for DRep input on the governance page',
  },
  drepInputLabelPreprod: {
    id: 'voting.governance.drepInputLabelPreprod',
    defaultMessage: '!!!Please type or paste a valid DRep ID (CIP-105) here.',
    description: 'Label for DRep input on the governance page for preprod',
  },
  drepInputLabelLinkText: {
    id: 'voting.governance.drepInputLabelLinkText',
    defaultMessage: '!!!DRep directory',
    description: 'Label link text for DRep input on the governance page',
  },
  drepInputLabelLinkUrl: {
    id: 'voting.governance.drepInputLabelLinkUrl',
    defaultMessage: 'https://gov.tools/drep_directory',
    description: 'Label link url for DRep input on the governance page',
  },
  drepInputLabelLinkUrlPreview: {
    id: 'voting.governance.drepInputLabelLinkUrlPreview',
    defaultMessage: 'https://preview.gov.tools/drep_directory',
    description: 'Label link url for DRep input on the governance page',
  },
  drepInputError: {
    id: 'voting.governance.drepInputError',
    defaultMessage: '!!!Invalid DRep ID',
    description: 'Error for DRep input on the governance page',
  },
  drepInputPlaceholder: {
    id: 'voting.governance.drepInputPlaceholder',
    defaultMessage: '!!!Paste DRep ID here …',
    description: 'Placeholder for DRep input on the governance page',
  },
  heading: {
    id: 'voting.governance.heading',
    defaultMessage: '!!!CARDANO VOTING POWER DELEGATION',
    description: 'Headline for Governance',
  },
  paragraph1: {
    id: 'voting.governance.paragraph1',
    defaultMessage: '!!!Governance first paragraph',
    description: 'First paragraph for governance page',
  },
  paragraph1LinkUrl: {
    id: 'voting.governance.paragraph1LinkUrl',
    defaultMessage: '!!!Governance first paragraph link url',
    description: 'First paragraph link for governance page',
  },
  paragraph1LinkText: {
    id: 'voting.governance.paragraph1LinkText',
    defaultMessage: '!!!Governance link label',
    description: 'Link labels for governance page',
  },
  selectWalletLabel: {
    id: 'voting.governance.selectWalletLabel',
    defaultMessage: '!!!Select a wallet to delegate from',
    description: 'Label for the wallet select on the governance page',
  },
  selectWalletPlaceholder: {
    id: 'voting.governance.selectWalletPlaceholder',
    defaultMessage: '!!!Select a wallet …',
    description: 'Placeholder for the wallet select on the governance page',
  },
  selectVotingTypeLabel: {
    id: 'voting.governance.selectVotingTypeLabel',
    defaultMessage: '!!!Select voting registration type',
    description:
      'Label for the registration type select on the governance page',
  },
  submitLabel: {
    id: 'voting.governance.submitLabel',
    defaultMessage: '!!!Submit',
    description: 'Label for the submit button on the governance page',
  },
  initializeTxErrorGeneric: {
    id: 'voting.governance.initializeTxError.generic',
    defaultMessage: '!!!Could not initialize transaction. Please try again!',
    description: 'Generic error for initialize transaction',
  },
  initializeTxErrorSameVote: {
    id: 'voting.governance.initializeTxError.sameVote',
    defaultMessage: '!!!Chosen same value as previously',
    description: 'Chosen same value as previously',
  },
  initializeNotEnoughMoney: {
    id: 'voting.governance.initializeTxError.notEnoughMoney',
    defaultMessage: '!!!Not enough funds',
    description: 'Governance voting error when wallet has not enough funds',
  },
});
