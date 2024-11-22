import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  abstain: {
    id: 'voting.governance.abstain',
    defaultMessage: '!!!Abstain',
    description: 'Translation for the "abstain" governance vote type',
  },
  delegateToDRep: {
    id: 'voting.governance.delegateToDRep',
    defaultMessage: '!!!Delegate to DRep',
    description: 'Translation for the "delegate to DRep" governance vote type',
  },
  drepInputLabel: {
    id: 'voting.governance.drepInputLabel',
    defaultMessage: '!!!Please type or paste a valid DRep ID here. Look up',
    description: 'Label for DRep input on the governance page',
  },
  drepInputLabelLink: {
    id: 'voting.governance.drepInputLabelLink',
    defaultMessage: '!!!DRep directory',
    description: 'Label link for DRep input on the governance page',
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
  learnMoreLinkLabel: {
    id: 'voting.governance.learnMoreLinkLabel',
    defaultMessage: '!!!Governance link label',
    description: 'Link labels for governance page',
  },
  noConfidence: {
    id: 'voting.governance.noConfidence',
    defaultMessage: '!!!No Confidence',
    description: 'Translation for the "no confidence" governance vote type',
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
});
