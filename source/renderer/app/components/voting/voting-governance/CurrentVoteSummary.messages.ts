import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  headerCurrent: {
    id: 'voting.governance.currentVote.headerCurrent',
    defaultMessage: '!!!Current delegation',
    description: 'Header of the current-vote summary panel',
  },
  statusDelegatedToDRep: {
    id: 'voting.governance.currentVote.statusDelegatedToDRep',
    defaultMessage: '!!!Delegated to DRep',
    description:
      'Status label when the wallet delegates its voting power to a DRep',
  },
  statusAbstain: {
    id: 'voting.governance.currentVote.statusAbstain',
    defaultMessage: '!!!Abstain',
    description: 'Status label when the wallet voting power is set to Abstain',
  },
  statusNoConfidence: {
    id: 'voting.governance.currentVote.statusNoConfidence',
    defaultMessage: '!!!No Confidence',
    description:
      'Status label when the wallet voting power is set to No Confidence',
  },
  noDelegationTitle: {
    id: 'voting.governance.currentVote.noDelegation.title',
    defaultMessage: '!!!No governance delegation',
    description: 'Panel title when the wallet has no governance delegation',
  },
  noDelegationWarning: {
    id: 'voting.governance.currentVote.noDelegation.warning',
    defaultMessage:
      "!!!Your staking rewards cannot be withdrawn until you delegate this wallet's voting power to a DRep, Abstain, or No Confidence.",
    description:
      'Reward-withdrawal warning shown when the wallet has no governance delegation',
  },
  noDelegationSubline: {
    id: 'voting.governance.currentVote.noDelegation.subline',
    defaultMessage:
      '!!!Daedalus will not pick a DRep for you — choose how you want your voting power to participate in Cardano governance.',
    description: 'Subline stating Daedalus never auto-delegates voting power',
  },
  noDelegationCta: {
    id: 'voting.governance.currentVote.noDelegation.cta',
    defaultMessage: '!!!Choose a delegation',
    description: 'Call-to-action to choose a governance delegation',
  },
  drepViewDetails: {
    id: 'voting.governance.currentVote.drep.viewDetails',
    defaultMessage: '!!!View details',
    description:
      'In-app link label to the delegated DRep detail view (rendered in a later slice)',
  },
  drepAnchorMetadata: {
    id: 'voting.governance.currentVote.drep.anchorMetadata',
    defaultMessage: '!!!Anchor metadata ↗',
    description:
      'External link label to the delegated DRep anchor metadata (rendered in a later slice)',
  },
  abstainCaption: {
    id: 'voting.governance.currentVote.abstain.caption',
    defaultMessage:
      '!!!Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.',
    description: 'Caption explaining the Abstain delegation state',
  },
  noConfidenceCaption: {
    id: 'voting.governance.currentVote.noConfidence.caption',
    defaultMessage:
      '!!!Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.',
    description: 'Caption explaining the No Confidence delegation state',
  },
});
