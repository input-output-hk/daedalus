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
  abstainCaption: {
    id: 'voting.governance.currentVote.abstain.caption',
    defaultMessage:
      '!!!Your stake is recorded on chain as not participating in governance.',
    description: 'Caption explaining the Abstain delegation state',
  },
  noConfidenceCaption: {
    id: 'voting.governance.currentVote.noConfidence.caption',
    defaultMessage:
      '!!!Your stake counts as Yes on every motion of no confidence, and as No on every other governance action.',
    description: 'Caption explaining the No Confidence delegation state',
  },
  statusInactiveSoon: {
    id: 'voting.governance.currentVote.status.inactiveSoon',
    defaultMessage:
      '!!!This DRep becomes inactive in {n, plural, one {# epoch} other {# epochs}}. Consider re-delegating.',
    description:
      'Caption shown when the delegated DRep registration lapses soon',
  },
  statusInactive: {
    id: 'voting.governance.currentVote.status.inactive',
    defaultMessage:
      '!!!This DRep is currently inactive. Your voting power is not counted until they record activity again. Consider re-delegating.',
    description: 'Caption shown when the delegated DRep is inactive',
  },
  statusUnavailable: {
    id: 'voting.governance.currentVote.status.unavailable',
    defaultMessage: '!!!DRep status is loading.',
    description:
      'Neutral caption shown when the DRep directory has no record for the delegated DRep yet',
  },
  sameVoteHint: {
    id: 'voting.governance.currentVote.sameVoteHint',
    defaultMessage:
      '!!!This wallet already votes {target, select, drep {for this DRep} abstain {Abstain} no_confidence {No Confidence} other {the same way}}.',
    description:
      'Hint shown when the chosen delegation is identical to the wallet current on-chain delegation',
  },
});
