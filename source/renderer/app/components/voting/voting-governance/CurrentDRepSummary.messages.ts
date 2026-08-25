import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  headerCurrent: {
    id: 'voting.governance.currentVote.headerCurrent',
    defaultMessage: '!!!Currently Delegated To',
    description: 'Header of the current-vote summary panel',
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
  statusLoading: {
    id: 'voting.governance.currentVote.status.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: "Shown while this wallet's DRep is still being looked up",
  },
  statusUnavailable: {
    id: 'voting.governance.currentVote.status.unavailable',
    defaultMessage: "!!!Could not load this DRep's data. It may have retired.",
    description:
      'Neutral caption shown when the DRep directory has no record for the delegated DRep yet',
  },
});
