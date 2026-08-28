import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  headerCurrent: {
    id: 'voting.governance.currentVote.headerCurrent',
    defaultMessage: '!!!Currently Delegated To',
    description: 'Header of the current-vote summary panel',
  },
  statusUnavailable: {
    id: 'voting.governance.currentVote.status.unavailable',
    defaultMessage: "!!!Could not load this DRep's data. It may have retired.",
    description:
      'Neutral caption shown when the DRep directory has no record for the delegated DRep yet',
  },
});
