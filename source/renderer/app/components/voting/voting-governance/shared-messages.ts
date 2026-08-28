import { defineMessages } from 'react-intl';

export const sharedGovernanceMessages = defineMessages({
  abstain: {
    id: 'voting.governance.abstain',
    defaultMessage: '!!!Abstain',
    description: 'Translation for the "abstain" governance vote type',
  },
  delegateTo: {
    id: 'voting.governance.selectedDRepHeading',
    defaultMessage: '!!!Delegate To',
    description:
      'Names what a wallet delegates its voting power to, on the form and again on the confirmation dialog',
  },
  noConfidence: {
    id: 'voting.governance.noConfidence',
    defaultMessage: '!!!No Confidence',
    description: 'Translation for the "no confidence" governance vote type',
  },
});
