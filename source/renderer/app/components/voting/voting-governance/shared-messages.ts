import { defineMessages } from 'react-intl';

export const sharedGovernanceMessages = defineMessages({
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
  noConfidence: {
    id: 'voting.governance.noConfidence',
    defaultMessage: '!!!No Confidence',
    description: 'Translation for the "no confidence" governance vote type',
  },
});
