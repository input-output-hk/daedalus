import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  headerCurrent: {
    id: 'voting.governance.currentVote.headerCurrent',
    defaultMessage: '!!!Currently Delegated To',
    description: 'Header of the current-vote summary panel',
  },
  headerEnded: {
    id: 'voting.governance.currentVote.headerEnded',
    defaultMessage: '!!!Voting power not delegated',
    description:
      'Heading of the current-delegation panel when the delegation ended',
  },
  endedRetired: {
    id: 'voting.governance.currentVote.endedRetired',
    defaultMessage:
      '!!!This wallet last delegated its voting power to this DRep. The DRep retired, which ended the delegation.',
    description: "Panel and notice text when the wallet's DRep retired",
  },
  endedRetiredRegisteredAgain: {
    id: 'voting.governance.currentVote.endedRetiredRegisteredAgain',
    defaultMessage:
      '!!!This wallet last delegated its voting power to this DRep. The DRep retired, which ended the delegation, and has since registered again. A new registration does not restore earlier delegations.',
    description:
      "Panel and notice text when the wallet's DRep retired and registered again",
  },
  heldRewards: {
    id: 'voting.governance.currentVote.heldRewards',
    defaultMessage:
      '!!!{rewardsAmount} of staking rewards cannot be withdrawn until the voting power is delegated to any DRep, Abstain or No Confidence.',
    description:
      'Added when the wallet holds rewards. {rewardsAmount} is the reward balance with its unit, hidden in discreet mode',
  },
  notInRegistry: {
    id: 'voting.governance.currentVote.notInRegistry',
    defaultMessage: "!!!This DRep is not in the wallet's DRep registry.",
    description:
      "Panel and notice text when the lookup of the wallet's DRep answered not found, on a build that cannot read the ledger's delegation",
  },
  lookupFailed: {
    id: 'voting.governance.currentVote.status.lookupFailed',
    defaultMessage: "!!!Could not load this DRep's data.",
    description: "Panel text when the lookup of the wallet's DRep failed",
  },
});
