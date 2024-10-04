import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  votingInstructions: {
    id: 'voting.registerToVote.votingInstructions',
    defaultMessage:
      '!!!If you are not registered yet, make sure to register to vote in the current fund before the snapshot date.',
    description: 'Voting instructions',
  },
  stepsTitle: {
    id: 'voting.registerToVote.stepsTitle',
    defaultMessage: '!!!Follow these steps to vote:',
    description: 'Steps to follow title',
  },
  step1CheckBoxLabel: {
    id: 'voting.registerToVote.step1CheckBoxLabel',
    defaultMessage: '!!!Download the Catalyst Voting app on your smartphone',
    description: 'First step to follow in order to vote',
  },
  step2CheckBoxLabel: {
    id: 'voting.registerToVote.step2CheckBoxLabel',
    defaultMessage:
      '!!!Ensure that you register and hold the necessary {minVotingFunds} ADA at the time of the snapshot.',
    description: 'Second step to follow in order to vote',
  },
  buttonLabel: {
    id: 'voting.registerToVote.registerToVoteButtonLabel',
    defaultMessage: '!!!Register to vote',
    description: 'Button Label for voting registration steps',
  },
});
