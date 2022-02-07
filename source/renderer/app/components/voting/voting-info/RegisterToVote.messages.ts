import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  dateLabel: {
    id: 'voting.registerToVote.dateLabel',
    defaultMessage: '!!!Snapshot date:',
    description: 'Voting info snapshot date label',
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
      '!!!Ensure that you register and hold the necessary 500 ADA at the time of the snapshot.',
    description: 'Second step to follow in order to vote',
  },
  buttonLabel: {
    id: 'voting.registerToVote.registerToVoteButtonLabel',
    defaultMessage: '!!!Register to vote',
    description: 'Button Label for voting registration steps',
  },
});
