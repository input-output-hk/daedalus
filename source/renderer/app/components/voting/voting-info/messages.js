// @flow
import { defineMessages } from 'react-intl';

export const registerToVote = defineMessages({
  name: {
    id: 'voting.registerToVote.name',
    defaultMessage: '!!!Fund{nextVotingFundNumber}',
    description: 'Regiter to fund name',
  },
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

export const currentFund = defineMessages({
  name: {
    id: 'voting.currentFund.name',
    defaultMessage: '!!!Fund{currentVotingFundNumber}',
    description: 'Current fund name',
  },
  headingForEndDate: {
    id: 'voting.currentFund.headingForEndDate',
    defaultMessage: '!!!End of voting:',
    description: 'Headline for end date',
  },
  viewResultsLinkLabel: {
    id: 'voting.currentFund.viewResultsLinkLabel',
    defaultMessage: '!!!View results',
    description: 'View resuls link label for Fund{currentVotingFundNumber}',
  },
  viewResultsLinkURL: {
    id: 'voting.currentFund.viewResultsLinkURL',
    defaultMessage: 'https://cardano.ideascale.com/a/pages/results',
    description: 'View results from Fund{currentVotingFundNumber}',
  },
});

export const headline = defineMessages({
  heading: {
    id: 'voting.catalyst.heading',
    defaultMessage: '!!!Project Catalyst',
    description: 'Headline Project Catalyst',
  },
  descriptionRow1: {
    id: 'voting.catalyst.descriptionRow1',
    defaultMessage:
      '!!!Decide which innovative ideas for Cardano will receive funding.',
    description: 'Description Project Catalyst',
  },
  descriptionRow2: {
    id: 'voting.catalyst.descriptionRow2',
    defaultMessage:
      '!!!{reward} worth of ada rewards will be distributed between ada holders who register their vote.',
    description: 'Description Project Catalyst',
  },
  learnMoreLinkLabel: {
    id: 'voting.info.learnMoreLinkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more link label for registration steps',
  },
  learnMoreLinkUrl: {
    id: 'voting.info.learnMoreLinkUrl',
    defaultMessage: '!!!https://cardano.ideascale.com/a/index',
    description: 'Learn more link url for registration steps',
  },
});

export const appStore = defineMessages({
  androidAppButtonUrl: {
    id: 'voting.info.androidAppButtonUrl',
    defaultMessage:
      '!!!https://play.google.com/store/apps/details?id=io.iohk.vitvoting',
    description: '"androidAppButtonUrl" for the Catalyst voting app',
  },
  appleAppButtonUrl: {
    id: 'voting.info.appleAppButtonUrl',
    defaultMessage:
      '!!!https://apps.apple.com/in/app/catalyst-voting/id1517473397',
    description: '"appleAppButtonUrl" for the Catalyst voting app',
  },
});
