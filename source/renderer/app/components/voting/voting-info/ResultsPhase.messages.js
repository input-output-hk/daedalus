// @flow
import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  endDateLabel: {
    id: 'voting.resultsPhase.endDateLabel',
    defaultMessage: '!!!End of voting:',
    description: 'Headline for end date',
  },
  viewResultsLinkLabel: {
    id: 'voting.resultsPhase.viewResultsLinkLabel',
    defaultMessage: '!!!View results',
    description: 'View resuls link label for Fund{currentVotingFundNumber}',
  },
  viewResultsLinkURL: {
    id: 'voting.resultsPhase.viewResultsLinkURL',
    defaultMessage: 'https://cardano.ideascale.com/a/pages/results',
    description: 'View results from Fund{currentVotingFundNumber}',
  },
});
