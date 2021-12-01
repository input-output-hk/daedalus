// @flow
import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  headingForEndDate: {
    id: 'voting.fundResults.headingForEndDate',
    defaultMessage: '!!!End of voting:',
    description: 'Headline for end date',
  },
  viewResultsLinkLabel: {
    id: 'voting.fundResults.viewResultsLinkLabel',
    defaultMessage: '!!!View results',
    description: 'View resuls link label for Fund{currentVotingFundNumber}',
  },
  viewResultsLinkURL: {
    id: 'voting.fundResults.viewResultsLinkURL',
    defaultMessage: 'https://cardano.ideascale.com/a/pages/results',
    description: 'View results from Fund{currentVotingFundNumber}',
  },
});
