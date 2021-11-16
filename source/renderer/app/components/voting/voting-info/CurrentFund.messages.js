// @flow
import { defineMessages } from 'react-intl';

export const messages = defineMessages({
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
