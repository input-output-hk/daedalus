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
    description: 'View results link label',
  },
  viewResultsLinkURL: {
    id: 'voting.resultsPhase.viewResultsLinkURL',
    defaultMessage: 'https://cardano.ideascale.com/a/pages/results',
    description: 'View results link',
  },
  toBeDefined: {
    id: 'voting.resultsPhase.toBeDefined',
    defaultMessage: '!!!To be defined',
    description:
      'Text to show when catalyst api is returning a past date value',
  },
});
