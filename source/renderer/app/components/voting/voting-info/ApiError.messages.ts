import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'voting.apiError.title',
    defaultMessage: '!!!Catalyst API unavailable',
    description: 'Title',
  },
  description1: {
    id: 'voting.apiError.description1',
    defaultMessage:
      '!!!We could not fetch the API to fetch the Catalyst Project dates',
    description: 'Description 1',
  },
  description2: {
    id: 'voting.apiError.description2',
    defaultMessage: '!!!Please, try again later',
    description: 'Description 2',
  },
});
