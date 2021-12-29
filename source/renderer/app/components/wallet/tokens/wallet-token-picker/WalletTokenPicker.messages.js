// @flow
import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'wallet.token.picker.title',
    defaultMessage: '!!!Add tokens',
    description: 'Token picker title',
  },
  allTokensLabel: {
    id: 'wallet.token.picker.allTokensLabel',
    defaultMessage: '!!!All tokens',
    description: 'Label for all tokens option',
  },
  favoriteTokensLabel: {
    id: 'wallet.token.picker.favoriteTokensLabel',
    defaultMessage: '!!!Favorites',
    description: 'Label for favorite tokens option',
  },
  checkMaxFirstLabel: {
    id: 'wallet.token.picker.checkMaxFirstLabel',
    defaultMessage: '!!!Select first {maxTokens}',
    description: 'Label for select first max button label',
  },
  checkAllLabel: {
    id: 'wallet.token.picker.checkAllLabel',
    defaultMessage: '!!!Select all',
    description: 'Label for select all button label',
  },
  checkedCountLabel: {
    id: 'wallet.token.picker.checkedCountLabel',
    defaultMessage: '!!!{checkedCount} out of {maxTokens} tokens.',
    description: 'Label of selected tokens count',
  },
  cancelButtonLabel: {
    id: 'wallet.token.picker.cancelButtonLabel',
    defaultMessage: '!!!Cancel',
    description: 'Label of cancel button',
  },
  addButtonLabel: {
    id: 'wallet.token.picker.addButtonLabel',
    defaultMessage: '!!!Add',
    description: 'Label of add button',
  },
  clearAll: {
    id: 'wallet.token.picker.clearAll',
    defaultMessage: '!!!Clear selection',
    description: 'Label of clear selection button',
  },
  noResults: {
    id: 'wallet.token.picker.noResults',
    defaultMessage: 'No results matching search query',
    description: 'Text for no results',
  },
});
