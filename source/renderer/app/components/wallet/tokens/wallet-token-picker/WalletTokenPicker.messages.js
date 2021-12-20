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
  check30FirstLabel: {
    id: 'wallet.token.picker.check30FirstLabel',
    defaultMessage: '!!!Select first 30',
    description: 'Label for select first 30 button label',
  },
  checkedCountLabel: {
    id: 'wallet.token.picker.checkedCountLabel',
    defaultMessage: '!!!{checkedCount} of {maxTokens} max tokens',
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
});
